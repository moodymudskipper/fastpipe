
[![Travis build
status](https://travis-ci.org/moodymudskipper/fastpipe.svg?branch=master)](https://travis-ci.org/moodymudskipper/fastpipe)
[![Codecov test
coverage](https://codecov.io/gh/moodymudskipper/fastpipe/branch/master/graph/badge.svg)](https://codecov.io/gh/moodymudskipper/fastpipe?branch=master)
<!-- badges: end -->

# fastpipe

This package proposes an alternative to the pipe from the *magrittr*
package. It’s named the same and passes all the *magrittr* test so can
easily be a drop-in replacement, its main advantages is that it is
faster and solves most of the issues of *magrittr*.

Install with :

``` r
remotes::install_github("fastpipe")
```

## Issues solved and special features

*fastpipe* passes all *magrittr*’s tests, but it doesn’t stop there and
solves most of its issues, at least most of the open issues on GitHub.

  - Lazy evaluation

<!-- end list -->

``` r
# https://github.com/tidyverse/magrittr/issues/195
# Issue with lazy evaluation #195
gen <- function(x) {function() eval(quote(x))}
identical(
  {fn <- gen(1); fn()},
  {fn <- 1 %>% gen(); fn()})
#> [1] TRUE
```

``` r
# https://github.com/tidyverse/magrittr/issues/159
# Pipes of functions have broken environments
identical(
  {
    compose <- function(f, g) { function(x) g(f(x)) }
    plus1   <- function(x) x + 1
    compose(plus1, plus1)(5)},
  {
    plus2 <- plus1 %>% compose(plus1)
    plus2(5)
})
#> [1] TRUE
# Note : copy and pasted because didn't work with knitr
```

  - It considers `!!!.` as `.` when deciding wether to insert a dot

This was requested by Lionel and seems fairly reasonable as its is
unlikely that a user will nedd to use both `.` and `!!!.` in a call.

``` r
letters[1:3] %>% rlang::list2(!!!.)
#> [[1]]
#> [1] "a"
#> 
#> [[2]]
#> [1] "b"
#> 
#> [[3]]
#> [1] "c"
```

  - `:::`, `::` and `$` get a special treatment to solve a common issue

<!-- end list -->

``` r
iris %>% base::dim
#> [1] 150   5
iris %>% base:::dim
#> [1] 150   5
x <- list(y = dim)
iris %>% x$y
#> [1] 150   5
```

  - It fails explicitly in some cases rather than allowing strange
    behavior silently

<!-- end list -->

``` r
iris %>% head %<>% dim
#> Error in iris %>% head %<>% dim: A compound pipe should only be used at the start of the chain
. %<>% head
#> Error in . %<>% head: You can't start a functional sequence on a compound operator
```

  - The new pipe `%S>%` allows the use of *rlang*’s `!!!` operator to
    splice dots in any function.

<!-- end list -->

``` r
c(a = 1, b = 2) %S>% data.frame(!!!.)
#>   a b
#> 1 1 2
```

  - The new pipe `%L>%` behaves like `%>%` except that it logs to the
    console the calls and the time they took to run.

<!-- end list -->

``` r
1000000 %L>% rnorm() %L>% sapply(cos) %>% max
#> rnorm(.)   ...
#> ~  0.23 sec
#> sapply(., cos)   ...
#> ~  1.96 sec
#> [1] 1
```

## Families of pipes and performance

  - A `%>%` family of pipes reproduce offered by *magrittr* faster and
    more robustly, and extends some features.
  - A `%>>%` family of pipes, offer very fast alternatives, provided the
    dots are provided explicitly on the rhs. It doesn’t support
    functional sequences (`. %>% foo()`) nor compound assignment (`bar
    %<>% baz()`). We call them *bare pipes*.

We provide a benchmark below, the last value is given for context, to
remember that we are discussing small time intervals.

``` r
`%.%` <- fastpipe::`%>%` # (Note that a *fastpipe*, unlike a *magrittr* pipe, can be copied)
`%>%` <- magrittr::`%>%`
bench::mark(check=F,
  'magrittr::`%>%`' =
    1 %>% identity %>% identity() %>% (identity) %>% {identity(.)},
  'fastpipe::`%>%`' =
    1 %.% identity %.% identity() %.% (identity) %.% {identity(.)},
  'fastpipe::`%>>%`' =
    1 %>>% identity(.) %>>% identity(.) %>>% identity(.) %>>% identity(.),
  base = identity(identity(identity(identity(1)))),
  `median(1:3)` = median(1:3)
)
#> # A tibble: 5 x 6
#>   expression            min   median `itr/sec` mem_alloc `gc/sec`
#>   <bch:expr>       <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
#> 1 magrittr::`%>%`   129.4us  302.1us     3035.   88.57KB     4.43
#> 2 fastpipe::`%>%`    59.5us  128.5us     7369.        0B     4.36
#> 3 fastpipe::`%>>%`   11.6us   26.4us    34892.    4.45KB     6.98
#> 4 base                1.6us    3.7us   252085.        0B     0   
#> 5 median(1:3)        29.9us   60.2us    13685.   26.61KB     4.94
rm(`%>%`) # reseting `%>%` to fastpipe::`%>%`
```

We see that our `%>%` pipe is twice faster, and that our `%>>%` pipe is
10 times faster (on a properly formatted call).

We’d like to stress that a pipe is unlikely to be the performance
bottleneck in a script or a function. If optimal performance is
critical, pipes are best avoided as they will always have an overhead.

In other cases `%>>%` is fast and robust to program and keep the main
benefit of piping.

## Implementation

The implementation is completely different from *magrittr*, we can sum
it up as follow :

  - `%>>%` pipes : evaluate *rhs* in parent environment, overriding `.`
    with the value of the *lhs*. Its code is extremely straightforward.

<!-- end list -->

``` r
`%>>%`
#> # a fastpipe object
#> function (lhs, rhs) 
#> {
#>     rhs_call <- substitute(rhs)
#>     eval(rhs_call, envir = list(. = lhs), enclos = parent.frame())
#> }
#> <bytecode: 0x000000001abe7730>
#> <environment: namespace:fastpipe>
```

  - `%>%` pipes :
      - Use heuristics to insert dots, so `x %>% head` becomes `x %>%
        head(.)`
      - support functionnal sequences, so pipe chains starting with `.`
        are functions
      - support compound assignment, so pipe chains starting with `%<>%`
        assign in place to the lhs

To support the two latter, we use global variables, stored in `globals`,
a child environment of our package’s environment. It contains the values
`master`, `is_fs` and `is_compound`. Outside of pipes master is always
`TRUE` while the two latter are always `FALSE`. The alternative to
global parameters was to play with classes and attributes but was slower
and more complex.

Whenever we enter a pipe we check the value of `globals$master`. If it
is `TRUE` we enter a sequence where :

  - We set `master` to `FALSE`, and use `on.exit()` to setup the
    restoration of global parameters to their default value at the end
    of the call.
  - We reexecute the whole call (which now won’t enter this sequence)
      - If no `.` nor `%<>%` are hit in the call recursion we just
        evaluate a sequence of calls as described by `%>>%`’s code,
        expliciting the implicit dots as we go.
      - If the chain starts with `.`
          - We switch on `is_fs`
          - When `is_fs` is switched on we return the original call,
            unevaluated, subtituting the pipe by it’s bare version (the
            name of the bare version is stored as an attribute of the
            pipe) and adding explicit dots (so `. %>% head(2)` becomes
            `. %>>% head(.,2)`).
          - It works recursively and we end up with a quoted pipe chain
            of bare pipes starting with a dot
      - If the chain starts with a `%<>%` call:
          - We switch on `is_compound`
          - We define a global variable named `compound_lhs`, which
            contains the quoted lhs of the `%<>%` call
          - We reevaluate the `%<>%` call, substituting `%<>%` by `%>>%`
          - Then consequent pipe calls are recognized as standard
  - Once we get the result:
      - If `is_fs` was switched on we wrap our quoted pipe in a function
        and return it
      - If `is_compound` was switched on we assign in the calling
        environment the result to the expression stored in
        `global$compoud_lhs`
      - If not any of those we just return the result
  - Default global values are restored, thanks to our `on.exit()` call

*fastpipe* operators contain their own code while in *magrittr*’s
current implementation they all have the main code and are recognized in
the function by their name. Moreover the pipe names are not hardcoded in
the functions, which prevents confusion like the fact that `%$%` still
sometimes work in *magrittr* when only `%>%` is reexported.
<https://github.com/tidyverse/magrittr/issues/194>

The pipes have a class and a printing methods, the functional sequences
don’t.

``` r
`%T>>%`
#> # a fastpipe object
#> function (lhs, rhs) 
#> {
#>     rhs_call <- substitute(rhs)
#>     {
#>         eval(rhs_call, envir = list(. = lhs), enclos = parent.frame())
#>         lhs
#>     }
#> }
#> <bytecode: 0x0000000018751190>
#> <environment: namespace:fastpipe>
. %>% sin %>% cos() %T>% tan(.)
#> function (.) 
#> . %>>% sin(.) %>>% cos(.) %T>>% tan(.)
```

## Benchmarks for functional sequences

An interesting and little known fact is that using functional sequences
in functionals is much more efficient than using lambda functions
calling pipes (though it will generally be largely offset by the content
of the fonction), for instance : `purrr::map(foo, ~ .x %>% bar %>% baz)`
is slower than `purrr::map(foo, . %>% bar %>% baz)`. I tried to keep
this nice feature but didn’t succeed to make it as efficient as in
*magrittr*. The difference show only for large number of iteration and
will probably be negligible in any realistic loop.

``` r
`%.%` <- fastpipe::`%>%`
`%>%` <- magrittr::`%>%`
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# functional sequence with 5 iterations, equivalent speed
library(rlang) # to call `as_function()`
bench::mark(check=F,
  rlang_lambda =
  vapply(1:5, as_function(~.x %>% identity %>% identity() %>% (identity) %>% {identity(.)}), integer(1)),
  fastpipe =
  vapply(1:5, . %.% identity %.% identity() %.% (identity) %.% {identity(.)}, integer(1)),
  magrittr =
  vapply(1:5, . %>% identity %>% identity() %>% (identity) %>% {identity(.)}, integer(1)),
  base = 
  vapply(1:5, function(x) identity(identity(identity(identity(x)))) , integer(1)),
  `median(1:3)` = median(1:3)
)
#> # A tibble: 5 x 6
#>   expression        min   median `itr/sec` mem_alloc `gc/sec`
#>   <bch:expr>   <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
#> 1 rlang_lambda  736.1us   1.41ms      591.    63.2KB     6.90
#> 2 fastpipe      128.7us 285.55us     3218.    32.1KB     6.82
#> 3 magrittr      163.5us  316.3us     2611.    13.8KB     6.76
#> 4 base           12.2us   27.6us    28746.    10.1KB     5.75
#> 5 median(1:3)      26us   59.8us    13693.        0B     4.37

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# functional sequence with 1000 iterations
bench::mark(check=F,
  rlang_lambda =
  vapply(1:1000, as_function(~.x %>% identity %>% identity() %>% (identity) %>% {identity(.)}), integer(1)),
  fastpipe =
  vapply(1:1000, . %.% identity %.% identity() %.% (identity) %.% {identity(.)}, integer(1)),
  magrittr =
  vapply(1:1000, . %>% identity %>% identity() %>% (identity) %>% {identity(.)}, integer(1)),
  base = 
  vapply(1:1000, function(x) identity(identity(identity(identity(x)))) , integer(1)),
  `median(1:3)` = median(1:3)
)
#> Warning: Some expressions had a GC in every iteration; so filtering is
#> disabled.
#> # A tibble: 5 x 6
#>   expression        min   median `itr/sec` mem_alloc `gc/sec`
#>   <bch:expr>   <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
#> 1 rlang_lambda  347.6ms 354.32ms      2.82  277.39KB     5.64
#> 2 fastpipe      16.91ms  30.11ms     30.7     3.95KB     7.67
#> 3 magrittr       7.47ms  15.29ms     61.1     4.23KB     5.92
#> 4 base           2.11ms   3.93ms    188.     14.09KB     7.90
#> 5 median(1:3)      33us   71.3us  12052.          0B     4.00
```

## Note

  - The package is young and might still change.
  - *magrittr*’s pipe is widespread and reexported by prominent
    *tidyverse* packages. It could have been annoying if they masked
    *fastpipe* ’s operator(s) each time so I set a hook so that whenever
    *magrittr*, *dplyr*, *purrr*, or *tidyr* are attached *fastpipe*
    will be detached and reattached at the end of the search path. This
    assumes that if you attach *fastpipe* you want to use its pipes by
    default, which I think is reasonnable. A message makes it explicit,
    for instance :

> Attaching package: ‘dplyr’
> 
> The following object is masked \_by\_ ‘package:fastpipe’:
> 
>     %>%
