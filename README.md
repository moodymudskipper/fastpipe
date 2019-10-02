
<!-- README.md is generated from README.Rmd. Please edit that file -->

# fastpipe

This package proposes an alternative to the pipe from the *magrittr*
package. It’s named the same and passes all the *magrittr* test so can
easily be a drop-in replacement, its main advantages is that it is
faster and solves most of the issues of *magrittr*.

Install with :

``` r
remotes::install_github("fastpipe")
```

## Families of pipes and performance

  - A `%>%` family of pipes reproduce and extends the features offered
    by *magrittr*
  - A `%>>%` family of pipes behaves the same, but doesn’t support
    functional sequences (`. %>% foo()`) nor compound assignment (`bar
    %<>% baz()`)
  - A `%>>>%` family of pipes behaves the same but doesn’t support
    implicit dots

For the 2 latter, skipping these steps has a dramatic effect on
performance. We provide a benchmark below :

``` r
library(fastpipe)
`%.%` <- fastpipe::`%>%` # (Note that a *fastpipe*, unlike a *magrittr* pipe, can be copied)
`%>%` <- magrittr::`%>%`
bench::mark(
  'magrittr::`%>%`' =
    1 %>% identity %>% identity() %>% (identity) %>% {identity(.)},
  'fastpipe::`%>%`' =
    1 %.% identity %.% identity() %.% (identity) %.% {identity(.)},
  'fastpipe::`%>>%`' =
    1 %>>% identity %>>% identity() %>>% (identity) %>>% {identity(.)},
  'fastpipe::`%>>>%`' =
    1 %>>>% identity(.) %>>>% identity(.) %>>>% identity(.) %>>>% identity(.),
  base = identity(identity(identity(identity(1))))
)
#> # A tibble: 5 x 6
#>   expression             min   median `itr/sec` mem_alloc `gc/sec`
#>   <bch:expr>        <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
#> 1 magrittr::`%>%`     91.9us  137.7us     6522.   90.81KB     12.3
#> 2 fastpipe::`%>%`     40.4us   47.7us    19782.   79.12KB     12.4
#> 3 fastpipe::`%>>%`    13.8us     19us    49262.    4.92KB     19.7
#> 4 fastpipe::`%>>>%`    8.9us   10.9us    85078.    4.45KB     25.5
#> 5 base                   1us    1.3us   497347.        0B      0
rm(`%>%`) # reseting `%>%` to fastpipe::`%>%`
```

We see that our standard `%>%` pipe is twice faster, and that our
`%>>>%` pipe is 10 times faster (on a properly formatted call).

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
#> ~  0.3 sec
#> sapply(., cos)   ...
#> ~  2.57 sec
#> [1] 1
```

## Implementation

The implementation is completely different from *magrittr*, we can sum
it up as follow :

  - `%>>>%` pipes : evaluate *rhs* in parent environment, overriding `.`
    with the value of the *lhs*. Its code is extremely straightforward

<!-- end list -->

``` r
`%>>>%`
#> # a fastpipe object
#> function (lhs, rhs) 
#> {
#>     rhs_call <- substitute(rhs)
#>     eval(rhs_call, envir = list(. = lhs), enclos = parent.frame())
#> }
#> <bytecode: 0x0000000013733118>
#> <environment: namespace:fastpipe>
```

  - `%>>%` pipes : same as above but use heuristics to insert dots, so
    `x %>% head` becomes `x %>% head(.)` etc

<!-- end list -->

``` r
`%>>%`
#> # a fastpipe object
#> function (lhs, rhs) 
#> {
#>     rhs_call <- insert_dot(substitute(rhs))
#>     eval(rhs_call, envir = list(. = lhs), enclos = parent.frame())
#> }
#> <bytecode: 0x00000000136e23b8>
#> <environment: namespace:fastpipe>
```

  - `%>%` pipes : same as above but must support functionnal sequences
    and compound assignment, we do so by modifying global variables when
    we find a relevant start to the pipe chain and reseting them when
    going out.

*fastpipe* operators contain their own code while in *magrittr*’s
current implementation they all have the main code and are recognized in
the function by their name. Moreover the pipe names are not hardcoded in
the functions, which prevents confusion like the fast that `%$%` still
sometimes work in *magrittr* when only `%<%` is reexported.
<https://github.com/tidyverse/magrittr/issues/194>

The pipes have a class, the functional sequences don’t, they are regular
functions using the `%>>>%` family of *fastpipes*.

``` r
`%T>>%`
#> # a fastpipe object
#> function (lhs, rhs) 
#> {
#>     rhs_call <- insert_dot(substitute(rhs))
#>     {
#>         eval(rhs_call, envir = list(. = lhs), enclos = parent.frame())
#>         lhs
#>     }
#> }
#> <bytecode: 0x00000000136ac920>
#> <environment: namespace:fastpipe>
. %>% sin %>% cos() %T>% tan(.)
#> function (.) 
#> . %>>>% sin(.) %>>>% cos(.) %T>>>% tan(.)
```

## Benchmarks for functional sequences

An interesting and little known fact is that using functional sequences
in functionals is much more efficient than using lambda functions
calling pipes, for instance : `purrr::map(foo, ~ .x %>% bar %>% baz)`
will often be much slower than `purrr::map(foo, . %>% bar %>% baz)`. I
tried to keep this nice feature but didn’t succeed to make it as
efficient as in *magrittr*. The difference show only for large number of
iteration and will probably be negligible in any realistic loop.

``` r
`%>%` <- magrittr::`%>%`
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# functional sequence with 5 iterations
walk <- purrr::walk
bench::mark(
  purrr_lambda =
  walk(1:5, ~.x %>% identity %>% identity() %>% (identity) %>% {identity(.)}),
  magrittr =
  walk(1:5, . %>% identity %>% identity() %>% (identity) %>% {identity(.)}),
  fastpipe =
  walk(1:5, . %.% identity %.% identity() %.% (identity) %.% {identity(.)})
)
#> # A tibble: 3 x 6
#>   expression        min   median `itr/sec` mem_alloc `gc/sec`
#>   <bch:expr>   <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
#> 1 purrr_lambda    615us    743us     1265.    77.3KB     17.8
#> 2 magrittr        153us    188us     5046.    26.8KB     14.0
#> 3 fastpipe        126us    153us     6177.    19.1KB     16.2

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# functional sequence with 1000 iterations
bench::mark(
  purrr_lambda =
    walk(1:1000, ~.x %>% identity %>% identity() %>% (identity) %>% {identity(.)}),
  magrittr =
    walk(1:1000, . %>% identity %>% identity() %>% (identity) %>% {identity(.)}),
  fastpipe =
    walk(1:1000, . %.% identity %.% identity() %.% (identity) %.% {identity(.)})
)
#> Warning: Some expressions had a GC in every iteration; so filtering is
#> disabled.
#> # A tibble: 3 x 6
#>   expression        min   median `itr/sec` mem_alloc `gc/sec`
#>   <bch:expr>   <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
#> 1 purrr_lambda 168.91ms 188.69ms      5.44   281.3KB     14.5
#> 2 magrittr       5.37ms   6.68ms    123.      8.13KB     13.9
#> 3 fastpipe      10.98ms  13.97ms     60.8     7.86KB     17.6
```

## A few notes

This is an experiment, and might still change change.

The current package might be well suited for programming because it’s
robust, fast, and very light.
