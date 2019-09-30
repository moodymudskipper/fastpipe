
<!-- README.md is generated from README.Rmd. Please edit that file -->

# fastpipe

This package proposes an alternative to the pipe from ‘magrittr’. It’s
named the same and passes all the ‘magrittr’ test so can easily be a
drop-in replacement, its main advantages is that it is faster and solves
most of the issues of *magrittr*

Install with :

``` r
remotes::install_github("fastpipe")
```

## What’s different from *magrittr* ?

The main differences are:

  - It features an option `fastpipe.bare`, which is `FALSE` by default
    but when set to `TRUE` turns off the features of compound assignment
    (`%<>%`) and of functional sequences (`. %>% foo`)

  - It is faster, especially whith `options(fastpipe.bare = TRUE)`

<!-- end list -->

``` r
microbenchmark::microbenchmark(
  magrittr = {`%>%` <- magrittr::`%>%`
  1 %>% identity %>% identity() %>% (identity) %>% {identity(.)}},
  fastpipe = {`%>%` <- fastpipe::`%>%`
  1 %>% identity %>% identity() %>% (identity) %>% {identity(.)}},
  fastpipe_bare = {
    options(fastpipe.bare = TRUE)
    `%>%` <- fastpipe::`%>%`
  1 %>% identity %>% identity() %>% (identity) %>% {identity(.)}
    options(fastpipe.bare = FALSE)},
  times=10000
)
#> Unit: microseconds
#>           expr  min    lq      mean median     uq    max neval cld
#>       magrittr 92.8 129.2 272.96893  195.3 416.35 5666.8 10000   c
#>       fastpipe 74.0 102.1 212.82853  152.8 308.30 7056.8 10000  b 
#>  fastpipe_bare 21.2  31.9  67.03829   47.6 102.20 4572.3 10000 a
```

  - Its implementation is much simpler

If we ignore the features of functional chains and compound pipe, the
code is really just :

  - modify the rhs to insert explicit dot at the right place if relevant
  - gives the dot the value of the lhs and evaluate it in the parent
    environment

<!-- end list -->

``` r
function(lhs, rhs) {
  if(!getOption("fastpipe.bare")[[1]]){
  origin <- get_origin(match.call())
  if(origin$type == "fs") return(origin$fs)
  if(origin$type == "compound") return(eval.parent(origin$modified_call))
  }
  rhs <- insert_dot(substitute(rhs))
  eval(rhs, envir = list(`.` = lhs), enclos = parent.frame())
}
#> function(lhs, rhs) {
#>   if(!getOption("fastpipe.bare")[[1]]){
#>   origin <- get_origin(match.call())
#>   if(origin$type == "fs") return(origin$fs)
#>   if(origin$type == "compound") return(eval.parent(origin$modified_call))
#>   }
#>   rhs <- insert_dot(substitute(rhs))
#>   eval(rhs, envir = list(`.` = lhs), enclos = parent.frame())
#> }
```

Moreover pipe operators contain their own code while in *magrittr*’s
current implementation they all have the main code and are recognized in
the function by their name. Which leads to the next point.

  - We can define new pipes

It’s not made as easy as in my *pipe* package, but nonetheless we can
define new pipes, which is forbidden by *magrittr* which allows only a
given list of symbols.

To do it we can copy `%>%` and tweak it. A pipe should have a class
`pipe` to make functional chain and compound operator work.

  - It is more robust

It passes every test from magrittr, and deals with issues of lazy
evaluation that were problematic without any adhoc adjustment.

``` r
# https://github.com/tidyverse/magrittr/issues/195
# Issue with lazy evaluation #195
gen <- function(x) {function() eval(quote(x))}
identical(
  {fn <- gen(1); fn()},
  {fn <- 1 %>% gen(); fn()})
#> [1] TRUE

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
```

  - It considers `!!!.` is considered as `.` when deciding wether to
    insert a dot

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

  - The new pipe `%S>%` allows the use of *rlang*’s `!!!` operator to
    splice dots in any function.

<!-- end list -->

``` r
library(pipe)
#> 
#> Attaching package: 'pipe'
#> The following object is masked _by_ '.GlobalEnv':
#> 
#>     %>%
c(a = 1, b = 2) %S>% data.frame(!!!.)
#>   a b
#> 1 1 2
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
#> Error: A compound pipe should only be used at the start of the chain
. %<>% head
#> Error in . %<>% head: You can't start a functional sequence on a compound operator
```

## A few notes

This is just an experiment, it’s likely to change.

I’ve also been working on the package *pipe* which focuses on
acompletely generalized approach of the pipe (while in this package the
`%<>%` operatorneeds special treatment) and makes it easier to define
new pipes, and to browse a pipe chain step by step.

The drawbacks of the latter system is that it is slower (a little bit
slower than *magrittr*), and it suffers from the same lazy evaluation
issues as magrittr does.

The current package might be better suited for programming because it’s
robust, fast, and very light, while *pipe* might be better for
interractive use where the lazy evaluation corner cases are unlikely to
appear and where efficiently is less important.
