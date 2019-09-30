################################################################################
# https://github.com/tidyverse/magrittr/issues/200
# compound pipe ignored with functional sequences

test_that("Trying to combine compound operators and functional sequences fails explicitly",{
  expect_error(. %<>% head(3) %>% dim)
  expect_error(. %<>% head(3))
})

################################################################################
# https://github.com/tidyverse/magrittr/issues/198
# Allow logging of calls and process time #198

# we can define an adhoc pipe

`%L>%` <- function(lhs, rhs) {
  origin <- get_origin(match.call())
  if(origin$type == "fs") return(origin$fs)
  if(origin$type == "compound") return(eval.parent(origin$modified_call))
  rhs <- insert_dot(substitute(rhs))
  # print the call
  cat(paste(deparse(rhs), collapse = "\n"), "  ~ ...")
  # measure the time
  time <- system.time(
    res <- eval(rhs, envir = list(`.` = lhs), enclos = parent.frame()))
  cat("\n", time[3], "sec\n")
  res
}

################################################################################
# https://github.com/tidyverse/magrittr/issues/195
# Issue with lazy evaluation #195
test_that("we can't reproduce issue 195",{
  gen <- function(x) {function() eval(quote(x))}
  expect_identical(
    {fn <- gen(1); fn()},
    {fn <- 1 %>% gen(); fn()})
})

################################################################################
# https://github.com/tidyverse/magrittr/issues/193
# Pipe does not do what is supposed to do when the function is given with library name and no parens

test_that(":: etc are handled without ()",{
  expect_identical(
    cars %>% utils::head,
    cars %>% utils::head())
})

################################################################################
# https://github.com/tidyverse/magrittr/issues/191
# Splicing the magrittr input

test_that("`!!!.` is considered as `.` when deciding wether to insert a dot",{
  expect_identical(!!(letters[1:3] %>% rlang::list2(!!!.)), list("a","b","c"))
})


################################################################################
# https://github.com/tidyverse/magrittr/issues/186
# if `.` is modified when using `%T>%`, the main chain is affected #186

test_that("tee pipe can't modify input",{
  expect_identical(mtcars %T>% {. <- iris} %>% head(2), head(mtcars,2))
})

################################################################################
# https://github.com/tidyverse/magrittr/issues/159
# Pipes of functions have broken environments

test_that("we can't reproduce issue 159",{
  expect_identical(
    {
      compose <- function(f, g) { function(x) g(f(x)) }
      plus1   <- function(x) x + 1
      compose(plus1, plus1)(5)},
    {
      plus2 <- plus1 %>% compose(plus1)
      plus2(5)})
})


################################################################################
# https://github.com/tidyverse/magrittr/issues/29
# R CMD check and no visible binding for global variable '.'

# doesn't happen here

