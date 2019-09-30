
test_that("pipe works",{
  expect_equal(cars %>% head %>% dim, c(6,2))
  expect_equal(cars %>% head %T>% print %>% dim, c(6,2))
  expect_equal(cars %>% head(2) %$% dist, c(2,10))
  expect_is(. %>% head %>% dim, "function")
  expect_equal("a" %in% letters %>% sum, 1)
  expect_error(cars %>% head %>% dim %<>% force)
  expect_error(cars %>% head %<>% dim %>% force)
  x <- cars
  x %<>% head %>% dim
  expect_equal(x, c(6,2))
  expect_equal(!!quote(c(a=1,b=1) %S>% c(!!!.)), c(a=1,b=1))

  # for coverage
  fastpipe:::.onLoad()
  options( fastpipe.bare = NULL)
  fastpipe:::.onLoad()
})
