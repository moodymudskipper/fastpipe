
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
  expect_equal(!!quote(c(a=1,b=1) %S>>% c(!!!.)), c(a=1,b=1))

  expect_equal(cars %>% head, cars %L>% head)
  expect_equal(`%>%`, print(`%>%`))

  build_pipes(
    "test",
    rhs_call =
      insert_dot(substitute(rhs)),
    returned_call =
      eval(rhs_call, envir = list(`.` = lhs), enclos = parent.frame())
  )

  # for coverage
  trace(bare_pipe_template, quote(`*RETURNED_CALL*` <- 1))
  bare_pipe_template(1,1)
  trace(standard_pipe_template,expression({
    `*RHS_CALL*` <- quote(.); `*RETURNED_CALL*` <- 1}))
  attr(standard_pipe_template, "bare_version") <- "bare_pipe_template"
  standard_pipe_template(.,force)
  standard_pipe_template(1,force)
  fs_on()
  standard_pipe_template(1,force)
  untrace(standard_pipe_template)
  untrace(bare_pipe_template)

})
