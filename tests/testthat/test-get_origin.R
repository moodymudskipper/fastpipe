test_that("we detect the origin properly", {
  # standard pipe chain => F F
  expect_equal(get_origin(quote(baz %>% bar %>% foo))$type,
               "standard")
  # pipe chain preceded by other calls => FF
  expect_equal(get_origin(quote(. %in% baz %in% bar %>% foo))$type,
               "standard")

  # regular functional chain => T F
  expect_equal(get_origin(quote(. %>% bar %>% foo))$type,
               "fs")

  # regular compound call => F T
  expect_equal(get_origin(quote(baz %<>% bar %>% foo))$type,
               "compound")

  # compound call and functional chain
  expect_error(get_origin(quote(. %<>% bar %>% foo))$type,
               "^You can't start")

  # compound call found in wrong place => fail
  expect_error(get_origin(quote(qux %>% baz %<>% bar %>% foo)),
               "^The compound pipe")

})
