
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# simple call
`%.%` <- fastpipe::`%>%`
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

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# simple functional sequence calls

bench::mark(iterations = 10000,
  magrittr =
  (. %>% identity %>% identity() %>% (identity) %>% {identity(.)})(1),
  fastpipe =
  (. %.% identity %.% identity() %.% (identity) %.% {identity(.)})(1)
)


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
