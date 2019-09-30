#' Pipes
#'
#' Equivalents to magrittr's pipes, with the addition of a `%S>%` pipe that is
#' just like `%>%` except it supports using `!!!` in any function.
#'
#' @param lhs A value or a dot (`.`).
#' @param rhs A function call using pipe semantics of the relevant pipe.
#' @name pipes
NULL

#' @export
#' @rdname pipes
#' @inheritParams pipes
`%>%` <- function(lhs, rhs) {
  if(!getOption("fastpipe.bare")[[1]]){
  origin <- get_origin(match.call())
  if(origin$type == "fs") return(origin$fs)
  if(origin$type == "compound") return(eval.parent(origin$modified_call))
  }
  rhs <- insert_dot(substitute(rhs))
  eval(rhs, envir = list(`.` = lhs), enclos = parent.frame())
}

#' @export
#' @rdname pipes
#' @inheritParams pipes
#' @export
`%<>%` <- function(lhs, rhs){
  if(substitute(lhs) == quote(.))
    stop("You can't start a functional sequence on a compound operator")
  eval.parent(substitute(lhs <- lhs %>% rhs))
}

#' @export
#' @rdname pipes
#' @inheritParams pipes
`%T>%` <- function(lhs, rhs) {
  rhs <- substitute(rhs)
  rhs <- insert_dot(rhs)
  eval(rhs, envir = list(`.` = lhs), enclos = parent.frame())
  lhs
}

#' @export
#' @rdname pipes
#' @inheritParams pipes
`%$%` <- function(lhs, rhs) {
  rhs <- substitute(with(.,rhs))
  eval(rhs, envir = list(`.` = lhs), enclos = parent.frame())
}

#' @export
#' @rdname pipes
#' @inheritParams pipes
`%S>%` <- function(lhs, rhs) {
  rhs <- substitute(rhs)
  rhs <- insert_dot(rhs)
  # splice
  rhs <- substitute(rlang::expr(rhs),list(rhs=rhs))
  rhs <- eval(rhs, envir = list(`.` = lhs), enclos = parent.frame())
  # eval
  eval(rhs, envir = list(`.` = lhs), enclos = parent.frame())
}

class(`%>%`) <- class(`%T>%`) <- class(`%T>%`) <- class(`%$%`) <- class(`%S>%`) <- "pipe"

