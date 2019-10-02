# The way we collapse the functions is not robust!!! we do like there are only standard pipes

#' Pipes
#'
#' Equivalents to magrittr's pipes, plus some additions :
#' * The `%S>%` pipe is like `%>%` except it supports using `!!!` in any function.
#' * The `%L>%` pipe is like `%>%` except it logs to the console the call and the execution time.
#' * The `%\*>>%` family of pipes contains equivalent that go faster because they
#'   don't support functional chains (`. %>% foo() %>% bar()` nor the compound pipe (`%<>%`).
#'
#' @param lhs A value or a dot (`.`).
#' @param rhs A function call using pipe semantics of the relevant pipe.
#' @name pipes
NULL

#' @export
#' @rdname pipes
#' @inheritParams pipes
#' @export
`%<>%` <- function(lhs, rhs){
  if(substitute(lhs) == quote(.))
    stop("You can't start a functional sequence on a compound operator")

  # check also if we have a pipe further left
  # ...
  lhs_call <- substitute(lhs)
  if(length(lhs_call) == 3 && is_fastpipe(eval(lhs_call[[1]])))
    stop("A compound pipe should only be used at the start of the chain")

  # if it's the main pipe
  if(globals$master)
    return(invisible(eval.parent(substitute(lhs <- lhs %>% rhs))))

  res <- eval.parent(substitute(LHS %>>% RHS, list(
    LHS = lhs_call, RHS = insert_dot(substitute(rhs)))))
  compound_on()
  set_compound_lhs(lhs_call)
  res
}

# %>%
build_pipes(
  "",
  rhs_call =
    insert_dot(substitute(rhs)),
  returned_call =
    eval(rhs_call, envir = list(`.` = lhs), enclos = parent.frame())
  )

# %L>%
build_pipes(
  "L",
  rhs_call =
    insert_dot(substitute(rhs)),
  returned_call = {
    cat(paste(deparse(rhs_call), collapse = "\n"), "  ...\n")
    cat("~ ", system.time(
      res <- eval(rhs_call, envir = list(`.` = lhs), enclos = parent.frame()))[3],
      "sec\n")
    res
  }
)

# %T>%
build_pipes(
  "T",
  rhs_call =
    insert_dot(substitute(rhs)),
  returned_call =
    {
      eval(rhs_call, envir = list(`.` = lhs), enclos = parent.frame())
      lhs
    }
)

# %$>%
build_pipes(
  "$",
  rhs_call =
    substitute(rhs),
  returned_call =
    eval(bquote(with(.,.(rhs_call))), envir = list(`.` = lhs), enclos = parent.frame())
)

# %S>%
build_pipes(
  "S",
  rhs_call =
    insert_dot(substitute(rhs)),
  returned_call =
    {
      # splice
      rhs_call <- substitute(rlang::expr(rhs),list(rhs=rhs_call))
      rhs_call <- eval(rhs_call, envir = list(`.` = lhs), enclos = parent.frame())
      # eval
      eval(rhs_call, envir = list(`.` = lhs), enclos = parent.frame())
    }
)

#' @export
#' @rdname pipes
#' @inheritParams pipes
`%>%` <- `%>%`

#' @export
#' @rdname pipes
#' @inheritParams pipes
`%>>%` <- `%>>%`

#' @export
#' @rdname pipes
#' @inheritParams pipes
`%T>%` <- `%T>%`

#' @export
#' @rdname pipes
#' @inheritParams pipes
`%T>>%` <- `%T>>%`

#' @export
#' @rdname pipes
#' @inheritParams pipes
`%$%` <- `%$>%`

#' @export
#' @rdname pipes
#' @inheritParams pipes
`%$>>%` <- `%$>>%`

#' @export
#' @rdname pipes
#' @inheritParams pipes
`%S>%` <- `%S>%`

#' @export
#' @rdname pipes
#' @inheritParams pipes
`%S>>%` <- `%S>>%`

#' @export
#' @rdname pipes
#' @inheritParams pipes
`%L>%` <- `%L>%`

#' @export
#' @rdname pipes
#' @inheritParams pipes
`%L>>%` <- `%L>>%`



class(`%>%`) <- class(`%T>%`) <- class(`%$%`) <- class(`%S>%`) <- class(`%L>%`) <-
  class(`%>>%`) <- class(`%T>>%`)  <- class(`%$>>%`) <- class(`%S>>%`) <- class(`%L>>%`) <-
  "fastpipe"

#' Print a fastpipe object
#'
#' @param x object to print
#' @param ... Ignored, kept for compatibility with other methods
#' @export
print.fastpipe <- function(x, ...){
  cat("# a fastpipe object\n")

  print(`attributes<-`(x, NULL))
  bv <- attr(x,"bare_version")
  if(!is.null(bv))
    cat("# Bare version: `", bv, "`", sep="")
  invisible(x)
}


#' Test if Object is a fastpipe
#'
#' @param x object to test
#'
#' @return a length one logical
#' @export
is_fastpipe <- function(x) {
  inherits(x, "fastpipe")
}

