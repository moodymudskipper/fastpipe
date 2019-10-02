standard_pipe_template <- function(lhs, rhs) {
  # mark the entrance in the pipe
  if(globals$master) {
    sc <- sys.call()
    # flips master switches, run back the call, and sort out the output
    return(eval_slaves(sc, parent.frame()))
  }

  lhs_call <- substitute(lhs)
  rhs_call <- `*LHS_CALL*`

  # initiate f_seq
  if(lhs_call == quote(.)) {
    turbo_pipe <- attr(sys.function(), "bare_version")
    res <- call(turbo_pipe, lhs_call, rhs_call)
    fs_on()
    return(res)
  }
  force(lhs)
  if(globals$is_fs) {
    turbo_pipe <- attr(sys.function(), "bare_version")
    res <- call(turbo_pipe, lhs, rhs_call)
    return(res)
  }

  `*RETURNED_CALL*`
}


fast_pipe_template <- function(lhs, rhs) {
  rhs_call <- `*LHS_CALL*`
  `*RETURNED_CALL*`
}

turbo_pipe_template <- function(lhs, rhs) {
  rhs_call <- substitute(rhs)
  `*RETURNED_CALL*`
}


eval_slaves <- function(sc, env){
  master_off()
  on.exit(reset_globals())
  res <- eval(sc, env)
  if(globals$is_compound) {

    res <- eval(bquote(.(globals$compound_lhs) <- .(res)),env)
    return(invisible(res))
  }
  if(! globals$is_fs)
    return(res)
  res <- as.function(c(alist(.=),res),envir = env)
  return(res)
}

initiate_fseq <- function(rhs_call) {
  res <- list(rhs_call)
  class(res) <- "f_seq"
  res
}

incremment_fseq <- function(lhs, rhs_call) {
  res <- c(lhs, rhs_call)
  class(res) <- "f_seq"
  res
}

insert_dot <- function(expr, special_cases = TRUE) {
  if(is.symbol(expr) || expr[[1]] == quote(`(`)) {
    # if a symbol or an expression inside parentheses, make it a call with dot arg
    expr <- as.call(c(expr, quote(`.`)))
  } else if(length(expr) ==1) {
    # if a call without arg, give it a dot arg
    expr <- as.call(c(expr[[1]], quote(`.`)))
  } else if(special_cases && (
    expr[[1]] == quote(`$`) ||
    expr[[1]] == quote(`::`) ||
    expr[[1]] == quote(`:::`))) {
    # deal with special cases of infix operators
    expr <- as.call(c(expr, quote(`.`)))
  } else if (expr[[1]] != quote(`{`) &&
             all(sapply(expr[-1], `!=`, quote(`.`))) &&
             all(sapply(expr[-1], `!=`, quote(`!!!.`)))) {
    # if a call with args but no dot in arg, insert one first
    expr <- as.call(c(expr[[1]], quote(`.`), as.list(expr[-1])))
  }
  expr
}

build_pipes <- function (root_name, lhs_call, returned_call) {
  # standard pipe
  nm1 <- paste0("%",root_name,">%")
  pipe1 <- standard_pipe_template
  body(pipe1) <- do.call(substitute, list(body(pipe1), list(
    `*LHS_CALL*` = substitute(lhs_call),
    `*RETURNED_CALL*` = substitute(returned_call))))

  # fast pipe
  nm2 <- paste0("%",root_name,">>%")
  pipe2 <- fast_pipe_template
  body(pipe2) <- do.call(substitute, list(body(pipe2), list(
    `*LHS_CALL*` = substitute(lhs_call),
    `*RETURNED_CALL*` = substitute(returned_call))))

  # turbo pipe3
  nm3 <- paste0("%",root_name,">>>%")
  pipe3 <- turbo_pipe_template
  body(pipe3) <- do.call(substitute, list(body(pipe3), list(
    `*RETURNED_CALL*` = substitute(returned_call))))

  # add turbo pipe as attribute of standard pipe, for construction of functional sequence
  attr(pipe1, "bare_version") <- nm3
  assign(nm1, pipe1, envir= parent.frame())
  assign(nm2, pipe2, envir= parent.frame())
  assign(nm3, pipe3, envir= parent.frame())
}





