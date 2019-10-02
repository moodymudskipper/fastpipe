standard_pipe_template <- function(lhs, rhs) {
  # mark the entrance in the pipe
  if(globals$master) {
    sc <- sys.call()
    # flips master switches, run back the call, and sort out the output
    return(eval_slaves(sc, parent.frame()))
  }

  lhs_call <- substitute(lhs)
  rhs_call <- `*RHS_CALL*`

  # initiate f_seq
  if(lhs_call == quote(.)) {
    bare_pipe <- attr(sys.function(), "bare_version")
    res <- call(bare_pipe, lhs_call, rhs_call)
    fs_on()
    return(res)
  }
  force(lhs)
  if(globals$is_fs) {
    bare_pipe <- attr(sys.function(), "bare_version")
    res <- call(bare_pipe, lhs, rhs_call)
    return(res)
  }

  `*RETURNED_CALL*`
}

bare_pipe_template <- function(lhs, rhs) {
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

build_pipes <- function (root_name, rhs_call, returned_call) {
  # standard pipe
  standard_pipe_nm <- paste0("%",root_name,">%")
  standard_pipe <- standard_pipe_template
  body(standard_pipe) <- do.call(substitute, list(body(standard_pipe), list(
    `*RHS_CALL*` = substitute(rhs_call),
    `*RETURNED_CALL*` = substitute(returned_call))))

  # bare pipe
  bare_pipe_nm <- paste0("%",root_name,">>%")
  bare_pipe <- bare_pipe_template
  body(bare_pipe) <- do.call(substitute, list(body(bare_pipe), list(
    `*RETURNED_CALL*` = substitute(returned_call))))

  # add bare pipe as attribute of standard pipe, for construction of functional sequence
  attr(standard_pipe, "bare_version") <- bare_pipe_nm
  assign(standard_pipe_nm, standard_pipe, envir= parent.frame())
  assign(bare_pipe_nm, bare_pipe, envir= parent.frame())
}
