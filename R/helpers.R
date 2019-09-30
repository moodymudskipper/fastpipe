is_pipe <- function(x) inherits(x, "pipe")

get_origin <-function(x){
  ind <- numeric(0)

  repeat {
    lhs <- x[[c(ind, 2)]]
    # did we reach a symbol ?
    if(length(lhs) == 1) {
      # if it's a dot we have a fs, else a standard pipe
      if(lhs == quote(.)) {
        # modify code so it can be executed as a regular pipe
        lhs <- quote((.))
        #if(x[[c(ind, 1)]] == quote(`%<>%`))
        return(list(type="fs", fs = as.function(c(alist(.=),bquote({
          opt <- options(fastpipe.bare = TRUE)
          on.exit(options(fastpipe.bare = opt[[1]]))
          .(x)
        })))))
      }
      return(list(type = "standard"))
    }

    # else go deeper
    ind <- c(ind,2)
    op <- x[[c(ind, 1)]]
    lhs <- x[[c(ind, 2)]]

    # did we reach a non pipe ?
    if(!is_pipe(eval(op))) {
      # if we found the compound pipe, check if we found the dot as well
      if(op  == quote(`%<>%`)){
        # compound AND dot
        if(lhs == quote(.)) {
          lhs <- quote(substitute(.))
          stop("You can't start a functional sequence on a compound operator")
        }

        # compound at the wrong place
        if(length(lhs) !=1 && is_pipe(eval(x[[c(ind,2,1)]])))
          stop("The compound pipe `%<>%` should be used only at the start of the chain",
               call. = FALSE)

        # only compound
        x[[c(ind, 1)]]  <- quote(`%>%`) # replace standard pipe

        return(list(type = "compound", modified_call = bquote(
          .(lhs) <- .(x))))
      }
      return(list(type = "standard"))
    }
  }
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
