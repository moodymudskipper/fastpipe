



#
# `%>0%` <- function(x,y) {
#   # capture rhs and replace dots by `*.*`
#   rhs <- substitute(y)
#   rhs <- do.call(substitute, list(rhs, list(. = quote(`*.*`))))
#   # replace back dots protected by formulas from `*.*` to `.`
#   rhs <- call2list2(rhs)
#
#   if(is.symbol(rhs) || rhs[[1]] == quote(`(`)) {
#     # if a symbol or an expression inside parentheses, make it a call with dot arg
#     rhs <- as.call(c(rhs, quote(`*.*`)))
#     } else if(length(rhs) ==1) {
#       # if a call without arg, give it a dot arg
#     rhs <- as.call(c(rhs[[1]], quote(`*.*`)))
#   } else if (
#     rhs[[1]] != quote(`{`) &&
#     all(sapply(rhs[-1], `!=`, quote(`*.*`)))) {
#     # if a call with args but no dot in arg, insert one first
#     rhs <- as.call(c(rhs[[1]], quote(`*.*`), as.list(rhs[-1])))
#   }
#
#   eval(rhs, envir = list(`*.*` = x), enclos = parent.frame())
# }
#
# call2list2 <- function(call){
#   # if symbol or if "*.*" is not in the call, just return it
#   if(length(call) == 1 || !"*.*" %in% all.names(call))
#     call
#   else {
#     # else substitute the symbol back to `.` in `~` and `function`
#     if(call[[1]] == quote(`~`) || call[[1]] == quote(`function`)){
#       do.call(substitute, list(call, list(`*.*` = quote(.))))
#     } else {
#       as.call(lapply(call, call2list2))
#     }
#   }
# }
#

# starts_with_dot <- function(lhs, pipe) {
#   if (!is_pipe(eval(lhs[[1]]))) return(FALSE)
#   if (lhs[[2]] == quote(.)) return(TRUE)
#   if (length(lhs[[2]]) == 1) return(FALSE)
#   starts_with_dot(lhs[[2]], pipe)
# }
#


# options(pipe.fs = TRUE)
#


# profvis::profvis(
#   x<-replicate(1000, cars %>% identity %>% identity() %>% (identity) %>% {identity(.)},simplify = F)
#
# )


# build_functional_seq <- function(expr, env) {
#   fun <- as.function(c(alist(.=), bquote({
#     opt <- options(fastpipe.bare = TRUE)
#     on.exit(options(fastpipe.bare = opt))
#     .(expr)
#   })))
#   environment(fun) <- env
#   fun
# }
