.onLoad <- function(libname, pkgname){
  setHook(packageEvent("magrittr", "attach"),
          function(...) fastpipe_first())
  setHook(packageEvent("dplyr", "attach"),
          function(...) fastpipe_first())
  setHook(packageEvent("purrr", "attach"),
          function(...) fastpipe_first())
  setHook(packageEvent("tidyr", "attach"),
          function(...) fastpipe_first())
}

fastpipe_first <- function(){
  detach("package:fastpipe")
  library(fastpipe, warn.conflicts = FALSE, quietly = TRUE )
}
