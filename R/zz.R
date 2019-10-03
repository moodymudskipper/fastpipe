.onLoad <- function(libname, pkgname){
  pkgs <- c(
    "magrittr","dplyr","purrr", "tidyr", "stringr", "forcats", "rvest",
    "modelr", "testthat")
  for(pkg in pkgs){
  setHook(packageEvent(pkg, "attach"),
          function(...) fastpipe_first())
  }
}

fastpipe_first <- function(){
  detach("package:fastpipe")
  library(fastpipe, warn.conflicts = FALSE, quietly = TRUE )
}
