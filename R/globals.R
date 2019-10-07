
globals <- new.env()
master_off <- function() globals$master <- FALSE
fs_on  <- function() globals$is_fs <- TRUE
compound_on  <- function() globals$is_compound <- TRUE

reset_globals <- function(){
  globals[["master"]] <- TRUE
  globals[["is_fs"]] <- FALSE
  globals[["is_compound"]] <- FALSE
  globals[["compound_lhs"]] <- NULL
}

set_compound_lhs <- function(lhs){
  globals$compound_lhs <- lhs
}

reset_globals()
