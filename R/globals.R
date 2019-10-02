
globals <- new.env()
#globals$master <- TRUE
#master_on  <- function() globals$master <- TRUE
master_off <- function() globals$master <- FALSE

#globals$is_fs <- FALSE
fs_on  <- function() globals$is_fs <- TRUE
#fs_off <- function() globals$is_fs <- FALSE

#globals$is_compound <- FALSE
compound_on  <- function() globals$is_compound <- TRUE
#compound_off <- function() globals$is_compound <- FALSE

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
