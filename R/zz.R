.onLoad <- function(libname, pkgname) {
  op <- options()
  op.fastpipe <- list(
    fastpipe.bare = FALSE,
    fastpipe.desc.author = "Antoine Fabri <antoine.fabri@gmail.com> [aut, cre]",
    fastpipe.desc.license = "GPL-3"
  )
  toset <- !(names(op.fastpipe) %in% names(op))
  if(any(toset)) options(op.fastpipe[toset])

  invisible()
}
