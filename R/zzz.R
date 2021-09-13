.onLoad <- function(libname, pkgname) {
  op <- options()
  op.makepipe <- list(
    makepipe.quiet = FALSE
  )
  toset <- !(names(op.makepipe) %in% names(op))
  if (any(toset)) options(op.makepipe[toset])

  invisible()
}
