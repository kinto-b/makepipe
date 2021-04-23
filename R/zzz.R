.onLoad <- function(libname, pkgname) {
  op <- options()
  op.piper <- list(
    piper.quiet = FALSE
  )
  toset <- !(names(op.piper) %in% names(op))
  if (any(toset)) options(op.piper[toset])

  invisible()
}
