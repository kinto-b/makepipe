
#' Custom errors
#'
#' Throw a nicely formatted error if a given object doesn't have a particular
#' class
#'
#' @param x An object to test
#' @param class A character vector naming classes
#'
#' @return `NULL` invisibly
#' @noRd
#' @keywords internal
#'
#' @examples
#'
#' package_datetime(c("utils", "makepipe"))
stopifnot_class <- function(x, class) {
  if (!inherits(x, class)) {
    stop("`", deparse(substitute(x)), "` must be of class ", class, call. = FALSE)
  }
}

