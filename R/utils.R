
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

#' Deparse
#'
#' Pinched from `flow` :p
#'
#' We might find code that is deparsed into something like foo$!!bar, which is
#' not syntactic if we do, we replace the dollar in `a$b` whenever `b` is not a
#' symbol
#'
#' @param call
#'
#' @return
#' @noRd
#' @keywords internal
#'
#' @examples
#' # Compare
#' robust_deparse(quote(`$`(a, !!b) + `$`(a, b)))
#' deparse(quote(`$`(a, !!b) + `$`(a, b)))
robust_deparse <- function(call) {
  txt <- paste(deparse(call, width.cutoff = 40L, backtick = TRUE), collapse = "\n")
  if (!grepl("\\$!!", txt)) return(txt)
  substitute_bad_dollars <- function(call) {
    if(!is.call(call)) return(call)
    if(length(call) == 3 && identical(call[[1]], quote(`$`))) {
      if(!is.symbol(call[[3]])) {
        call[[1]] <- as.symbol("$\b")
      }
    }
    call <- as.call(lapply(as.list(call), substitute_bad_dollars))
    call
  }
  call <- substitute_bad_dollars(call)
  txt <- paste(deparse(call, width.cutoff = 40L, backtick = TRUE), collapse = "\n")
  gsub("`\\$\\\\b`", "`$`", txt)
}
