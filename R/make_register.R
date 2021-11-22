#' Register objects to be returned from `make_with_source`
#'
#' It is sometimes useful to have access to certain objects which are generated as
#' side-products in a source script which yields as a main-product one or more
#' targets. Typically these objects are used for checking that the targets were
#' produced as expected.
#'
#' @param value A value to be registered in a source script and returned as part
#'   of the `Segment`
#' @param name A variable name, given as a character string. No coercion is
#'   done, and the first element of a character vector of length greater than
#'   one will be used, with a warning.
#' @param quiet A logical determining whether or not warnings are signaled when
#'   `make_register()` is called outside of a 'makepipe' pipeline
#'
#' @return `value` invisibly
#' @export
#'
#' @examples
#'
#' \dontrun{
#'   # Imagine this is part of your source script:
#'   x <- readRDS("input.Rds")
#'   x <- do_stuff(x)
#'   chk <- do_check(x)
#'   make_register(chk, "x_check")
#'   saveRDS(x, "output.Rds")
#'
#'   # You will have access to `chk` in your pipeline script:
#'   step_one <- make_with_source(
#'     "source.R",
#'     "output.Rds",
#'     "input.Rds",
#'   )
#'   step_one$result$chk
#' }
make_register <- function(value, name, quiet = FALSE) {
  msg <- "`make_register()` called outside of a 'makepipe' pipeline"
  if (exists("__makepipe_register__", parent.frame())) {
    register_env <- get("__makepipe_register__", parent.frame())
    if (!is.environment(register_env) & !quiet) warning(msg, call. = FALSE)
  } else {
    if (!quiet) warning(msg, call. = FALSE)
    return(invisible(value))
  }

  assign(name, value, register_env)
  invisible(value)
}
