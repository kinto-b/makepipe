#' Check if targets are out-of-date vis-a-vis their dependencies
#'
#' @inheritParams make_params
#' @return `TRUE` if any of `targets` are older than any of `dependencies` or if
#'   any of `targets` do not exist; `FALSE` otherwise
#' @export
#'
#' @examples
#' \dontrun{
#' # TODO
#' }
out_of_date <- function (targets, dependencies) {
  stopifnot(is.character(targets))
  stopifnot(is.character(dependencies))

  any(unlist(lapply(targets, function(fp_x) {
    unlist(lapply(dependencies, function (fp_y) {
      # Target x older than dependency y?
      if (!file.exists(fp_y)) usethis::ui_stop("{fp_y} does not exist")
      if (!file.exists(fp_x)) return(TRUE)
      file.mtime(fp_x) < file.mtime(fp_y)
    }))
  })))
}
