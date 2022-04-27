#' Check if targets are out-of-date vis-a-vis their dependencies
#'
#' @inheritParams make_params
#' @return `TRUE` if any of `targets` are older than any of `dependencies` or if
#'   any of `targets` do not exist; `FALSE` otherwise
#' @export
#'
#' @examples
#' \dontrun{
#' out_of_date("data/processed_data.Rds", "data/raw_data.Rds")
#' }
out_of_date <- function(targets, dependencies, packages = NULL) {
  stopifnot_class(targets, "character")
  if (!is.null(dependencies)) stopifnot_class(dependencies, "character")

  outdated <- any(unlist(lapply(targets, function(fp_x) {
    if (!file.exists(fp_x)) return(TRUE)
    unlist(lapply(dependencies, function(fp_y) {
      # Target x older than dependency y?
      if (!file.exists(fp_y)) {
        warning('One or more `dependencies` do not exist: ', fp_y, call. = FALSE)
        return(TRUE)
      }
      file.mtime(fp_x) < file.mtime(fp_y)
    }))
  })))

  if (outdated) return(outdated)

  if (!is.null(packages)) {
    stopifnot_class(packages, "character")
    outdated <- outdated | any(unlist(lapply(targets, function(fp_x) {
      unlist(lapply(packages, function(fp_y) {
        # Target x older than package y?
        file.mtime(fp_x) < package_datetime(packages)
      }))
    })))
  }

  outdated
}
