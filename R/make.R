#' Make targets out of dependencies using a source file
#'
#' @inheritParams make_params
#' @param source The path to an R script which makes the `targets`
#' @param ... Additional parameters to pass to `base::source()`
#'
#' @return `NULL` invisibly.
#' @export
#'
#' @examples
#'
#' \dontrun{
#' # TODO
#' }
#'
make_with_source <- function(source, targets, dependencies, ...) {
  stopifnot(is.character(source))

  pipeline <- get_pipeline()
  if (is.null(pipeline)) set_pipeline(Pipeline$new())
  pipeline$pipeline_network(
    dependencies = dependencies,
    targets = targets,
    source = source
  )

  if (out_of_date(targets, c(dependencies, source))) {
    usethis::ui_info("Targets are out of date. Updating...")
    source(source)
  } else {
    usethis::ui_done("Targets are up to date")
  }

  invisible(NULL)
}
