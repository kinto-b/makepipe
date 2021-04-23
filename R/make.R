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
  outdated <- out_of_date(targets, c(dependencies, source))

  pipeline <- get_pipeline()
  if (is.null(pipeline)) set_pipeline(Pipeline$new())
  pipeline$add_source_segment(
    source = source,
    targets = targets,
    dependencies = dependencies
  )

  if (outdated) {
    cli::cli_process_start(
      "Targets are out of date. Updating...",
      msg_done = "Finished updating",
      msg_failed = "Something went wrong"
    )
    cli::cat_line()
    source(source, ...)
    cli::cli_process_done()
  } else {
    cli::cli_alert_success("Targets are up to date")
  }

  invisible(NULL)
}


#' Make targets out of dependencies using a recipe
#'
#' @inheritParams make_params
#' @param recipe A chunk of R code which makes the `targets`
#' @param envir The environment in which to run `recipe`
#' @param ... Additional parameters to pass to `base::eval()`
#'
#' @return The result of evaluating the `recipe` if the `targets` are out of
#'   date otherwise `NULL``
#' @export
#'
#' @examples
#'
#' \dontrun{
#' # TODO
#' }
#'
make_with_recipe <- function(recipe, targets, dependencies, envir = parent.frame(), ...) {
  outdated <- out_of_date(targets, dependencies)
  recipe_txt <- paste(deparse(substitute(recipe)), collapse = "<br>")

  pipeline <- get_pipeline()
  if (is.null(pipeline)) set_pipeline(Pipeline$new())
  pipeline$add_recipe_segment(
    recipe = recipe_txt,
    targets = targets,
    dependencies = dependencies
  )

  if (outdated) {
    cli::cli_process_start(
      "Targets are out of date. Updating...",
      msg_done = "Finished updating",
      msg_failed = "Something went wrong"
    )
    cli::cat_line()
    out <- eval(recipe, envir = envir, ...)
    cli::cli_process_done()
  } else {
    cli::cli_alert_success("Targets are up to date")
    out <- NULL
  }

  invisible(out)
}

