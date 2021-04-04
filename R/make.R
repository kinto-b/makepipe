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
    usethis::ui_info("Targets are out of date. Updating...")
    source(source)
  } else {
    usethis::ui_done("Targets are up to date")
  }

  invisible(NULL)
}


#' Make targets out of dependencies using a recipe
#'
#' @inheritParams make_params
#' @param recipe A chunk of R code which makes the `targets`
#' @param envir The environment in which to run `recipe`
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
make_with_recipe <- function(recipe, targets, dependencies, envir = parent.frame()) {
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
    usethis::ui_info("Targets are out of date. Updating...")
    out <- eval(recipe, envir = envir)
  } else {
    usethis::ui_done("Targets are up to date")
    out <- NULL
  }

  invisible(out)
}

