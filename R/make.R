#' Make targets out of dependencies using a source file
#'
#' @inheritParams make_params
#' @param source The path to an R script which makes the `targets`
#' @param ... Additional parameters to pass to `base::source()`
#'
#' @return `NULL` invisibly.
#' @export
#' @family make
#'
#' @examples
#' \dontrun{
#' # Merge files in fresh environment if raw data has been updated since last
#' # merged
#' make_with_source(
#'   source = "merge_data.R",
#'   targets = "data/merged_data.Rds",
#'   dependencies = c("data/raw_data.Rds", "data/raw_pop.Rds")
#' )
#'
#'
#' # Merge files in current environment if raw data has been updated since last
#' # merged. (If source executed, all objects bound in source will be available
#' # in current env).
#' make_with_source(
#'   source = "merge_data.R",
#'   targets = "data/merged_data.Rds",
#'   dependencies = c("data/raw_data.Rds", "data/raw_pop.Rds"),
#'   envir = environment()
#' )
#'
#'
#' # Merge files in global environment if raw data has been updated since last
#' # merged. (If source executed, all objects bound in source will be available
#' # in global env).
#' make_with_source(
#'   source = "merge_data.R",
#'   targets = "data/merged_data.Rds",
#'   dependencies = c("data/raw_data.Rds", "data/raw_pop.Rds"),
#'   envir = globalenv()
#' )
#' }
#'
make_with_source <- function(source, targets, dependencies, packages = NULL,
                             envir = new.env(parent = parent.frame()),
                             quiet = getOption("makepipe.quiet"), ...) {
  stopifnot(is.character(source))
  outdated <- out_of_date(targets, c(dependencies, source), packages = packages)

  pipeline <- get_pipeline()
  if (is.null(pipeline)) {
    pipeline <- Pipeline$new()
    set_pipeline(pipeline)
  }
  pipeline$add_source_segment(
    source = source,
    targets = targets,
    dependencies = dependencies,
    packages = packages
  )

  if (outdated) {
    if (!quiet) {
      cli::cli_process_start(
        "Targets are out of date. Updating...",
        msg_done = "Finished updating",
        msg_failed = "Something went wrong"
      )
      cli::cat_line()
    }
    source(source, local = envir, ...)
    if (!quiet) cli::cli_process_done()
  } else {
    if (!quiet) cli::cli_alert_success("Targets are up to date")
  }

  invisible(NULL)
}


#' Make targets out of dependencies using a recipe
#'
#' @inheritParams make_params
#' @param recipe A chunk of R code which makes the `targets`
#' @param ... Additional parameters to pass to `base::eval()`
#' @return The result of evaluating the `recipe` if the `targets` are out of
#'   date otherwise `NULL``
#' @export
#' @family make
#'
#' @examples
#' \dontrun{
#' # Merge files in fresh environment if raw data has been updated since last
#' # merged
#' make_with_recipe(
#'   recipe = {
#'     dat <- readRDS("data/raw_data.Rds")
#'     pop <- readRDS("data/pop_data.Rds")
#'     merged_dat <- merge(dat, pop, by = "id")
#'     saveRDS(merged_dat, "data/merged_data.Rds")
#'   },
#'   targets = "data/merged_data.Rds",
#'   dependencies = c("data/raw_data.Rds", "data/raw_pop.Rds")
#' )
#'
#' # Merge files in current environment if raw data has been updated since last
#' # merged. (If recipe executed, all objects bound in source will be available
#' # in current env).
#' make_with_recipe(
#'   recipe = {
#'     dat <- readRDS("data/raw_data.Rds")
#'     pop <- readRDS("data/pop_data.Rds")
#'     merged_dat <- merge(dat, pop, by = "id")
#'     saveRDS(merged_dat, "data/merged_data.Rds")
#'   },
#'   targets = "data/merged_data.Rds",
#'   dependencies = c("data/raw_data.Rds", "data/raw_pop.Rds"),
#'   envir = environment()
#' )
#'
#' # Merge files in global environment if raw data has been updated since last
#' # merged. (If source executed, all objects bound in source will be available
#' # in global env).
#' make_with_recipe(
#'   recipe = {
#'     dat <- readRDS("data/raw_data.Rds")
#'     pop <- readRDS("data/pop_data.Rds")
#'     merged_dat <- merge(dat, pop, by = "id")
#'     saveRDS(merged_dat, "data/merged_data.Rds")
#'   },
#'   targets = "data/merged_data.Rds",
#'   dependencies = c("data/raw_data.Rds", "data/raw_pop.Rds"),
#'   envir = globalenv()
#' )
#' }
make_with_recipe <- function(recipe, targets, dependencies, packages = NULL,
                             envir = new.env(parent = parent.frame()),
                             quiet = getOption("makepipe.quiet"), ...) {
  outdated <- out_of_date(targets, c(dependencies), packages = packages)
  recipe <- substitute(recipe)
  recipe_txt <- paste(deparse(recipe), collapse = "<br>")

  pipeline <- get_pipeline()
  if (is.null(pipeline)) {
    pipeline <- Pipeline$new()
    set_pipeline(pipeline)
  }
  pipeline$add_recipe_segment(
    recipe = recipe_txt,
    targets = targets,
    dependencies = dependencies,
    packages = packages
  )

  if (outdated) {
    if (!quiet) {
      cli::cli_process_start(
        "Targets are out of date. Updating...",
        msg_done = "Finished updating",
        msg_failed = "Something went wrong"
      )
      cli::cat_line()
    }
    out <- eval(recipe, envir = envir, ...)
    if (!quiet) cli::cli_process_done()
  } else {
    if (!quiet) cli::cli_alert_success("Targets are up to date")
    out <- NULL
  }

  invisible(out)
}
