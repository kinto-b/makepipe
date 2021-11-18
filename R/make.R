#' Make targets out of dependencies using a source file
#'
#' @inheritParams make_params
#' @param source The path to an R script which makes the `targets`
#' @param ... Additional parameters to pass to `base::source()`
#'
#' @return A `Segment` object containing execution metadata.
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
  targets <- unique(targets)
  dependencies <- unique(dependencies)
  packages <- unique(packages)
  if (any(targets %in% source)) {
    stop("`source` must not be among the `targets`", call. = FALSE)
  }
  if (any(targets %in% dependencies)) {
    stop("`dependencies` must not be among the `targets`", call. = FALSE)
  }

  outdated <- out_of_date(targets, c(dependencies, source), packages = packages)

  # Update pipeline
  pipeline <- get_pipeline()
  if (is.null(pipeline)) {
    pipeline <- Pipeline$new()
    set_pipeline(pipeline)
  }
  segment <- pipeline$add_source_segment(source, targets, dependencies, packages, envir)

  # Prepare register
  register_env <- new.env(parent = emptyenv())
  assign("__makepipe_register__", register_env, envir)

  # Execute
  if (outdated) {
    if (!quiet) {
      cli::cli_process_start(
        "Targets are out of date. Updating...",
        msg_done = "Finished updating",
        msg_failed = "Something went wrong"
      )
      cli::cat_line()
    }

    execution_time <- Sys.time()
    source(source, local = envir, ...)
    execution_time <- Sys.time() - execution_time

    if (!quiet) cli::cli_process_done()
  } else {
    execution_time <- NULL
    if (!quiet) cli::cli_alert_success("Targets are up to date")
  }

  out <- segment$update_result(outdated, execution_time, register_env)

  invisible(out)
}


#' Make targets out of dependencies using a recipe
#'
#' @inheritParams make_params
#' @param recipe A chunk of R code which makes the `targets`
#' @param ... Additional parameters to pass to `base::eval()`
#' @return A `Segment` object containing execution metadata.
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
  targets <- unique(targets)
  dependencies <- unique(dependencies)
  packages <- unique(packages)
  recipe <- rlang::enexpr(recipe)

  if (any(targets %in% dependencies)) {
    stop("`dependencies` must not be among the `targets`", call. = FALSE)
  }

  .make_with_recipe(recipe, targets, dependencies, packages, envir, quiet, ...)
}

#' @noRd
.make_with_recipe <- function(recipe, targets, dependencies, packages,
                              envir, quiet, ...) {
  outdated <- out_of_date(targets, c(dependencies), packages = packages)

  pipeline <- get_pipeline()
  if (is.null(pipeline)) {
    pipeline <- Pipeline$new()
    set_pipeline(pipeline)
  }
  segment <- pipeline$add_recipe_segment(recipe, targets, dependencies, packages, envir)

  if (outdated) {
    if (!quiet) {
      cli::cli_process_start(
        "Targets are out of date. Updating...",
        msg_done = "Finished updating",
        msg_failed = "Something went wrong"
      )
      cli::cat_line()
    }

    execution_time <- Sys.time()
    out <- eval(recipe, envir = envir, ...)
    execution_time <- Sys.time() - execution_time

    if (!quiet) cli::cli_process_done()
  } else {
    execution_time <- NULL
    if (!quiet) cli::cli_alert_success("Targets are up to date")
    out <- NULL
  }

  out <- segment$update_result(outdated, execution_time, out)

  invisible(out)
}
