# Source -----------------------------------------------------------------------

#' Make targets out of dependencies using a source file
#'
#' @inheritParams make_params
#' @param note A description of what the `source` does, displayed in pipeline
#'   visualisations
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
make_with_source <- function(source, targets, dependencies = NULL, packages = NULL,
                             envir = new.env(parent = parent.frame()),
                             quiet = getOption("makepipe.quiet"),
                             force = FALSE,
                             label = NULL,
                             note = NULL,
                             build = TRUE,
                             ...) {
  pipeline <- get_pipeline()
  if (is.null(pipeline)) {
    pipeline <- Pipeline$new()
    set_pipeline(pipeline)
  }
  segment <- pipeline$add_source_segment(source, targets, dependencies, packages, envir, force)

  if (build) {
    segment$execute(quiet = quiet)
  }

  add_note_and_label(pipeline, segment, label, note)
  invisible(segment)
}




# Recipe -----------------------------------------------------------------------


#' Make targets out of dependencies using a recipe
#'
#' @inheritParams make_params
#' @param note A description of what the `recipe` does, displayed in pipeline
#'   visualisations. If `NULL`, the `recipe` code is used.
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
make_with_recipe <- function(recipe, targets, dependencies = NULL, packages = NULL,
                             envir = new.env(parent = parent.frame()),
                             quiet = getOption("makepipe.quiet"),
                             force = FALSE,
                             label = NULL,
                             note = NULL,
                             build = TRUE,
                             ...) {
  recipe <- substitute(recipe)
  pipeline <- get_pipeline()
  if (is.null(pipeline)) {
    pipeline <- Pipeline$new()
    set_pipeline(pipeline)
  }
  segment <- pipeline$add_recipe_segment(recipe, targets, dependencies, packages, envir, force)

  if (build) {
    segment$execute(quiet = quiet)
  }

  add_note_and_label(pipeline, segment, label, note)
  invisible(segment)
}


# Internal ----------------------------------------------------------------

add_note_and_label <- function(pipeline, segment, label, note) {
  node_id <- as.character(segment$nodes[segment$nodes$.source, ]$id)
  if (!is.null(label)) names(label) <- node_id
  if (!is.null(note)) names(note) <- node_id
  pipeline$annotate(labels = label, notes = note)
  invisible(NULL)
}
