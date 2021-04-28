#' Pipeline visualisations
#'
#' @description A Pipeline object is automatically constructed as calls to `make_*()` are
#' made. It stores the relationships between targets, dependencies, and
#' sources.
#'
#' @keywords internal
#' @family pipeline
#' @export Pipeline
#' @aliases Pipeline
#' @importFrom R6 R6Class
Pipeline <- R6::R6Class(classname = "Pipeline", list(
  #' @field edges A data frame
  edges = data.frame(
    from = character(0),
    to = character(0),
    arrows = character(0),
    .source = logical(0),
    .recipe = logical(0),
    stringsAsFactors = FALSE
  ),

  #' @field nodes A data frame
  nodes = data.frame(
    id = integer(0),
    label = character(0),
    title = character(0),
    shape = character(0),
    colour = character(0)
  ),

  #' @description Add a pipeline segment corresponding to a `make_with_source()`
  #'   call
  #' @param source The path to an R script which makes the `targets`
  #' @param targets A character vector of paths to files
  #' @param dependencies A character vector of paths to files which the
  #'   `targets` depend on
  #' @return `self`
  add_source_segment = function(source, targets, dependencies) {
    stopifnot(is.character(dependencies))
    stopifnot(is.character(source))
    stopifnot(is.character(targets))

    # Construct new edges
    new_edges <- rbind(
      expand.grid(from = dependencies, to = source, arrows = "to", .source = TRUE, .recipe = FALSE, stringsAsFactors = FALSE),
      expand.grid(from = source, to = targets, arrows = "to", .source = FALSE, .recipe = FALSE, stringsAsFactors = FALSE)
    )

    # Convert old edges to character and bind
    self$edges$from <- as.character(self$edges$from)
    self$edges$to <- as.character(self$edges$to)
    self$edges <- rbind(self$edges, new_edges)

    # Regenerate nodes using edges
    self$nodes <- data.frame(
      id = factor(unique(c(self$edges$from, self$edges$to))),
      title = unique(c(self$edges$from, self$edges$to))
    )
    self$style_nodes()

    # Convert edges back to factor
    self$edges$from <- factor(self$edges$from, levels = levels(self$nodes$id))
    self$edges$to <- factor(self$edges$to, levels = levels(self$nodes$id))


    invisible(self)
  },

  #' @description Add a pipeline segment corresponding to a `make_with_recipe()`
  #'   call
  #' @param recipe A character vector containing a deparsed expression, which would make the `targets` if evaluated.
  #' @param targets A character vector of paths to files
  #' @param dependencies A character vector of paths to files which the `targets` depend on
  #' @return `self`
  add_recipe_segment = function(recipe, targets, dependencies) {
    stopifnot(is.character(dependencies))
    stopifnot(is.character(recipe))
    stopifnot(is.character(targets))

    # Construct new edges
    new_edges <- rbind(
      expand.grid(from = dependencies, to = recipe, arrows = "to", .source = TRUE, .recipe = TRUE, stringsAsFactors = FALSE),
      expand.grid(from = recipe, to = targets, arrows = "to", .source = FALSE, .recipe = FALSE, stringsAsFactors = FALSE)
    )

    # Convert old edges to character and bind
    self$edges$from <- as.character(self$edges$from)
    self$edges$to <- as.character(self$edges$to)
    self$edges <- rbind(self$edges, new_edges)

    # Regenerate nodes using edges
    self$nodes <- data.frame(
      id = factor(unique(c(self$edges$from, self$edges$to))),
      title = unique(c(self$edges$from, self$edges$to))
    )
    self$style_nodes()

    # Convert edges back to factor
    self$edges$from <- factor(self$edges$from, levels = levels(self$nodes$id))
    self$edges$to <- factor(self$edges$to, levels = levels(self$nodes$id))


    invisible(self)
  },

  #' @description Style pipeline nodes
  #' @return `self`
  style_nodes = function() {
    nodes <- self$nodes
    edges <- self$edges

    # Out of date?
    edges$from_mtime <- file.mtime(as.character(edges$from))
    edges$to_mtime <- file.mtime(as.character(edges$to))
    edges$out_of_date <- edges$from_mtime > edges$to_mtime
    edges$out_of_date <- ifelse(is.na(edges$out_of_date), FALSE, edges$out_of_date)
    for (i in seq_along(edges$from)) {
      x <- edges$out_of_date
      edges$out_of_date <- (edges$from %in% edges[edges$out_of_date, "to"]) | (edges$out_of_date)
      if (identical(edges$out_of_date, x)) break
    }

    # Group
    nodes$group <- ifelse(nodes$id %in% edges[edges$out_of_date, "to"], "Out-of-date", "Up-to-date")
    nodes$group <- ifelse(nodes$id %in% edges[edges$.source, "to"], "Source", nodes$group)

    # Colour
    nodes$shape <- ifelse(nodes$id %in% edges[edges$.source, "to"], "triangle", "square")
    nodes$shape <- ifelse(nodes$id %in% edges[edges$.recipe, "to"], "circle", "square")

    # Label
    nodes$label <- ifelse(nodes$id %in% edges[edges$.recipe, "to"], "Recipe", basename(as.character(nodes$id)))

    self$nodes <- nodes
    invisible(self)
  },

  #' @description Display pipeline
  #' @return `self`
  print = function() {
    out <- visNetwork::visNetwork(self$nodes, self$edges)
    out <- visNetwork::visGroups(out, groupname = "Out-of-date", color = "#ffcaef")
    out <- visNetwork::visGroups(out, groupname = "Up-to-date", color = "#caffda")
    out <- visNetwork::visLegend(out)
    print(out)
    invisible(self)
  },

  #' @description Save pipeline
  #' @param file File to save HTML into
  #' @param ... Additional arguments to pass to `visNetwork::visSave()`
  #' @return `self`
  save = function(file, ...) {
    out <- visNetwork::visNetwork(self$nodes, self$edges)
    out <- visNetwork::visGroups(out, groupname = "Out-of-date", color = "#ffcaef")
    out <- visNetwork::visGroups(out, groupname = "Up-to-date", color = "#caffda")
    out <- visNetwork::visLegend(out)
    visNetwork::visSave(out, file, ...)
    invisible(self)
  }
))

# -------------------------------------------------------------------------

#' Access and interface with Pipeline.
#'
#' `get_pipeline()`, `set_pipeline()` access and modify the current *active*
#' pipeline, while all other helper functions do not affect the active pipeline
#'
#'
#' @param pipeline A pipeline. See [Pipeline] for more details.
#' @name pipeline-accessors
#' @family pipeline
#' @examples
#'
#' \dontrun{
#'  # Build up a pipeline from scratch and save it out
#'  set_pipeline(Pipeline$new())
#'  # A series of `make_with_*()` blocks go here...
#'  saveRDS(get_pipeline(), "data/my_pipeline.Rds")
#' }
NULL
piper_env <- new.env(parent = emptyenv())

#' @rdname pipeline-accessors
#' @export
is_pipeline <- function(pipeline) {
  inherits(pipeline, c("Pipeline", "R6"))
}

#' @rdname pipeline-accessors
#' @export
set_pipeline <- function(pipeline) {
  if (!is_pipeline(pipeline)) stop("`pipeline` must be a Pipeline object", call. = FALSE)
  old <- piper_env$pipeline
  piper_env$pipeline <- pipeline
  invisible(old)
}

#' @rdname pipeline-accessors
#' @export
get_pipeline <- function() {
  piper_env$pipeline
}


#' Visualise the Pipeline.
#'
#' Produce an HTML flowchart visualisation of the pipeline.
#'
#' Tooltips and labels must be supplied as named character vector where the
#' names correspond to the filepaths of nodes (i.e. `targets`, `dependencies`,
#' or `source` scripts)
#'
#' @param file File to save HTML into
#' @param pipeline A pipeline. See [Pipeline] for more details.
#' @param tooltips A named character vector mapping nodes in the `pipeline` onto
#'   tooltips to display on hover-over.
#' @param labels A named character vector mapping nodes in the `pipeline` onto
#'   labels to display beside them.
#' @param ... Additional arguments to pass to `visNetwork::visSave()`.
#' @name pipeline-vis
#' @family pipeline
#' @examples
#'
#' \dontrun{
#'  # Run pipeline
#'  make_with_source(
#'    "recode.R",
#'    "data/0 raw_data.R",
#'    "data/1 data.R"
#'  )
#'  make_with_source(
#'    "merge.R",
#'    c("data/1 data.R", "data/0 raw_pop.R"),
#'    "data/2 data.R"
#'  )
#'
#'  # Visualise pipeline with custom tooltips
#'  show_pipeline(tooltips = c(
#'    "data/0 raw_data.R" = "Raw survey data",
#'    "data/0 raw_pop.R" = "Raw population data",
#'    "data/1 data.R" = "Survey data with recodes applied",
#'    "data/2 data.R" = "Survey data with demographic variables merged in"
#'  ))
#'
#' }
NULL
#' @rdname pipeline-vis
#' @export
show_pipeline <- function(pipeline = get_pipeline(), tooltips = NULL, labels = NULL) {
  pipeline <- annotate_pipeline(pipeline, tooltips, labels)
  pipeline$print()
}

#' @rdname pipeline-vis
#' @export
save_pipeline <- function(file, pipeline = get_pipeline(), tooltips = NULL, labels = NULL, ...) {
  pipeline <- annotate_pipeline(pipeline, tooltips, labels)
  pipeline$save(file, ...)
}

#' @noRd
annotate_pipeline <- function(pipeline, tooltips, labels) {
  if (!is_pipeline(pipeline)) stop("`pipeline` must be a Pipeline object", call. = FALSE)

  if (!is.null(tooltips)) {
    stopifnot(is.character(tooltips))
    if (!identical(length(names(tooltips)), length(tooltips))) stop("`tooltips` must be named", call. = FALSE)
    bad_nodes <- setdiff(names(tooltips), as.character(pipeline$nodes$id))
    if (length(bad_nodes) > 0) stop("`", paste(bad_nodes, collapse = "`, "), "` are not nodes in `pipeline`", call. = FALSE)
    if (any(duplicated(names(tooltips)))) stop("names of `tooltips` must not be duplicated")

    pipeline <- pipeline$clone(deep = TRUE)
    old_nodes <- pipeline$nodes
    old_nodes$node_id <- as.character(old_nodes$id)
    tooltips <- data.frame(node_id = names(tooltips), new_title = tooltips)
    new_nodes <- merge(old_nodes, tooltips, by = "node_id", all.x = TRUE)
    new_nodes$title <- ifelse(is.na(new_nodes$new_title), new_nodes$title, new_nodes$new_title)
    new_nodes <- new_nodes[, setdiff(names(new_nodes), c("node_id", "new_title"))]
    pipeline$nodes <- new_nodes
  }

  if (!is.null(labels)) {
    stopifnot(is.character(labels))
    if (!identical(length(names(labels)), length(labels))) stop("`labels` must be named", call. = FALSE)
    bad_nodes <- setdiff(names(labels), as.character(pipeline$nodes$id))
    if (length(bad_nodes) > 0) stop("`", paste(bad_nodes, collapse = "`, "), "` are not nodes in `pipeline`", call. = FALSE)
    if (any(duplicated(names(labels)))) stop("names of `labels` must not be duplicated")

    pipeline <- pipeline$clone(deep = TRUE)
    old_nodes <- pipeline$nodes
    old_nodes$node_id <- as.character(old_nodes$id)
    labels <- data.frame(node_id = names(labels), new_label = labels)
    new_nodes <- merge(old_nodes, labels, by = "node_id", all.x = TRUE)
    new_nodes$label <- ifelse(is.na(new_nodes$new_label), new_nodes$label, new_nodes$new_label)
    new_nodes <- new_nodes[, setdiff(names(new_nodes), c("node_id", "new_label"))]
    pipeline$nodes <- new_nodes
  }

  pipeline
}
