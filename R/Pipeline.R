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
    for (i in nrow(edges)) {
      edges$out_of_date <- (edges$from %in% edges[edges$out_of_date, "to"]) | (edges$out_of_date)
    }
    edges$out_of_date <- ifelse(edges$.recipe, FALSE, edges$out_of_date)

    # Group
    nodes$group <- ifelse(nodes$id %in% edges[edges$out_of_date, "to"], "Out-of-date", "Up-to-date")
    nodes$group <- ifelse(nodes$id %in% edges[edges$.source, "to"], "Source", nodes$group)

    # Colour
    nodes$shape <- ifelse(nodes$id %in% edges[edges$.source, "to"], "triangle", "square")
    nodes$shape <- ifelse(nodes$id %in% edges[edges$.recipe, "to"], "circle", "square")

    # Label
    nodes$label <- ifelse(nodes$id %in% edges[edges$.recipe, "to"], "Recipe", basename(nodes$title))

    self$nodes <- nodes
    invisible(self)
  },

  #' @description Display pipeline
  #' @return `self`
  print = function() {
    out <- visNetwork::visNetwork(self$nodes, self$edges)
    out <- visNetwork::visGroups(out, groupname = "Out-of-date", color = "#ffcaef")
    out <- visNetwork::visGroups(out, groupname = "Up-to-date", color = "#caffda")
    visNetwork::visLegend(out)
  }
))

# -------------------------------------------------------------------------

#' Access and interface with Pipeline.
#'
#' `get_pipeline()`, `set_pipeline()` access and modify the current *active*
#' pipeline, while `show_pipeline()` and `is_pipeline()` are used to display
#' or test a given pipeline.
#'
#' @param pipeline A pipeline. See [Pipeline] for more details.
#' @name pipeline-accessors
#' @family pipeline
#' @examples
#'
#' \dontrun{
#'  # Build up a pipeline from scratch:
#'  set_pipeline(Pipeline$new())
#'  # A series of `make_with_*()` blocks go here...
#'  saveRDS(get_pipeline(), "data/my_pipeline.Rds")
#'  show_pipeline()
#' }
NULL
piper_env <- new.env(parent = emptyenv())

#' @rdname pipeline-accessors
#' @export
set_pipeline <- function(pipeline) {
  if (!is_pipeline(pipeline)) stop("`pipeline` must be a Pipeline object")
  old <- piper_env$pipeline
  piper_env$pipeline <- pipeline
  invisible(old)
}

#' @rdname pipeline-accessors
#' @export
get_pipeline <- function() {
  piper_env$pipeline
}

#' @rdname pipeline-accessors
#' @export
show_pipeline <- function(pipeline = get_pipeline()) {
  if (!is_pipeline(pipeline)) stop("`pipeline` must be a Pipeline object")
  pipeline$print()
}

#' @rdname pipeline-accessors
#' @export
is_pipeline <- function(pipeline) {
  inherits(pipeline, c("Pipeline", "R6"))
}
