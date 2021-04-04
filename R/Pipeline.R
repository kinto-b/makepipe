#' Pipeline visualisations
#'
#' A Pipeline object is automatically constructed as calls to `make_*()` are
#' made. It stores the relationships between targets, dependencies, and
#' sources.
#'
#' @keywords internal
#' @export Pipeline
#' @aliases Pipeline
#' @importFrom R6 R6Class
#' @examples
#' \dontrun{
#' # TODO
#' }
Pipeline <- R6::R6Class(classname = "Pipeline", list(
  #' @field edges A data frame
  edges = data.frame(
    from = character(0),
    to = character(0),
    arrows = character(0),
    .source = logical(0),
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

  #' @param targets A character vector of paths to files
  #' @param dependencies A character vector of paths to files which the `targets` depend on
  #' @param source The path to an R script which makes the `targets`
  #' @return `self`
  pipeline_network = function(dependencies, source, targets) {
    stopifnot(is.character(dependencies))
    stopifnot(is.character(source))
    stopifnot(is.character(targets))

    # Construct new edges
    new_edges <- rbind(
      expand.grid(from = dependencies, to = source, arrows = "to", .source = TRUE, stringsAsFactors = FALSE),
      expand.grid(from = source, to = targets, arrows = "to", .source = FALSE, stringsAsFactors = FALSE)
    )

    # Convert old edges to character and bind
    self$edges$from <- as.character(self$edges$from)
    self$edges$to <- as.character(self$edges$to)
    self$edges <- rbind(self$edges, new_edges)

    # Regenerate nodes using edges
    self$nodes <- data.frame(
      id = factor(unique(c(self$edges$from, self$edges$to))),
      label = fs::path_file(unique(c(self$edges$from, self$edges$to))),
      title = unique(c(self$edges$from, self$edges$to)),
      shape = ifelse(grepl("\\.R$", unique(c(self$edges$from, self$edges$to))), "triangle", "square")
    )

    self$refresh_nodes()

    # Convert edges back to factor
    self$edges$from <- factor(self$edges$from, levels = levels(self$nodes$id))
    self$edges$to <- factor(self$edges$to, levels = levels(self$nodes$id))


    invisible(self)
  },

  #' @return `self`
  refresh_nodes = function() {
    nodes <- self$nodes
    edges <- self$edges
    edges$from_mtime <- file.mtime(as.character(edges$from))
    edges$to_mtime <- file.mtime(as.character(edges$to))
    edges$out_of_date <- edges$from_mtime > edges$to_mtime
    for (i in nrow(edges)) {
      edges$out_of_date <- (edges$from %in% edges[edges$out_of_date, "to"]) | (edges$out_of_date)
    }
    nodes$group <- ifelse(nodes$id %in% edges[edges$out_of_date, "to"], "Out-of-date", "Up-to-date")
    nodes$group <- ifelse(nodes$id %in% edges[edges$.source, "to"], "Source", nodes$group)

    self$nodes <- nodes
    invisible(self)
  },

  #' @return `self`
  print = function() {
    out <- visNetwork::visNetwork(self$nodes, self$edges)
    out <- visNetwork::visGroups(out, groupname = "Out-of-date", color = "#ffcaef")
    out <- visNetwork::visGroups(out, groupname = "Up-to-date", color = "#caffda")
    visNetwork::visLegend(out)
  }
))

# -------------------------------------------------------------------------

#' Access Pipeline.
#'
#' `get_pipeline()` and `set_pipeline()` access and modify the current "active"
#' pipeline.
#'
#' @param pipeline A pipeline. See [Pipeline] for more details.
#' @keywords internal
#' @name pipeline-accessors
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