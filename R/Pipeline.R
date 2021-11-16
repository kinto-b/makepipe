
# R6 Object --------------------------------------------------------------------

#' Pipeline visualisations
#'
#' @description A Pipeline object is automatically constructed as calls to
#'   `make_*()` are made. It stores the relationships between targets,
#'   dependencies, and sources.
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
    .pkg = logical(0),
    .outdated = logical(0),
    stringsAsFactors = FALSE
  ),

  #' @field nodes A data frame
  nodes = data.frame(
    id = integer(0),
    label = character(0),
    title = character(0),
    shape = character(0),
    colour = character(0),
    .source = logical(0),
    .recipe = logical(0),
    .pkg = logical(0),
    stringsAsFactors = FALSE
  ),

  #' @description Add a pipeline segment corresponding to a `make_with_source()`
  #'   call
  #' @param source The path to an R script which makes the `targets`
  #' @param targets A character vector of paths to files
  #' @param dependencies A character vector of paths to files which the
  #'   `targets` depend on
  #' @param packages A character vector of names of packages which `targets`
  #'   depend on
  #' @return `self`
  add_source_segment = function(source, targets, dependencies, packages) {
    stopifnot(is.character(dependencies))
    stopifnot(is.character(source))
    stopifnot(is.character(targets))
    stopifnot(is.character(packages) | is.null(packages))

    # Construct new edges
    new_edges <- rbind(
      expand.grid(
        from = dependencies,
        to = source,
        arrows = "to",
        .source = TRUE,
        .recipe = FALSE,
        .pkg = FALSE,
        .outdated = FALSE,
        stringsAsFactors = FALSE
      ),
      expand.grid(
        from = source,
        to = targets,
        arrows = "to",
        .source = FALSE,
        .recipe = FALSE,
        .pkg = FALSE,
        .outdated = FALSE,
        stringsAsFactors = FALSE
      )
    )


    if (length(packages) > 0) {
      new_edges <- rbind(
        expand.grid(
          from = packages,
          to = source,
          arrows = "to",
          .source = TRUE,
          .recipe = FALSE,
          .pkg = TRUE,
          .outdated = FALSE,
          stringsAsFactors = FALSE
        ),
        new_edges
      )
    }

    # Convert old edges to character and bind
    self$edges$from <- as.character(self$edges$from)
    self$edges$to <- as.character(self$edges$to)
    self$edges <- rbind(self$edges, new_edges)

    # Regenerate nodes using edges
    self$nodes <- data.frame(
      id = factor(unique(c(self$edges$from, self$edges$to))),
      title = unique(c(self$edges$from, self$edges$to))
    )

    self$nodes$.source <- self$nodes$id %in% self$edges[self$edges$.source, "to"]
    self$nodes$.recipe <- self$nodes$id %in% self$edges[self$edges$.recipe, "to"]
    self$nodes$.pkg <- self$nodes$id %in% self$edges[self$edges$.pkg, "from"]
    self$style_nodes()

    # Convert edges back to factor
    self$edges$from <- factor(self$edges$from, levels = levels(self$nodes$id))
    self$edges$to <- factor(self$edges$to, levels = levels(self$nodes$id))

    self$out_of_date()

    invisible(self)
  },

  #' @description Add a pipeline segment corresponding to a `make_with_recipe()`
  #'   call
  #' @param recipe A character vector containing a deparsed expression, which
  #'   would make the `targets` if evaluated.
  #' @param targets A character vector of paths to files
  #' @param dependencies A character vector of paths to files which the
  #'   `targets` depend on
  #' @param packages A character vector of names of packages which `targets`
  #'   depend on
  #' @return `self`
  add_recipe_segment = function(recipe, targets, dependencies, packages) {
    stopifnot(is.character(dependencies))
    stopifnot(is.character(recipe))
    stopifnot(is.character(targets))
    stopifnot(is.character(packages) | is.null(packages))

    # Construct new edges
    new_edges <- rbind(
      expand.grid(
        from = dependencies,
        to = recipe,
        arrows = "to",
        .source = TRUE,
        .recipe = TRUE,
        .pkg = FALSE,
        .outdated = FALSE,
        stringsAsFactors = FALSE
      ),
      expand.grid(
        from = recipe,
        to = targets,
        arrows = "to",
        .source = FALSE,
        .recipe = FALSE,
        .pkg = FALSE,
        .outdated = FALSE,
        stringsAsFactors = FALSE
      )
    )

    if (length(packages) > 0) {
      new_edges <- rbind(
        expand.grid(
          from = packages,
          to = recipe,
          arrows = "to",
          .source = TRUE,
          .recipe = FALSE,
          .pkg = TRUE,
          .outdated = FALSE,
          stringsAsFactors = FALSE
        ),
        new_edges
      )
    }

    # Convert old edges to character and bind
    self$edges$from <- as.character(self$edges$from)
    self$edges$to <- as.character(self$edges$to)
    self$edges <- rbind(self$edges, new_edges)

    # Regenerate nodes using edges
    self$nodes <- data.frame(
      id = factor(unique(c(self$edges$from, self$edges$to))),
      title = unique(c(self$edges$from, self$edges$to))
    )

    self$nodes$.source <- self$nodes$id %in% self$edges[self$edges$.source, "to"]
    self$nodes$.recipe <- self$nodes$id %in% self$edges[self$edges$.recipe, "to"]
    self$nodes$.pkg <- self$nodes$id %in% self$edges[self$edges$.pkg, "from"]
    self$style_nodes()

    # Convert edges back to factor
    self$edges$from <- factor(self$edges$from, levels = levels(self$nodes$id))
    self$edges$to <- factor(self$edges$to, levels = levels(self$nodes$id))
    self$out_of_date()

    invisible(self)
  },

  #' @description Style pipeline nodes
  #' @return `self`
  out_of_date = function() {
    edges <- self$edges

    # Out of date?
    edges$from_mtime <- file.mtime(as.character(edges$from))
    edges$to_mtime <- file.mtime(as.character(edges$to))
    edges$.outdated <- ifelse(
      edges$.source,
      FALSE,
      edges$from_mtime > edges$to_mtime
    )

    # Propagate out-of-dateness
    for (i in seq_along(edges$to)) {
      if (edges$.source[i]) next
      edges$.outdated[i] <- propagate_outofdateness(edges$to[i], edges)
    }

    edges <- edges[, c("from", "to", "arrows", ".source", ".recipe", ".pkg", ".outdated")]
    self$edges <- edges
    invisible(self)
  },

  #' @description Style pipeline nodes
  #' @return `self`
  style_nodes = function() {
    nodes <- self$nodes
    edges <- self$edges

    # Update out-of-dateness
    self$out_of_date()

    # Group
    nodes$group <- ifelse(
      nodes$id %in% edges[edges$.outdated, "to"], "Out-of-date", "Up-to-date"
    )
    nodes$group <- ifelse(nodes$.source, "Source", nodes$group)

    # Aesthetics
    nodes$shape <- ifelse(nodes$.recipe, "circle", "square")
    nodes$shape <- ifelse(nodes$.pkg, "triangle", nodes$shape)

    # Label
    if (is.null(nodes$label)) {
      lbl <- basename(as.character(nodes$id))
      nodes$label <- ifelse(nodes$.recipe, "Recipe", lbl)
    }

    self$nodes <- nodes
    invisible(self)
  },

  #' @description Display pipeline
  #' @param ...  Arguments (other than `nodes` and `edges`) to pass to
  #'   `visNetwork::visNetwork()`
  #' @return `self`
  print = function(...) {
    self$style_nodes()
    out <- pipeline_network(nodes = self$nodes, edges = self$edges, ...)
    print(out)
    invisible(self)
  },

  #' @description Save pipeline
  #' @param file File to save HTML into
  #' @param selfcontained Whether to save the HTML as a single self-contained
  #'   file (with external resources base64 encoded) or a file with external
  #'   resources placed in an adjacent directory.
  #' @param background Text string giving the html background color of the
  #'   widget. Defaults to white.
  #' @param ...  Arguments (other than `nodes` and `edges`) to pass to
  #'   `visNetwork::visNetwork()`
  #' @return `self`
  save = function(file, selfcontained = TRUE, background = "white", ...) {
    self$style_nodes()
    out <- pipeline_network(nodes = self$nodes, edges = self$edges, ...)
    visNetwork::visSave(out, file, selfcontained, background)
    invisible(self)
  }
))

# Accessors --------------------------------------------------------------------

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
#' \dontrun{
#' # Build up a pipeline from scratch and save it out
#' set_pipeline(Pipeline$new())
#' # A series of `make_with_*()` blocks go here...
#' saveRDS(get_pipeline(), "data/my_pipeline.Rds")
#' }
NULL
makepipe_env <- new.env(parent = emptyenv())

#' @rdname pipeline-accessors
#' @export
is_pipeline <- function(pipeline) {
  inherits(pipeline, c("Pipeline", "R6"))
}

#' @rdname pipeline-accessors
#' @export
set_pipeline <- function(pipeline) {
  if (!is_pipeline(pipeline)) {
    stop("`pipeline` must be a Pipeline object", call. = FALSE)
  }

  old <- makepipe_env$pipeline
  makepipe_env$pipeline <- pipeline
  invisible(old)
}

#' @rdname pipeline-accessors
#' @export
get_pipeline <- function() {
  pipe <- makepipe_env$pipeline
  if (!is.null(pipe)) pipe$style_nodes() # Refresh
  pipe
}


# Visualisors ------------------------------------------------------------------

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
#' @param selfcontained Whether to save the HTML as a single self-contained
#'   file (with external resources base64 encoded) or a file with external
#'   resources placed in an adjacent directory.
#' @param background Text string giving the html background color of the widget.
#'   Defaults to white.
#' @param ...  Arguments (other than `nodes` and `edges`) to pass to
#'   `visNetwork::visNetwork()`
#'
#' @name pipeline-vis
#' @family pipeline
#' @examples
#' \dontrun{
#' # Run pipeline
#' make_with_source(
#'   "recode.R",
#'   "data/0 raw_data.R",
#'   "data/1 data.R"
#' )
#' make_with_source(
#'   "merge.R",
#'   c("data/1 data.R", "data/0 raw_pop.R"),
#'   "data/2 data.R"
#' )
#'
#' # Visualise pipeline with custom tooltips
#' show_pipeline(tooltips = c(
#'   "data/0 raw_data.R" = "Raw survey data",
#'   "data/0 raw_pop.R" = "Raw population data",
#'   "data/1 data.R" = "Survey data with recodes applied",
#'   "data/2 data.R" = "Survey data with demographic variables merged in"
#' ))
#' }
NULL
#' @rdname pipeline-vis
#' @export
show_pipeline <- function(pipeline = get_pipeline(), tooltips = NULL, labels = NULL, ...) {
  pipeline <- annotate_pipeline(pipeline, tooltips, labels)
  pipeline$print()
}

#' @rdname pipeline-vis
#' @export
save_pipeline <- function(file, pipeline = get_pipeline(), tooltips = NULL, labels = NULL, selfcontained = TRUE, background = "white", ...) {
  pipeline <- annotate_pipeline(pipeline, tooltips, labels)
  pipeline$save(file, ...)
}


# Internal ---------------------------------------------------------------------

#' @noRd
propagate_outofdateness <- function(initial_node, edges, next_node = NULL) {
  outdated <- FALSE
  target_mtime <- unique(edges[edges$to == initial_node, "to_mtime"])
  if (is.null(next_node)) next_node <- initial_node

  inputs <- edges[edges$to == next_node, ]
  for (i in seq_along(inputs$from)) {
    outdated <- inputs$.outdated[i] | inputs$from_mtime[i] > target_mtime
    next_node <- inputs$from[i]

    # Base case
    if (is.na(outdated)) outdated <- FALSE
    if (outdated) return(outdated)
    if (next_node == initial_node) return(TRUE) # Loop detected.

    # Recursive case
    outdated <- propagate_outofdateness(initial_node, edges, next_node)
  }

  outdated
}



#' @noRd
pipeline_network <- function(nodes, edges, ...) {
  out <- visNetwork::visNetwork(nodes = nodes, edges = edges, ...)
  out <- visNetwork::visGroups(out, groupname = "Out-of-date", color = "#ffcaef")
  out <- visNetwork::visGroups(out, groupname = "Up-to-date", color = "#caffda")
  out <- visNetwork::visLegend(out)

  visNetwork::visHierarchicalLayout(
    out, sortMethod = "directed", direction = "LR"
  )
}

#' @noRd
annotate_pipeline <- function(pipeline, tooltips, labels) {
  if (!is_pipeline(pipeline)) stop("`pipeline` must be a Pipeline object", call. = FALSE)

  if (!is.null(tooltips)) {
    validate_annotation(pipeline, tooltips, "tooltips")
    pipeline <- pipeline$clone(deep = TRUE)
    new_nodes <- apply_annotations(pipeline$nodes, tooltips, "title")
    pipeline$nodes <- new_nodes
  }

  if (!is.null(labels)) {
    validate_annotation(pipeline, labels, "labels")
    pipeline <- pipeline$clone(deep = TRUE)
    new_nodes <- apply_annotations(pipeline$nodes, labels, "label")
    pipeline$nodes <- new_nodes
  }

  pipeline
}

#' @noRd
validate_annotation <- function(pipeline, x, x_name) {
  stopifnot(is.character(x))
  if (!identical(length(names(x)), length(x))) {
    stop("`", x_name, "` must be named", call. = FALSE)
  }

  if (any(duplicated(names(x)))) {
    stop("names of `", x_name, "` must not be duplicated")
  }

  bad_nodes <- setdiff(names(x), as.character(pipeline$nodes$id))
  if (length(bad_nodes) > 0) {
    stop(
      "`", paste(bad_nodes, collapse = "`, "), "` ",
      "are not nodes in `pipeline`", call. = FALSE
    )
  }

  invisible(NULL)
}

#' @noRd
apply_annotations <- function(nodes, annotations, at) {
  nodes$node_id <- as.character(nodes$id)
  annotations <- data.frame(node_id = names(annotations), ..annotation = annotations)

  new_nodes <- merge(nodes, annotations, by = "node_id", all.x = TRUE)
  new_nodes[[at]] <- ifelse(
    is.na(new_nodes$..annotation),
    new_nodes[[at]],
    new_nodes$..annotation
  )
  new_nodes <- new_nodes[, setdiff(names(new_nodes), c("node_id", "..annotation"))]
  new_nodes
}
