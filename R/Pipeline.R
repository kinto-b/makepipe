
# R6 Object --------------------------------------------------------------------

#' Pipeline visualisations
#'
#' @description A Pipeline object is automatically constructed as calls to
#'   `make_*()` are made. It stores the relationships between targets,
#'   dependencies, and sources.
#'
#' @param recipe A language object which, when evaluated, makes the `targets`
#' @param source The path to an R script which makes the `targets`
#' @param targets A character vector of paths to files
#' @param dependencies A character vector of paths to files which the
#'   `targets` depend on
#' @param packages A character vector of names of packages which `targets`
#'   depend on
#' @param envir The environment in which to execute the `source` or `recipe`.
#' @param force A logical determining whether or not execution of the `source`
#'   or `recipe` will be forced (i.e. happen whether or not the targets are
#'   out-of-date)
#'
#' @keywords internal
#' @family pipeline
#' @export Pipeline
#' @aliases Pipeline
#' @importFrom R6 R6Class
Pipeline <- R6::R6Class(classname = "Pipeline",
  private = list(
    edges = data.frame(
      from = factor(),
      to = factor(),
      arrows = character(0),
      .segment_id = integer(0),
      .source = logical(0),
      .recipe = logical(0),
      .pkg = logical(0),
      .outdated = logical(0),
      stringsAsFactors = FALSE
    ),
    nodes = data.frame(
      id = factor(),
      label = character(0),
      title = character(0),
      .source = logical(0),
      .recipe = logical(0),
      .pkg = logical(0),
      stringsAsFactors = FALSE
    ),

    #' @description Add an edge to `edges`
    #' @param new_edge An data.frame constructed with `new_edge()`
    add_edge = function(new_edge) {
      edges <- private$edges

      # Relevel
      all_ids <- unlist(list(edges$from, edges$to, new_edge$from, new_edge$to))
      lvls <- levels(all_ids)
      edges$from <- factor(edges$from, lvls)
      edges$to <- factor(edges$to, lvls)
      new_edge$from <- factor(new_edge$from, lvls)
      new_edge$to <- factor(new_edge$to, lvls)

      # Combine
      private$edges <- rbind(edges, new_edge)

      invisible(self)
    },

    #' @description Add any nodes in `private$edges` that are missing from
    #'   `private$nodes` into `private$nodes`
    refresh_nodes = function() {
      edges <- private$edges
      old_nodes <- private$nodes

      nodes <- lapply(self$segments, function(x) x$nodes)
      if (length(nodes) == 1) {
        nodes <- nodes[[1]]
      } else {
        nodes <- unique(do.call(rbind, nodes))
      }

      # Add titles/labels
      lbl <- basename(as.character(nodes$id))
      nodes$label <- ifelse(nodes$.recipe, "Recipe", lbl)
      nodes$title <- ""

      # Restore old titles/labels
      labels <- old_nodes$label
      names(labels) <- as.character(old_nodes$id)
      nodes <- apply_annotations(nodes, labels, "label")

      titles <- old_nodes$title
      names(titles) <- as.character(old_nodes$id)
      nodes <- apply_annotations(nodes, titles, "title")

      private$nodes <- nodes
      invisible(self)
    },

    #' @description Reconstruct Pipeline edges from Segment edges. Called
    #'   primarily to update outofdateness
    refresh_edges = function() {
      edges <- lapply(self$segments, function(x) x$edges)
      if (length(edges) == 1) {
        edges <- edges[[1]]
      } else {
        edges <- do.call(rbind, edges)
      }
      edges <- propagate_outofdateness(edges)

      private$edges <- edges
      invisible(self)
    }
  ),
  public = list(
    #' @field segments A list of `Segment` objects
    segments = NULL,

    #' @description Add a pipeline segment corresponding to a `make_with_source()`
    #'   call
    #' @return The `SegmentSource` added to the `Pipeline`
    add_source_segment = function(source, targets, dependencies, packages, envir, force) {
      id <- as.integer(length(self$segments) + 1)
      new_segment <- SegmentSource$new(
        id, source, targets, dependencies, packages, envir, force,
        FALSE, NULL, NULL
      )

      self$segments <- c(self$segments, new_segment)
      private$add_edge(new_segment$edges)
      private$refresh_nodes()

      new_segment
    },

    #' @description Add a pipeline segment corresponding to a `make_with_recipe()`
    #'   call
    #' @return The `SegmentRecipe` added to the `Pipeline`
    add_recipe_segment = function(recipe, targets, dependencies, packages, envir, force) {
      id <- length(self$segments) + 1
      new_segment <- SegmentRecipe$new(
        id, recipe, targets, dependencies, packages, envir, force,
        FALSE, NULL, NULL
      )

      self$segments <- c(self$segments, new_segment)
      private$add_edge(new_segment$edges)
      private$refresh_nodes()

      new_segment
    },

    #' @description Build all targets
    #' @param quiet A logical determining whether or not messages are signaled
    #' @return `self`
    build = function(quiet = getOption("makepipe.quiet")) {
      edges <- sort_topologically(private$edges)
      executables <- edges[edges$.source, ]
      execution_order <- vapply(
        split(executables, executables$.segment_id),
        function(x) max(x$level),
        numeric(1)
      )
      execution_order <- sort(execution_order)

      for (segment_id in names(execution_order)) {
        segment_id <- as.integer(segment_id)
        self$segments[[segment_id]]$execute()
      }

      invisible(self)
    },

    #' @description Clean all targets
    #' @return `self`
    clean = function() {
      for (segment in self$segments) {
        file.remove(segment$targets)
      }

      invisible(self)
    },

    #' @description Apply annotations to Pipeline
    #' @param tooltips A named character vector mapping nodes in the `Pipeline` onto
    #'   tooltips to display on hover-over.
    #' @param labels A named character vector mapping nodes in the `Pipeline` onto
    #'   labels to display beside them.
    annotate = function(tooltips, labels) {
      if (!is.null(tooltips)) {
        validate_annotation(tooltips, "tooltips", private$nodes)
        new_nodes <- apply_annotations(private$nodes, tooltips, "title")
        private$nodes <- new_nodes
      }

      if (!is.null(labels)) {
        validate_annotation(labels, "labels", private$nodes)
        new_nodes <- apply_annotations(private$nodes, labels, "label")
        private$nodes <- new_nodes
      }

      invisible(self)
    },

    #' @description Refresh Pipeline to check outofdateness
    refresh = function() {
      private$refresh_edges()
      private$refresh_nodes()
      invisible(self)
    },

    #' @description Display pipeline
    #' @param ...  Arguments (other than `nodes` and `edges`) to pass to
    #'   `nomnoml::nomnoml()`
    #' @return `self`
    nomnoml = function(...) {
      self$refresh()
      out <- pipeline_nomnoml(nodes = private$nodes, edges = private$edges, ...)
      print(out)
      invisible(self)
    },

    #' @description Display pipeline
    #' @param ...  Arguments (other than `nodes` and `edges`) to pass to
    #'   `visNetwork::visNetwork()`
    #' @return `self`
    visnetwork = function(...) {
      self$refresh()
      out <- pipeline_network(nodes = private$nodes, edges = private$edges, ...)
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
    save_visnetwork = function(file, selfcontained = TRUE, background = "white", ...) {
      self$refresh()
      out <- pipeline_network(nodes = private$nodes, edges = private$edges, ...)
      visNetwork::visSave(out, file, selfcontained, background)
      invisible(self)
    }
  )
)

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
  pipeline$annotate(tooltips, labels)
  pipeline$print()
}

#' @rdname pipeline-vis
#' @export
save_pipeline <- function(file, pipeline = get_pipeline(), tooltips = NULL, labels = NULL, selfcontained = TRUE, background = "white", ...) {
  pipeline$annotate(tooltips, labels)
  pipeline$save(file, ...)
}


# Internal ---------------------------------------------------------------------

#' Propagate out-of-dateness
#'
#' Any target that is downstream of an out-of-date target is itself out-of-date.
#' This algorithm ensures the `edges` data.frame reflects this fact by
#' propagating out-of-dateness along network edges.
#'
#' @param edges A data.frame defining the edges
#'
#' @return A data.frame defining edges from all nodes in `from` to all nodes in
#'   `to`.
#' @noRd
propagate_outofdateness <- function(edges) {
  nodes <- unlist(list(edges$from, edges$to))
  nodes_left <- nodes
  edges_left <- edges
  while(length(nodes_left)){
    # Targets with no dependencies
    next_targets <- setdiff(nodes_left, edges_left$to)
    next_targets <- factor(next_targets, levels = levels(nodes))

    # Step outofdateness forward
    for (i in next_targets) {
      dependencies <- edges[edges$to %in% i, "from"]
      if (length(dependencies) == 0) next
      dependency_outdated <- any((edges$to %in% dependencies) & edges$.outdated)
      outdated <- edges[edges$to %in% i, ".outdated"] | dependency_outdated
      edges[edges$to %in% i, ".outdated"] <- outdated
    }

    # Prune the graph.
    nodes_left <- setdiff(nodes_left, next_targets)
    edges_left <- edges_left[!(edges_left$from %in% next_targets),]
  }

  edges[edges$.source, ".outdated"] <- FALSE

  edges
}

#' Sort edges topologically
#'
#' This algorithm sorts edges topologically by starting with nodes without
#' dependencies, assigning them to first level, removing them from the graph and
#' then repeating the process. Once again, we take nodes without any
#' dependencies left in the graph, assign them to second level. We keep going
#' until there are no nodes left in the graph.
#'
#' @param edges A data.frame defining the edges
#'
#' @return A data.frame defining edges from all nodes in `from` to all nodes in
#'   `to`.
#' @noRd
sort_topologically <- function(edges) {
  level <- 1
  edges$level <- NA
  nodes <- unlist(list(edges$from, edges$to))

  nodes_left <- nodes
  edges_left <- edges
  while(length(nodes_left)){
    # Targets with no dependencies
    next_targets <- setdiff(nodes_left, edges_left$to)
    next_targets <- factor(next_targets, levels = levels(nodes))

    # Assign to level
    edges[edges$from %in% next_targets, "level"] <- level

    # Prune the graph.
    nodes_left <- setdiff(nodes_left, next_targets)
    edges_left <- edges_left[!(edges_left$from %in% next_targets),]

    level <- level + 1
  }

  edges
}

## Network ---------------------------------------------------------------------
#' Create a network visualisation of the Pipeline
#'
#' @param nodes A data.frame defining the nodes
#' @param edges A data.frame defining the edges
#' @param ...  Arguments (other than `nodes` and `edges`) to pass to
#'   `visNetwork::visNetwork()`
#' @return A visNetwork
#' @noRd
pipeline_network <- function(nodes, edges, ...) {

  # Add aesthetics to nodes
  nodes$shape <- ifelse(nodes$.recipe, "circle", "square")
  nodes$shape <- ifelse(nodes$.pkg, "triangle", nodes$shape)

  outdated <- nodes$id %in% edges[edges$.outdated, "to"]
  nodes$group <- ifelse(outdated, "Out-of-date", "Up-to-date")
  nodes$group <- ifelse(nodes$.source, "Source", nodes$group)

  # Visualise
  out <- visNetwork::visNetwork(nodes = nodes, edges = edges, ...)
  out <- visNetwork::visGroups(out, groupname = "Out-of-date", color = "#ffcaef")
  out <- visNetwork::visGroups(out, groupname = "Up-to-date", color = "#caffda")
  out <- visNetwork::visLegend(out)

  visNetwork::visHierarchicalLayout(
    out, sortMethod = "directed", direction = "LR"
  )
}


#' Validate annotations
#'
#' @param x An annotation
#' @param x_name A string, the kind of annotation
#' @param nodes A data.frame defining the nodes
#'
#' @return A data.frame defining edges from all nodes in `from` to all nodes in
#'   `to`.
#' @noRd
validate_annotation <- function(x, x_name, nodes) {
  stopifnot_class(x, "character")
  if (!identical(length(names(x)), length(x))) {
    stop("`", x_name, "` must be named", call. = FALSE)
  }

  if (any(duplicated(names(x)))) {
    stop("names of `", x_name, "` must not be duplicated", call. = FALSE)
  }

  bad_nodes <- setdiff(names(x), as.character(nodes$id))
  if (length(bad_nodes) > 0) {
    stop(
      "`", paste(bad_nodes, collapse = "`, "), "` ",
      "are not nodes in `Pipeline`", call. = FALSE
    )
  }

  invisible(NULL)
}

#' Validate annotations
#'
#' @param nodes A data.frame defining the nodes
#' @param annotations A named character vector of annotations to apply
#' @param at A string, the kind of annotation to apply
#'
#' @return A data.frame defining the nodes with annotations applied
#' @noRd
apply_annotations <- function(nodes, annotations, at) {
  nodes$node_id <- as.character(nodes$id)
  annotations <- data.frame(
    node_id = names(annotations),
    ..annotation = annotations,
    stringsAsFactors = FALSE
  )

  new_nodes <- merge(nodes, annotations, by = "node_id", all.x = TRUE)
  new_nodes[[at]] <- ifelse(
    is.na(new_nodes$..annotation),
    new_nodes[[at]],
    new_nodes$..annotation
  )
  new_nodes <- new_nodes[, setdiff(names(new_nodes), c("node_id", "..annotation"))]
  new_nodes
}


## Nomnoml ---------------------------------------------------------------------

pipeline_nomnoml <- function(nodes, edges, add = NULL, ...) {
  # TODO: Display out-of-dateness
  # TODO: Differentiate recipe, source, etc.

  # Deduplicate labels
  dups <- duplicated(nodes$label)
  if (any(dups)) {
    warning("Replacing duplicate node labels with node ids")
    nodes$label <- nodes$id
  }

  # Add notes
  nodes$label <- ifelse(
    nodes$title != "",
    paste(nodes$label, stringr::str_wrap(nodes$title, 30), sep = "|"),
    nodes$label
  )

  # Graph
  nom_fr <- nodes$label[match(edges$from, nodes$id)]
  nom_to <- nodes$label[match(edges$to, nodes$id)]
  nom_edges <- paste0("[", nom_fr, "]", "-->", "[", nom_to, "]")
  nom_edges <- paste(nom_edges, collapse = "\n")

  # Addenda (e.g. styles)
  add <- paste(add, collapse = "\n")
  out <- paste(nom_edges, add, collapse = "\n")

  nomnoml::nomnoml(out, ...)
}


