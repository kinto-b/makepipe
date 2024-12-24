
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
      note = character(0),
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
      if (length(nodes) == 0) {
        return(invisible(self))
      } else if (length(nodes) == 1) {
        nodes <- nodes[[1]]
      } else {
        nodes <- unique(do.call(rbind, nodes))
      }

      # Restore old notes/labels
      nodes$label <- ""
      nodes$note  <- ""
      if (nrow(old_nodes) > 0) {
        labels <- old_nodes$label
        names(labels) <- as.character(old_nodes$id)
        nodes <- apply_annotations(nodes, labels, "label")

        notes <- old_nodes$note
        names(notes) <- as.character(old_nodes$id)
        nodes <- apply_annotations(nodes, notes, "note")
      }

      private$nodes <- nodes
      invisible(self)
    },

    #' @description Reconstruct Pipeline edges from Segment edges. Called
    #'   primarily to update outofdateness
    refresh_edges = function() {
      edges <- lapply(self$segments, function(x) x$edges)
      if (length(edges)==0) {
        return(invisible(self))
      } else if (length(edges) == 1) {
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
      if (warn_pipeline_is_empty(self$segments, "Nothing to build")) {
        return(invisible(self))
      }

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
      if (warn_pipeline_is_empty(self$segments, "Nothing to clean")) {
        return(invisible(self))
      }

      for (segment in self$segments) {
        file.remove(segment$targets)
      }

      invisible(self)
    },

    #' @description Touch all targets, updating file modification time to
    #'   current time. This is useful when you know your targets are all
    #'   up-to-date but makepipe doesn't (e.g. after a negligible change was
    #'   made to your source code).
    #' @return `self`
    touch = function() {
      if (warn_pipeline_is_empty(self$segments, "Nothing to touch")) {
        return(invisible(self))
      }

      now <- Sys.time()
      for (segment in self$segments) {
        Sys.setFileTime(segment$targets, now)
      }

      invisible(self)
    },


    #' @description Apply annotations to Pipeline
    #' @param labels A named character vector mapping nodes in the `Pipeline` onto
    #'   labels to display beside them.
    #' @param notes A named character vector mapping nodes in the `Pipeline` onto
    #'   notes to display on beside the labels (nomnoml) or as tooltips (visNetwork).
    annotate = function(labels = NULL, notes = NULL) {
      if (!is.null(notes)) {
        validate_annotation(notes, "notes", private$nodes)
        new_nodes <- apply_annotations(private$nodes, notes, "note")
        private$nodes <- new_nodes
      }

      if (!is.null(labels)) {
        validate_annotation(labels, "labels", private$nodes)
        new_nodes <- apply_annotations(private$nodes, labels, "label")
        private$nodes <- new_nodes
      }

      # Copy label/note info to Segments
      segment_nodes <- private$nodes$id[private$nodes$.source]
      segment_nodes <- private$edges[private$edges$from %in% segment_nodes, c("from", ".segment_id")]
      annotations <- private$nodes[private$nodes$.source, c("id", "label", "note")]
      annotations <- merge(segment_nodes, annotations, by.x = "from", by.y = "id")
      for (i in seq_along(annotations$.segment_id)) {
        id <- annotations$.segment_id[i]
        self$segments[[id]]$annotate(annotations$label[i], annotations$note[i])
      }

      invisible(self)
    },

    #' @description Refresh Pipeline to check outofdateness
    refresh = function() {
      private$refresh_edges()
      private$refresh_nodes()
      invisible(self)
    },

    #' @description Display the pipeline with nomnoml
    #'
    #' @return `self`
    #' @param direction The direction the flowchart should go in
    #' @param arrow_size The arrowhead size
    #' @param edge_style The arrow edge style
    #' @param bend_size The degree of rounding in the arrows (requires
    #'   `edge_style=rounded`)
    #' @param font The name of a font to use
    #' @param font_size The font size
    #' @param line_width The line width for arrows and box outlines
    #' @param padding The amount of padding *within* boxes
    #' @param spacing The amount of spacing *between* boxes,
    #' @param leading The amount of spacing between lines of text
    #' @param stroke The color of arrows, text, and box outlines
    #' @param fill_arrows Whether arrow heads are full triangles (`TRUE`) or
    #'   angled (`FALSE`)
    #' @param gutter The amount space to leave around the flowchart
    #' @param edge_margin The amount of space to leave between boxes and arrows

    nomnoml = function(direction = c("down", "right"),
                       arrow_size = 1,
                       edge_style = c("hard", "rounded"),
                       bend_size = 0.3,
                       font = "Courier",
                       font_size = 12,
                       line_width = 3,
                       padding = 16,
                       spacing = 40,
                       leading = 1.25,
                       stroke = "#33322E",
                       fill_arrows = FALSE,
                       gutter = 5,
                       edge_margin = 0) {
      if (warn_pipeline_is_empty(self$segments, "Nothing to display")) {
        return(invisible(self))
      }

      self$refresh()
      out <- pipeline_nomnoml_code(
        nodes = private$nodes,
        edges = private$edges,
        direction = direction,
        # ranker = ranker,
        arrow_size = arrow_size,
        edge_style = edge_style,
        bend_size = bend_size,
        font = font,
        font_size = font_size,
        line_width = line_width,
        padding = padding,
        spacing = spacing,
        leading = leading,
        stroke = stroke,
        # fill = fill,
        # title = title,
        # zoom = zoom,
        fill_arrows = fill_arrows,
        # acyclicer = acyclicer,
        gutter = gutter,
        edge_margin = edge_margin
      )

      print(nomnoml::nomnoml(out))
      invisible(self)
    },

    #' @description Display the pipeline with nomnoml
    #' @param ...  Arguments (other than `nodes` and `edges`) to pass to
    #'   `visNetwork::visNetwork()`
    #' @return `self`
    visnetwork = function(...) {
      stop_required("visNetwork")
      if (warn_pipeline_is_empty(self$segments, "Nothing to display")) {
        return(invisible(self))
      }

      self$refresh()
      out <- pipeline_network(nodes = private$nodes, edges = private$edges, ...)
      print(out)
      invisible(self)
    },

    #' @description Display a text summary of the pipeline
    #' @return `self`
    text_summary = function() {
      out <- pipeline_text_summary(private$nodes, private$edges, self$segments)
      cat(paste(out, collapse = "\n"))
    },

    #' @description Display
    #' @param ...  Arguments (other than `nodes` and `edges`) to pass to
    #'   `visNetwork::visNetwork()`
    #' @return `self`
    print = function(...) {
      self$nomnoml()
      invisible(self)
    },

    #' @description Save pipeline visNetwork
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
      stop_required("visNetwork")
      if (warn_pipeline_is_empty(self$segments, "Nothing to save")) {
        return(invisible(self))
      }

      self$refresh()
      out <- pipeline_network(nodes = private$nodes, edges = private$edges, ...)
      visNetwork::visSave(out, file, selfcontained, background)
      invisible(self)
    },

    #' @description Save pipeline nomnoml
    #' @param file File to save the png into
    #' @param width Image width
    #' @param height Image height
    #' @param ...  Arguments to pass to `self$nomnoml()`
    #' @return `self`
    save_nomnoml = function(file, width = NULL, height = NULL, ...) {
      stop_required("webshot2")
      if (warn_pipeline_is_empty(self$segments, "Nothing to save")) {
        return(invisible(self))
      }

      stopifnot("`file` must be a .png path" = grepl(x=file, ".png$"))
      wd <- getwd()
      on.exit(setwd(wd))

      self$refresh()
      code <- pipeline_nomnoml_code(nodes = private$nodes, edges = private$edges, ...)

      x <- list(code = code, svg = FALSE)
      widget <- htmlwidgets::createWidget(
        name = "nomnoml",
        x, width = width, height = height,
        package = "nomnoml"
      )

      html <- tempfile("flow_", fileext = ".html")
      htmlwidgets::saveWidget(widget, html)

      out <- tempfile("out_", fileext = ".png")

      setwd(tempdir())
      webshot2::webshot(basename(html), out, selector = "canvas", quiet=TRUE)
      setwd(wd)

      file.copy(out, file)
      unlink(out)
      unlink(html)

      invisible(self)
    },

    #' @description Save a text summary of the pipeline
    #' @param file File to save text summary into
    #' @return `self`
    save_text_summary = function(file) {
      is_txt <- grepl(x=file, ".(txt|md)$", ignore.case = TRUE)
      stopifnot("`file` must be a .txt or .md path" = is_txt)

      out <- pipeline_text_summary(private$nodes, private$edges, self$segments)
      writeLines(out, con = file)

      invisible(self)
    }
  )
)

# Accessors --------------------------------------------------------------------

#' Access and interface with Pipeline.
#'
#' `get_pipeline()`, `set_pipeline()` and `reset_pipeline()` access and modify
#' the current *active* pipeline, while all other helper functions do not affect
#' the active pipeline
#'
#'
#' @param pipeline A pipeline. See [Pipeline] for more details.
#' @name pipeline-accessors
#' @family pipeline
#' @examples
#' \dontrun{
#' # Build up a pipeline from scratch and save it out
#' reset_pipeline()
#' # A series of `make_with_*()` blocks go here...
#' saveRDS(get_pipeline(), "data/my_pipeline.Rds")
#'
#' # ... Later on we can read in and set the pipeline
#' p <- readRDS("data/my_pipeline.Rds")
#' set_pipeline(p)
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

#' @rdname pipeline-accessors
#' @export
reset_pipeline <- function() {
  old <- makepipe_env$pipeline
  makepipe_env$pipeline <- Pipeline$new()
  invisible(old)
}


# Visualisors ------------------------------------------------------------------

#' Visualise the Pipeline.
#'
#' Produce a flowchart visualisation of the pipeline. Out-of-date targets will
#' be coloured red, up-to-date targets will be coloured green, and everything
#' else will be blue.
#'
#' Labels and notes must be supplied as named character vector where the
#' names correspond to the filepaths of nodes (i.e. `targets`, `dependencies`,
#' or `source` scripts)
#'
#' @param file File to save png (nomnoml) or html (visnetwork) into
#' @param pipeline A pipeline. See [Pipeline] for more details.
#' @param as A string determining whether to use `nomnoml` or `visNetwork`
#' @param labels A named character vector mapping nodes in the `pipeline` onto
#'   labels to display beside them.
#' @param notes A named character vector mapping nodes in the `Pipeline` onto
#'   notes to display on beside the labels (nomnoml) or as tooltips (visNetwork).
#' @param ...  Arguments passed onto `Pipeline$nomnoml()` or `Pipeline$visnetwork`
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
#' # Visualise pipeline with custom notes
#' show_pipeline(notes = c(
#'   "data/0 raw_data.R" = "Raw survey data",
#'   "data/0 raw_pop.R" = "Raw population data",
#'   "data/1 data.R" = "Survey data with recodes applied",
#'   "data/2 data.R" = "Survey data with demographic variables merged in"
#' ))
#' }
NULL
#' @rdname pipeline-vis
#' @export
show_pipeline <- function(pipeline = get_pipeline(),
                          as = c("nomnoml", "visnetwork", "text"),
                          labels = NULL, notes = NULL, ...) {
  as <- match.arg(as)
  pipeline$annotate(labels, notes)
  switch(as,
    nomnoml = pipeline$nomnoml(...),
    visnetwork = pipeline$visnetwork(...),
    text = pipeline$text_summary()
  )
}

#' @rdname pipeline-vis
#' @export
save_pipeline <- function(file, pipeline = get_pipeline(),
                          as = c("nomnoml", "visnetwork", "text"),
                          labels = NULL, notes = NULL, ...) {
  as <- match.arg(as)
  pipeline$annotate(labels, notes)
  switch(as,
         nomnoml = pipeline$save_nomnoml(file, ...),
         visnetwork = pipeline$save_visnetwork(file, ...),
         text = pipeline$save_text_summary(file, ...)
  )
}


# Internal ---------------------------------------------------------------------

#' Issue a warning because the pipeline is empty
#'
#' @param segments Pipeline segments
#' @param msg A message to warn with if pipeline is empty
#'
#' @return TRUE (if pipeline is empty) or FALSE
#' @noRd
#' @keywords internal
#'
warn_pipeline_is_empty <- function(segments, msg) {
  empty <- length(segments) == 0
  if (empty) warning("`Pipeline` is empty. ", msg, call. = FALSE)
  empty
}

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
  if (is.null(edges)) return(NULL)
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
  stop_required("visNetwork")

  # Add default notes/labels
  recipe_no_label <- nodes$.recipe & (nodes$label == "")
  nodes$label[recipe_no_label] <- "Recipe"
  no_label <-nodes$label == ""
  nodes$label[no_label] <- basename(as.character(nodes$id[no_label]))
  no_note <- (nodes$note == "")
  nodes$note[no_note] <- as.character(nodes$id[no_note])


  # visNetwork expects tooltips to be stored in `title` column
  nodes$title <- nodes$note

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
pipeline_nomnoml_code <- function(nodes,
                                  edges,
                                  direction = c("down", "right"),
                                  ranker = c("network-simplex", "tight-tree", "longest-path"),
                                  arrow_size = 1,
                                  edge_style = c("hard", "rounded"),
                                  bend_size = 0.3,
                                  font = "Courier",
                                  font_size = 12,
                                  line_width = 3,
                                  padding = 16,
                                  spacing = 40,
                                  leading = 1.25,
                                  stroke = "#33322E",
                                  fill = "#eee8d5",
                                  title = "filename",
                                  zoom = 1,
                                  fill_arrows = FALSE,
                                  acyclicer = "greedy",
                                  gutter = 5,
                                  edge_margin = 0) {
  direction <- match.arg(direction)
  edge_style     <- match.arg(edge_style)
  ranker    <- match.arg(ranker)

  # FALSE or TRUE must become "false" or "true" for nomnoml
  fill_arrows <- tolower(fill_arrows)

  # Add default notes/labels
  recipe_no_label <- nodes$.recipe & (nodes$label == "")
  nodes$label[recipe_no_label] <- "Recipe"
  no_label <-nodes$label == ""
  nodes$label[no_label] <- basename(as.character(nodes$id[no_label]))
  recipe_no_note <- nodes$.recipe & (nodes$note == "")
  nodes$note[recipe_no_note] <- as.character(nodes$id[recipe_no_note])

  # Enforce uniquness of label since this is our nomnoml id
  nodes$label <- make.unique(nodes$label, sep = " +")

  # Now wrap the ones that aren't code
  note_matches_id <- nodes$note==as.character(nodes$id)
  nodes$note[!note_matches_id] <- strwrap2(nodes$note[!note_matches_id], 40)

  # Escape special characters
  nodes$label <- escape_pipes_and_brackets(nodes$label)
  nodes$note <- escape_pipes_and_brackets(nodes$note)

  # Add aesthetics

  outdated <- nodes$id %in% edges[edges$.outdated, "to"]
  nodes$shape <- ifelse(nodes$.recipe, "recipe", "box")
  nodes$shape <- ifelse(nodes$.pkg, "pkg", nodes$shape)
  nodes$color <- ifelse(outdated, "red", "green")
  nodes$color <- ifelse(nodes$.source, "blue", nodes$color)

  # Build boxes
  nodes$box <- NA
  nodes$box <- sprintf(
    "[<%s%s> %s | %s]",
    nodes$color,
    nodes$shape,
    nodes$label,
    nodes$note
  )
  nodes$box  <- sub(" \\|\\s* ]$", "]", nodes$box) # Cleanup if no note

  # Graph
  nom_edges <- sprintf(
    "%s --> %s",
    nodes$box[match(edges$from, nodes$id)],
    nodes$box[match(edges$to, nodes$id)]
  )
  nom_edges <- paste(nom_edges, collapse = "\n")

  # Styles
  header <- paste0(
    "#.redbox: fill=#ffcaef title=bold align=center\n",
    "#.greenbox: fill=#caffda title=bold align=center\n",
    "#.bluebox: fill=#77b6fe title=bold align=center\n",
    "#.bluerecipe: align=center fill=#77b6fe title=bold\n",
    "#.bluepkg: visual=database fill=#77b6fe title=bold align=center\n",
    "#arrowSize: ", arrow_size, "\n",
    "#bendSize: ", bend_size, "\n",
    "#direction: ", direction, "\n",
    "#gutter: ", gutter, "\n",
    "#edgeMargin: ", edge_margin, "\n",
    "#edges: ", edge_style, "\n",
    "#fill: ", fill, "\n",
    "#fillArrows: ", fill_arrows, "\n",
    "#font: ", font, "\n",
    "#fontSize: ", font_size, "\n",
    "#leading: ", leading, "\n",
    "#lineWidth: ", line_width, "\n",
    "#padding: ", padding, "\n",
    "#spacing: ", spacing, "\n",
    "#stroke: ", stroke, "\n",
    "#title: ", title, "\n",
    "#zoom: ", zoom, "\n",
    "#acyclicer: ", acyclicer, "\n",
    "#ranker: ", ranker, "\n"
  )

  paste0(header,"\n", nom_edges)
}


escape_pipes_and_brackets <- function(x) {
  x <- gsub("]","\\]", x ,fixed = TRUE)
  x <- gsub("[","\\[", x ,fixed = TRUE)
  x <- gsub("|","\\|", x ,fixed = TRUE)
  x
}


# Text summary ------------------------------------------------------------

pipeline_text_summary <- function(nodes, edges, segments) {
  edges <- sort_topologically(edges)

  # Sort segments by topological order
  segment_nodes <- nodes$id[nodes$.source]
  segment_nodes <- edges[edges$from %in% segment_nodes, c("from", ".segment_id", "level")]
  segment_nodes <- segment_nodes[order(segment_nodes$level), ]

  out <- c("# Pipeline", "")
  for (i in segment_nodes$.segment_id) {
    out <- c(out, segments[[i]]$text_summary, "")
  }

  out
}

