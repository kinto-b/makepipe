#' Create a pipeline using roxygen tags
#'
#' Instead of maintaining a separate pipeline script containing calls to
#' `make_with_source()`, you can add roxygen-like headers to the .R files in
#' your pipeline containing the `@makepipe` tag along with `@targets`,
#' `@dependencies`, and so on. These tags will be parsed by `make_with_dir()`
#' and used to construct a pipeline. You can call a specific part of the
#' pipeline that has been documented in this way using `make_with_roxy()`.
#'
#' Other than `@makepipe`, which is used to tell whether a given script should
#' be included in the pipeline, the tags recognised mirror the arguments to
#' `make_with_source()`. In particular,
#'
#' - `@targets` and `@dependencies` are for declaring inputs and outputs, the
#' expected format is a comma separated list of strings like
#' `@targets "out1.Rds", "out2.Rds"` but R code like `@targets file.path(DIR, "out.Rds")`
#' (evaluated in `envir`) works too
#' - `@packages` is for declaring the packages that the targets depend on, the
#' expected format is `@packages pkg1 pkg2 etc`
#' - `@force` is for declaring whether or not execution should be forced, the
#' expected format is a logical like `TRUE` or `FALSE`
#'
#' See the getting started vignette for more information.
#'
#' @param dir A character vector of full path names; the default corresponds to the working directory
#' @param recursive A logical determining whether or not to recurse into
#'   subdirectories
#' @inheritParams make_params
#'
#' @return A `Pipeline` object
#' @export
#' @family make
#'
#' @examples
#' \dontrun{
#' # Create a pipeline from scripts in the working dir without executing it
#' p <- make_with_dir(build = FALSE)
#' p$build() # Then execute it yourself
#' }
make_with_dir <- function(dir = ".", recursive = FALSE, build = TRUE,
                          envir = new.env(parent = parent.frame()),
                          quiet = getOption("makepipe.quiet")) {
  source_files <- list.files(
    dir,
    recursive = recursive,
    pattern = "\\.R$", ignore.case = TRUE,
    full.names = TRUE
  )

  any_found <- FALSE
  for (fp in source_files) {
    res <- make_with_tags(fp, envir, quiet, build)
    if (!is.null(res)) any_found <- TRUE
  }
  if (!any_found) {
    warning(
      "No `@makepipe` tags found in any R scripts in'", dir, "'",
      call. = FALSE
    )
  }

  pipeline <- get_pipeline()
  if (build) pipeline$build(quiet)
  pipeline
}


#' @rdname make_with_dir
#' @export
make_with_roxy <- function(source,
                           envir = new.env(parent = parent.frame()),
                           quiet = getOption("makepipe.quiet"),
                           build = TRUE) {
  segment <- make_with_tags(source, envir, quiet, warn = TRUE)
  if (is.null(segment)) {
    return(invisible(NULL))
  }

  if (build) {
    segment$execute(quiet = quiet)
  }

  invisible(segment)
}


# Internal ---------------------------------------------------------------------
#' Make targets out of dependencies using a source file with a roxygen header
#'
#' @inheritParams make_params
#' @param warn A logical determining whether or not a warning is signaled if
#'   no `@makepipe` tag is found
#'
#' @return A `Segment` object containing execution metadata.
#' @noRd
#' @keywords internal
make_with_tags <- function(source,
                           envir = new.env(parent = parent.frame()),
                           quiet = getOption("makepipe.quiet"),
                           warn = FALSE) {
  if (!file.exists(source)) stop("Cannot find file '", source, "'", call. = FALSE)
  f <- roxygen2::parse_file(source, envir)
  if (length(f) == 0) {
    if (warn) warn_no_makepipe_tag(source)
    return(NULL)
  }

  is_makepipe_block <- sapply(f, function(block) {
    "makepipe" %in% sapply(block$tags, `[[`, "tag")
  })
  if (sum(is_makepipe_block) == 0) {
    if (warn) warn_no_makepipe_tag(source)
    return(NULL)
  }

  pipeline <- get_pipeline()
  if (is.null(pipeline)) {
    pipeline <- Pipeline$new()
    set_pipeline(pipeline)
  }

  # Each file can have at most one make declaration
  makepipe_block <- which(is_makepipe_block)[1] # Use first declaration
  makepipe_block <- f[[makepipe_block]]
  if (sum(is_makepipe_block) > 1) {
    warning(
      "More than one makepipe declaration in '",
      basename(makepipe_block$file), "'. Only the first will be used",
      call. = FALSE
    )
  }

  # Extract makepipe tags
  block_tags <- lapply(makepipe_block$tags, `[[`, "val")
  names(block_tags) <- lapply(makepipe_block$tags, `[[`, "tag")
  if (!"force" %in% names(block_tags)) block_tags$force <- FALSE

  # Evaluate targets/dependencies
  if (!is.null(block_tags$dependencies)) {
    block_tags$dependencies <- evaluate_vector(block_tags$dependencies, envir = envir)
  }
  if (!is.null(block_tags$targets)) {
    block_tags$targets <- evaluate_vector(block_tags$targets, envir = envir)
  }

  # Add segment
  segment <- pipeline$add_source_segment(
    makepipe_block$file,
    block_tags$targets,
    block_tags$dependencies,
    block_tags$packages,
    envir,
    block_tags$force
  )
  add_note_and_label(pipeline, segment, block_tags$title, block_tags$description)

  segment
}

# Helpers ----------------------------------------------------------------------

warn_no_makepipe_tag <- function(source) {
  warning(
    "No `@makepipe` tag found in '",
    basename(source), "'",
    call. = FALSE
  )
}

evaluate_vector <- function(x, envir) {
  eval(parse(text = paste("c(", x, ")")), envir = envir)
}


#' @importFrom roxygen2 roxy_tag_parse
#' @export
roxy_tag_parse.roxy_tag_makepipe <- function(x) {
  roxygen2::tag_toggle(x)
}

#' @importFrom roxygen2 roxy_tag_parse
#' @export
roxy_tag_parse.roxy_tag_dependencies <- function(x) {
  roxygen2::tag_markdown(x)
}

#' @importFrom roxygen2 roxy_tag_parse
#' @export
roxy_tag_parse.roxy_tag_targets <- function(x) {
  roxygen2::tag_markdown(x)
}

#' @importFrom roxygen2 roxy_tag_parse
#' @export
roxy_tag_parse.roxy_tag_force <- function(x) {
  out <- roxygen2::tag_markdown(x)
  out$val <- eval(parse(text = paste("c(", out$val, ")")))
  out$val <- as.logical(out$val)
  out
}

#' @importFrom roxygen2 roxy_tag_parse
#' @export
roxy_tag_parse.roxy_tag_packages <- function(x) {
  out <- roxygen2::tag_words(x)
  out$val <- gsub(",", "", out$val)
  out$val <- gsub("'", "", out$val)
  out$val <- gsub('"', "", out$val)
  out
}
