#' Create a pipeline using roxygen tags
#'
#' @param dir A character vector of full path names; the default corresponds to the working directory
#' @param recursive A logical determining whether or not to recurse into
#'   subdirectories
#' @param build A logical determining whether or not the pipeline will be built
#'   immediately or simply returned to the user
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
make_with_dir <- function(dir = ".", recursive = FALSE, build = TRUE, quiet = getOption("makepipe.quiet")) {
  stop_required("roxygen2")
  envir <- new.env(parent = parent.frame())

  fp <- list.files(
    dir, recursive = recursive,
    pattern = "\\.R$", ignore.case = TRUE,
    full.names = TRUE
  )

  roxy_files <- lapply(fp, roxygen2::parse_file, env = NULL)
  roxy_files <- roxy_files[sapply(roxy_files, length)>0]

  pipeline <- Pipeline$new()
  for (f in roxy_files) {
    for (block in f) {
      block_tags <- lapply(block$tags, `[[`, "val")
      names(block_tags) <- lapply(block$tags, `[[`, "tag")
      if (!"targets" %in% names(block_tags)) next
      if (!"force" %in% names(block_tags)) block_tags$force <- FALSE

      segment <- pipeline$add_source_segment(
        block$file,
        block_tags$targets,
        block_tags$dependencies,
        block_tags$packages,
        envir,
        block_tags$force
      )
      add_note_and_label(pipeline, segment, block_tags$title, block_tags$description)

      break # Each file can have at most one make declaration
    }
  }

  if (build) pipeline$build(quiet)
  pipeline
}

# Helpers ----------------------------------------------------------------------

parse_vector <- function(x) {
  x <- trimws(x)
  x <- gsub("^\\[", "c(", x)
  x <- gsub("\\]$", ")", x)
  eval(parse(text=x))
}


#' @importFrom roxygen2 roxy_tag_parse
#' @export
roxy_tag_parse.roxy_tag_dependencies <- function(x) {
  out <- roxygen2::tag_markdown(x)
  out$val <- eval(parse(text=paste("c(", out$val, ")")))
  out
}

#' @importFrom roxygen2 roxy_tag_parse
#' @export
roxy_tag_parse.roxy_tag_targets <- function(x) {
  out <- roxygen2::tag_markdown(x)
  out$val <- eval(parse(text=paste("c(", out$val, ")")))
  out
}

#' @importFrom roxygen2 roxy_tag_parse
#' @export
roxy_tag_parse.roxy_tag_force <- function(x) {
  out <- roxygen2::tag_markdown(x)
  out$val <- eval(parse(text=paste("c(", out$val, ")")))
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

