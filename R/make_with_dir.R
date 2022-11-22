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
make_with_dir <- function(dir = ".", recursive = FALSE, build = TRUE,
                          envir = new.env(parent = parent.frame()),
                          quiet = getOption("makepipe.quiet")) {
  fp <- list.files(
    dir, recursive = recursive,
    pattern = "\\.R$", ignore.case = TRUE,
    full.names = TRUE
  )

  roxy_files <- lapply(fp, roxygen2::parse_file, env = envir)
  roxy_files <- roxy_files[sapply(roxy_files, length)>0]

  pipeline <- Pipeline$new()
  for (f in roxy_files) {
    is_makepipe_block <- sapply(f, function(block) {
      "makepipe" %in% sapply(block$tags, `[[`, "tag")
    })
    if (sum(is_makepipe_block) == 0) next

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
roxy_tag_parse.roxy_tag_makepipe <- function(x) {
  roxygen2::tag_toggle(x)
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

