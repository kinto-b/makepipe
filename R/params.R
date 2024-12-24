#' Parameters for make-like functions
#' @keywords internal
#' @param source The path to an R script which makes the `targets`
#' @param recipe A chunk of R code which makes the `targets`
#' @param targets A character vector of paths to files
#' @param dependencies A character vector of paths to files which the `targets`
#'   depend on
#' @param packages A character vector of names of packages which `targets`
#'   depend on
#' @param envir The environment in which to execute the `source` or `recipe`. By
#'   default, execution will take place in a fresh environment whose parent is
#'   the calling environment.
#' @param quiet A logical determining whether or not messages are signaled
#' @param force A logical determining whether or not execution of the `source`
#'   or `recipe` will be forced (i.e. happen whether or not the targets are
#'   out-of-date)
#' @param label A short label for the `source` or `recipe`, displayed in pipeline
#'   visualisations. If `NULL`, the `basename(source)` or 'Recipe' will be used.
#' @param build A logical determining whether or not the pipeline/segment will be built
#'   immediately or simply returned to the user
#' @name make_params
NULL
