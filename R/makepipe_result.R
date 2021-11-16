
#' Construct `make_*()` results
#'
#' @inheritParams make_params
#' @param executed A logical, whether or not the instructions were executed
#' @param result An object, whatever is returned by executing the instructions
#' @param instructions Either the name of a source file or a recipe expression
#' @param subclass Either "source" or "recipe"
#'
#' @return An object of class `makepipe_result`
#' @noRd
makepipe_result <- function(executed, result, instructions, execution_time,
                            targets, dependencies, packages, envir, subclass) {
  stopifnot(subclass %in% c("source", "recipe"))
  subclass <- paste0("makepipe_result_", subclass)
  x <-

  structure(
    list(
      result = result,
      executed = executed,
      instructions = instructions,
      execution_time = execution_time,
      targets = targets,
      dependencies = dependencies,
      packages = packages,
      envir = envir
    ),
    class = c(subclass, "makepipe_result")
  )
}


# Printers ---------------------------------------------------------------------


#' @export
print.makepipe_result_source <- function(x, ...) {
  x$instructions <- paste0("Source: '", x$instructions, "'")
  x$result <- paste0("Result: ", length(x$result), " object(s)")
  NextMethod()
}

#' @export
print.makepipe_result_recipe <- function(x, ...) {
  x$instructions <- paste(deparse(x$instructions), collapse = "\n")
  x$instructions <- paste0("Recipe: \n\n", x$instructions, "\n")
  x$result <- ifelse(is.null(x$result), "Result: None", "Result: 1 object")
  NextMethod()
}

#' @export
print.makepipe_result <- function(x, ...) {
  targets <- paste0("'", paste(x$targets, collapse = "', '"), "'")
  dependencies <- paste0("'", paste(x$dependencies, collapse = "', '"), "'")
  if (length(x$packages) > 0) {
    x$packages <- paste0("'", paste(x$packages, collapse = "', '"), "'")
  }


  cli::cat_line(cli::col_grey("# makepipe segment"))
  cli::cat_bullet(x$instructions)
  cli::cat_bullet("Targets: ", targets)
  cli::cat_bullet("File dependencies: ", dependencies)
  if (length(x$packages) > 0) cli::cat_bullet("Package dependencies: ", x$packages)
  cli::cat_bullet("Executed: ", x$executed)
  if (x$executed) cli::cat_bullet("Execution time: ", x$execution_time)
  if (x$executed) cli::cat_bullet(x$result)
  cli::cat_bullet("Environment: ", env_name(x$env))
}


# Helpers ----------------------------------------------------------------------

#' Get an environment's name
#'
#' @param x An environment
#'
#' @return A character vector
#' @noRd
env_name <- function(x) {
  stopifnot(is.environment(x))
  env_name <- environmentName(x)
  if (env_name == "") {
    env_name <- utils::capture.output(print(x))
    env_name <- regmatches(
      env_name,
      regexec("<environment: (.*)>", text = env_name)
    )

    env_name <- env_name[[1]][2]
  }

  env_name
}
