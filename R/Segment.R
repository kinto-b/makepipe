
# Segment class ----------------------------------------------------------------
#' Segment
#'
#' @description A Segment object is automatically constructed and attached to
#'   the Pipeline when a call to `make_*()` is made. It stores the relationships
#'   between targets, dependencies, and sources.
#'
#' @keywords internal
#' @family segment
#' @export Segment
#' @aliases Segment
#' @importFrom R6 R6Class
Segment <- R6::R6Class("Segment",
  private = list(
    id = NULL,
    instructions_txt = NULL,
    result_txt = NULL
  ),
  public = list(
    #' @field targets A character vector of paths to files
    targets = NULL,
    #' @field dependencies A character vector of paths to files which the `targets`
    #'   depend on
    dependencies = NULL,
    #' @field packages A character vector of names of packages which `targets`
    #'   depend on
    packages = NULL,
    #' @field envir The environment in which to execute the instructions.
    envir = NULL,
    #' @field result An object, whatever is returned by executing the instructions
    result = NULL,
    #' @field executed A logical, whether or not the instructions were executed
    executed = FALSE,
    #' @field execution_time A difftime, the time taken to execute the instructions
    execution_time = NULL,

    #' @description Initialise a new Segment
    #' @param id An integer that uniquely identifies the segment
    #' @param targets A character vector of paths to files
    #' @param dependencies A character vector of paths to files which the `targets`
    #'   depend on
    #' @param packages A character vector of names of packages which `targets`
    #'   depend on
    #' @param envir The environment in which to execute the instructions.
    #' @param result An object, whatever is returned by executing the instructions
    #' @param executed A logical, whether or not the instructions were executed
    #' @param execution_time A difftime, the time taken to execute the instructions
    initialize = function(id, targets, dependencies, packages, envir,
                          executed, result, execution_time) {
      stopifnot("`id` must be numeric" = is.numeric(id))
      stopifnot("`targets` must be character" = is.character(targets))
      stopifnot("`dependencies` must be character" = is.character(dependencies))
      stopifnot("`packages` must be character" = is.character(packages) | is.null(packages))
      stopifnot("`envir` must be an environment" = is.environment(envir))
      stopifnot("`executed` must be logical" = is.logical(executed))
      stopifnot("`execution_time` must be difftime" = inherits(execution_time, "difftime") | is.null(execution_time))
      find.package(packages) # Error if package cannot be found

      miss_deps <- !file.exists(dependencies)
      if (any(miss_deps)) {
        stop('One or more `dependencies` do not exist: ', dependencies[miss_deps])
      }

      if (any(targets %in% dependencies)) {
        stop("`dependencies` must not be among the `targets`", call. = FALSE)
      }

      targets <- unique(targets)
      dependencies <- unique(dependencies)
      packages <- unique(packages)

      private$id <- as.integer(id)

      self$targets <- targets
      self$dependencies <- dependencies
      self$packages <- packages
      self$envir <- rlang::env_clone(envir)
      self$executed <- executed
      self$result <- result
      self$execution_time <- execution_time

      invisible(self)
    },

    #' @description Printing method
    print = function() {
      targets <- paste0("'", paste(self$targets, collapse = "', '"), "'")
      dependencies <- paste0("'", paste(self$dependencies, collapse = "', '"), "'")
      if (length(self$packages) > 0) {
        packages <- paste0("'", paste(self$packages, collapse = "', '"), "'")
      }

      cli::cat_line(cli::col_grey("# makepipe segment"))
      cli::cat_bullet(private$instructions_txt)
      cli::cat_bullet("Targets: ", targets)
      cli::cat_bullet("File dependencies: ", dependencies)
      if (length(self$packages) > 0) cli::cat_bullet("Package dependencies: ", packages)
      cli::cat_bullet("Executed: ", self$executed)
      if (self$executed) cli::cat_bullet("Execution time: ", format(self$execution_time))
      if (self$executed) cli::cat_bullet(private$result_txt)
      cli::cat_bullet("Environment: ", tolower(rlang::env_label(self$envir)))
    },

    #' @description Update the Segment with new execution information
    #' @param executed A logical, whether or not the instructions were executed
    #' @param execution_time A difftime, the time taken to execute the instructions
    #' @param result An object, whatever is returned by executing the instructions
    update_result = function(executed, execution_time, result) {
      stopifnot("`executed` must be logical" = is.logical(executed))
      stopifnot("`execution_time` must be difftime" = inherits(execution_time, "difftime") | is.null(execution_time))
      self$executed <- executed
      self$result <- result
      self$execution_time <- execution_time

      invisible(self)
    }
  ),
  active = list(
    #' @field edges Construct edges connecting the dependencies, instructions, and targets
    edges = function() {
      is_recipe <- inherits(self, "SegmentRecipe")
      edges <- rbind(
        new_edge(self$dependencies, private$instructions_txt, TRUE, is_recipe, FALSE),
        new_edge(private$instructions_txt, self$targets, FALSE, FALSE, FALSE)
      )

      if (length(self$packages) > 0) {
        edges <- rbind(
          new_edge(self$packages, private$instructions_txt, TRUE, FALSE, TRUE),
          edges
        )
      }

      edges$id <- self$id

      edges
    }
  )
)

# Recipe subclass --------------------------------------------------------------
#' Segment
#'
#' @description A Segment object is automatically constructed and attached to
#'   the Pipeline when a call to `make_*()` is made. It stores the relationships
#'   between targets, dependencies, and sources.
#'
#' @keywords internal
#' @family segment
#' @export SegmentRecipe
#' @aliases SegmentRecipe
#' @importFrom R6 R6Class
SegmentRecipe <- R6::R6Class("SegmentRecipe",
  inherit = Segment,
  public = list(
    #' @field recipe A chunk of R code which makes the `targets`
    recipe = expression(),

    #' @description Initialise a new Segment
    #' @param id An integer that uniquely identifies the segment
    #' @param recipe A chunk of R code which makes the `targets`
    #' @param targets A character vector of paths to files
    #' @param dependencies A character vector of paths to files which the `targets`
    #'   depend on
    #' @param packages A character vector of names of packages which `targets`
    #'   depend on
    #' @param envir The environment in which to execute the instructions.
    #' @param result An object, whatever is returned by executing the instructions
    #' @param executed A logical, whether or not the instructions were executed
    #' @param execution_time A difftime, the time taken to execute the instructions
    initialize = function(id, recipe, targets, dependencies, packages, envir,
                          executed, result, execution_time) {
      stopifnot("`recipe` must be an expression" = rlang::is_expression(recipe))
      super$initialize(id, targets, dependencies, packages, envir, executed, result, execution_time)

      instructions_txt <- paste(deparse(recipe), collapse = "\n")
      instructions_txt <- paste0("Recipe: \n\n", instructions_txt, "\n")
      result_txt <- ifelse(is.null(result), "Result: 0 object(s)", "Result: 1 object(s)")

      private$instructions_txt <- instructions_txt
      private$result_txt <- result_txt
      self$recipe <- recipe

      invisible(self)
    },

    #' @description Printing method
    print = function() {
      instructions_txt <- paste(deparse(self$recipe), collapse = "\n")
      instructions_txt <- paste0("Recipe: \n\n", instructions_txt, "\n")
      private$instructions_txt <- instructions_txt
      super$print()
    },

    #' @description Update the Segment with new execution information
    #' @param executed A logical, whether or not the instructions were executed
    #' @param execution_time A difftime, the time taken to execute the instructions
    #' @param result An object, whatever is returned by executing the instructions
    update_result = function(executed, execution_time, result) {
      private$result_txt <- ifelse(is.null(result), "Result: 0 object(s)", "Result: 1 object(s)")
      super$update_result(executed, execution_time, result)
    },

    execute = function(envir = NULL, quiet = getOption("makepipe.quiet"), ...) {
      if (!is.null(envir)) {
        stopifnot("`envir` must be an environment" = is.environment(envir))
        self$envir <- envir
      }
      outdated <- out_of_date(self$targets, self$dependencies, self$packages)

      # Prepare fresh execution environment so we don't clutter existing environment
      envir <- rlang::env_clone(self$envir)

      if (outdated) {
        if (!quiet) {
          cli::cli_process_start(
            "Targets are out of date. Updating...",
            msg_done = "Finished updating",
            msg_failed = "Something went wrong"
          )
          cli::cat_line()
        }

        execution_time <- Sys.time()
        out <- eval(self$recipe, envir = envir, ...)
        execution_time <- Sys.time() - execution_time

        if (!quiet) cli::cli_process_done()
      } else {
        execution_time <- NULL
        if (!quiet) cli::cli_alert_success("Targets are up to date")
        out <- NULL
      }

      self$update_result(outdated, execution_time, out)

      invisible(self)
    }
  ),
  active = list(
    #' @field edges Construct edges connecting the dependencies, instructions, and targets
    edges = function() {
      private$instructions_txt <- paste(deparse(self$recipe), collapse = "\n")
      super$edges
    }
  )
)


# Source subclass --------------------------------------------------------------
#' Segment
#'
#' @description A Segment object is automatically constructed and attached to
#'   the Pipeline when a call to `make_*()` is made. It stores the relationships
#'   between targets, dependencies, and sources.
#'
#' @keywords internal
#' @family segment
#' @export SegmentSource
#' @aliases SegmentSource
#' @importFrom R6 R6Class
SegmentSource <- R6::R6Class("SegmentSource",
   inherit = Segment,
   public = list(
     #' @field source The path to an R script which makes the `targets`
     source = character(),

     #' @description Initialise a new Segment
     #' @param id An integer that uniquely identifies the segment
     #' @param source The path to an R script which makes the `targets`
     #' @param targets A character vector of paths to files
     #' @param dependencies A character vector of paths to files which the `targets`
     #'   depend on
     #' @param packages A character vector of names of packages which `targets`
     #'   depend on
     #' @param envir The environment in which to execute the instructions.
     #' @param result An object, whatever is returned by executing the instructions
     #' @param executed A logical, whether or not the instructions were executed
     #' @param execution_time A difftime, the time taken to execute the instructions
     initialize = function(id, source, targets, dependencies, packages, envir,
                           executed, result, execution_time) {
       stopifnot("`source` must be character" = is.character(source))
       stopifnot("`source` does not exist" = file.exists(source))
       if (any(targets %in% source)) {
         stop("`source` must not be among the `targets`", call. = FALSE)
       }

       super$initialize(id, targets, dependencies, packages, envir, executed, result, execution_time)

       private$instructions_txt <- paste0("Source: '", source, "'")
       private$result_txt <- paste0("Result: ", length(result), " object(s)")
       self$source <- source

       invisible(self)
     },

     #' @description Printing method
     print = function() {
       private$instructions_txt <- paste0("Source: '", self$source, "'")
       super$print()
     },

     #' @description Update the Segment with new execution information
     #' @param executed A logical, whether or not the instructions were executed
     #' @param execution_time A difftime, the time taken to execute the instructions
     #' @param result An object, whatever is returned by executing the instructions
     update_result = function(executed, execution_time, result) {
       private$result_txt <- paste0("Result: ", length(result), " object(s)")
       super$update_result(executed, execution_time, result)
     },

     execute = function(envir = NULL, quiet = getOption("makepipe.quiet"), ...) {
       if (!is.null(envir)) {
         stopifnot("`envir` must be an environment" = is.environment(envir))
         self$envir <- envir
       }
       outdated <- out_of_date(self$targets, c(self$dependencies, self$source), self$packages)

       # Prepare fresh execution environment so we don't clutter existing environment
       envir <- rlang::env_clone(self$envir)
       register_env <- new.env(parent = emptyenv())
       assign("__makepipe_register__", register_env, envir)

       if (outdated) {
         if (!quiet) {
           cli::cli_process_start(
             "Targets are out of date. Updating...",
             msg_done = "Finished updating",
             msg_failed = "Something went wrong"
           )
           cli::cat_line()
         }

         execution_time <- Sys.time()
         source(self$source, local = envir, ...)
         execution_time <- Sys.time() - execution_time

         if (!quiet) cli::cli_process_done()
       } else {
         execution_time <- NULL
         if (!quiet) cli::cli_alert_success("Targets are up to date")
       }

       self$update_result(outdated, execution_time, register_env)

       invisible(self)
     }
   ),
   active = list(
     #' @field edges Construct edges connecting the dependencies, instructions, and targets
     edges = function() {
       private$instructions_txt <- self$source
       super$edges
     }
   )
)


# Internal ---------------------------------------------------------------------

#' @noRd
new_edge <- function(from, to, .source, .recipe, .pkg) {
  lvls <- levels(factor(c(from, to)))
  expand.grid(
    from = factor(from, lvls),
    to = factor(to, lvls),
    arrows = "to",
    .source = .source,
    .recipe = .recipe,
    .pkg = .pkg,
    .outdated = FALSE,
    stringsAsFactors = FALSE
  )
}
