# Test files -------------------------------------------------------------------
dependency <- tempfile(fileext=".csv")
write.csv(mtcars, dependency)

target1 <- tempfile(fileext=".csv")
write.csv(mtcars, target1)

target2 <- tempfile(fileext=".txt")
write.table(mtcars, target2)

source1 <- tempfile(fileext=".R")
file.copy(system.file("tests", "source1.R", package = "makepipe"), source1)

source2 <- tempfile(fileext=".R")
file.copy(system.file("tests", "source2.R", package = "makepipe"), source2)


# Test Pipeline -----------------------------------------------------------
#' Define a Pipeline subclass to expose private fields for testing:

PipelineTest <- R6::R6Class(
  inherit = Pipeline,
  public = list(
    get_nodes = function() {
      private$nodes
    },
    is_outdated = function(node) {
      self$refresh()
      node %in% private$edges[private$edges$.outdated, "to"]
    }
  )
)

# Functions --------------------------------------------------------------------
order_filetimes <- function(...) {
  all_files <- list(...)
  delta <- length(all_files)
  for (i in seq_along(all_files)) {
    Sys.setFileTime(all_files[[i]], Sys.time()-delta)
    delta <- delta - 1
  }
}

expect_outofdate <- function(node_id, pipeline = get_pipeline()) {
  expect(
    all(pipeline$is_outdated(node_id)),
    "`target` should be out of date but isn't"
  )
}

expect_uptodate <- function(node_id, pipeline = get_pipeline()) {
  expect(
    all(!pipeline$is_outdated(node_id)),
    "`target` should be up to date but isn't"
  )
}

# Out-of-dateness --------------------------------------------------------------

## make_with_source ------------------------------------------------------------
test_that("targets rebuilt if older than source", {
  set_pipeline(PipelineTest$new())
  order_filetimes(dependency, target1, source1)
  res <- make_with_source(source1, target1, dependency, quiet = TRUE)
  expect_true(res$executed)
})

test_that("targets rebuilt if older than dependency", {
  set_pipeline(PipelineTest$new())
  order_filetimes(source1, target1, dependency)
  res <- make_with_source(source1, target1, dependency, quiet = TRUE)
  expect_true(res$executed)
})

test_that("targets rebuilt if older than dependency and source", {
  set_pipeline(PipelineTest$new())
  order_filetimes(target1, source1, dependency)
  res <- make_with_source(source1, target1, dependency, quiet = TRUE)
  expect_true(res$executed)
})

test_that("out-of-dateness is passed along", {
  set_pipeline(PipelineTest$new())
  order_filetimes(target1, source1, dependency, source2, target2)
  make_with_source(source1, target1, dependency, quiet = TRUE)
  expect_outofdate(target1)
  make_with_source(source2, target2, target1, quiet = TRUE)
  expect_outofdate(target2)
})

test_that("targets not rebuilt if newer than dependency and source", {
  set_pipeline(PipelineTest$new())
  order_filetimes(source1, dependency, target1)
  res <- make_with_source(source1, target1, dependency, quiet = TRUE)
  expect_false(res$executed)
})

test_that("targets rebuilt if forced", {
  set_pipeline(PipelineTest$new())
  order_filetimes(source1, dependency, target1)
  res <- make_with_source(source1, target1, dependency, quiet = TRUE, force = TRUE)
  expect_true(res$executed)

  set_pipeline(PipelineTest$new())
  order_filetimes(dependency, target1)
  res <- make_with_recipe(2+2, target1, dependency, quiet = TRUE, force = TRUE)
  expect_true(res$executed)
})

test_that("source never out-of-dated", {
  set_pipeline(PipelineTest$new())
  order_filetimes(dependency, source1, target1)
  make_with_recipe(source1, target1, dependency, quiet = TRUE)
  expect_uptodate(source1)
})

## make_with_recipe ------------------------------------------------------------
test_that("targets rebuilt if older than dependency", {
  set_pipeline(PipelineTest$new())
  order_filetimes(target1, dependency)
  res <- make_with_recipe({
    mt <- read.csv(dependency, check.names = FALSE)
    write.csv(mt, target1, row.names = FALSE)
  }, target1, dependency, quiet = TRUE)
  expect_true(res$executed)
})

test_that("out-of-dateness is passed along", {
  set_pipeline(PipelineTest$new())

  make_with_recipe({
    mt <- read.csv(dependency, check.names = FALSE)
    write.csv(mt, target1, row.names = FALSE)
  }, target1, dependency, quiet = TRUE)

  make_with_recipe({
    mt <- read.csv(target1)
    write.table(mt, target2, sep = "|")
  }, target2, target1, quiet = TRUE)

  # target2 is newer than its dependencies but target1 is older than its dependencies.
  # So target2 should be out of date since one of its dependencies (target1) is out of date.
  order_filetimes(target1, source1, dependency, source2, target2)

  expect_outofdate(target1)
  expect_outofdate(target2)
})


# Empty pipelines ---------------------------------------------------------

test_that("warning shown if empty", {
  reset_pipeline()
  p <- get_pipeline()

  expect_warning(p$build())
  expect_warning(p$clean())
  expect_warning(p$nomnoml())
  expect_warning(p$print())
})

# show_pipeline ----------------------------------------------------------------

test_that("out-of-dateness is kept up-to-date", {
  set_pipeline(PipelineTest$new())
  order_filetimes(target1, source1, dependency, source2, target2)
  make_with_source(source1, target1, dependency, quiet = TRUE)
  make_with_source(source2, target2, target1, quiet = TRUE)

  order_filetimes(target1, source1, dependency, source2, target2)
  show_pipeline()
  expect_outofdate(target1)
})


# save_pipeline -----------------------------------------------------------
set_pipeline(PipelineTest$new())
order_filetimes(dependency, target1, source1)
make_with_source(source1, target1, dependency, quiet = TRUE)
pipe <- get_pipeline()

annotation <- c("one", "two", "three")
names(annotation) <- c(dependency, source1, target1)
pipe$annotate(labels = annotation, notes = annotation)


test_that("pipeline can be saved as png", {
  skip_on_ci()
  skip_on_cran()

  temp_png <- tempfile(fileext = ".png")
  save_pipeline(temp_png, pipeline = pipe)
  expect_snapshot_file(temp_png, "pipeline.png")
  unlink(temp_png)
})

test_that("pipeline can be saved as html", {
  temp_html <- tempfile(fileext = ".html")
  save_pipeline(temp_html, pipeline = pipe, as = "visnetwork")
  expect(file.exists(temp_html), failure_message = "HTML didn't save properly")
  unlink(temp_html)
})

# Annotations ------------------------------------------------------------------
set_pipeline(PipelineTest$new())
order_filetimes(dependency, target1, source1)
make_with_source(source1, target1, dependency, quiet = TRUE)
pipe <- get_pipeline()

test_that("annotations aren't overwritten by print method", {
  annotation <- c("one", "two", "three")
  names(annotation) <- c(dependency, source1, target1)
  pipe$annotate(labels = annotation, notes = annotation)

  print(pipe)
  expect_identical(sort(pipe$get_nodes()$note), sort(c("one", "two", "three")))
  expect_identical(sort(pipe$get_nodes()$label), sort(c("one", "two", "three")))

  # Change annotations
  annotation <- c("1", "2", "3")
  names(annotation) <- c(dependency, source1, target1)
  pipe$annotate(labels = annotation, notes = annotation)

  print(pipe)
  expect_identical(sort(pipe$get_nodes()$note), sort(c("1", "2", "3")))
  expect_identical(sort(pipe$get_nodes()$label), sort(c("1", "2", "3")))
})

# Duplicates
test_that("duplicate annotations are disallowed", {
  annotation <- c("one", "two")
  names(annotation) <- rep(dependency, 2)

  expect_error(pipe$annotate(
    notes = annotation,
    labels = NULL
  ), regexp = "must not be duplicated")

  expect_error(pipe$annotate(
    labels = annotation,
    notes = NULL
  ), regexp = "must not be duplicated")
})

# Non-existent
test_that("annotations cannot be applied to nodes that don't exist", {
  expect_error(pipe$annotate(
    notes = c("R/aaa.R" = "input"),
    labels = NULL
  ), regexp = "not nodes in `Pipeline`")
})

# Non-character
test_that("annotations cannot must be character", {
  notes <- c(1)
  names(notes) <- dependency

  expect_error(pipe$annotate(
    notes = notes,
    labels = NULL
  ), regexp = "must be of class character")
})


# Clean and build --------------------------------------------------------------

test_that("cleaning triggers rebuild", {
  set_pipeline(PipelineTest$new())

  make_with_recipe({
    mt <- read.csv(dependency, check.names = FALSE)
    write.csv(mt, target1, row.names = FALSE)
  }, target1, dependency, quiet = TRUE)

  make_with_recipe({
    mt <- read.csv(target1)
    write.table(mt, target2, sep = "|")
  }, target2, target1, quiet = TRUE)

  pipe <- get_pipeline()
  pipe$clean()
  expect_snapshot(pipe$build())
})


# Touch ------------------------------------------------------------------------

test_that("touching removes out-of-dateness", {
  set_pipeline(PipelineTest$new())
  order_filetimes(target1, source1, dependency, source2, target2)
  make_with_source(source1, target1, dependency, quiet = TRUE)
  make_with_source(source2, target2, target1, quiet = TRUE)
  pipe <- get_pipeline()

  expect_outofdate(target1)
  expect_outofdate(target2)

  pipe$touch()
  expect_uptodate(target1)
  expect_uptodate(target1)
})

# Unlink -----------------------------------------------------------------------

unlink(c("dependency", "target1", "target2", "source1", "source2"))
