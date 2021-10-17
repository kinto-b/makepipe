# Test files -------------------------------------------------------------------
dependency <- system.file("tests", "mtcars_src.csv", package = "makepipe")
source1 <- system.file("tests", "mtcars1.R", package = "makepipe")
source2 <- system.file("tests", "mtcars2.R", package = "makepipe")
target1 <- system.file("tests", "mtcars.csv", package = "makepipe")
target2 <- system.file("tests", "mtcars.txt", package = "makepipe")

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
  nodes <- pipeline$nodes
  expect(
    all(nodes[nodes$id %in% node_id, "group"] %in% "Out-of-date"),
    "`target` should be out of date but isn't"
  )
}

expect_uptodate <- function(node_id, pipeline = get_pipeline()) {
  nodes <- pipeline$nodes
  expect(
    all(nodes[nodes$id %in% node_id, "group"] %in% "Up-to-date"),
    "`target` should be up to date but isn't"
  )
}

# Out-of-dateness --------------------------------------------------------------

## make_with_source ------------------------------------------------------------
test_that("targets rebuilt if older than source", {
  set_pipeline(Pipeline$new())
  order_filetimes(dependency, target1, source1)
  make_with_source(source1, target1, dependency, quiet = TRUE)
  expect_outofdate(target1)
})

test_that("targets rebuilt if older than dependency", {
  set_pipeline(Pipeline$new())
  order_filetimes(source1, target1, dependency)
  make_with_source(source1, target1, dependency, quiet = TRUE)
  expect_outofdate(target1)
})

test_that("targets rebuilt if older than dependency and source", {
  set_pipeline(Pipeline$new())
  order_filetimes(target1, source1, dependency)
  make_with_source(source1, target1, dependency, quiet = TRUE)
  expect_outofdate(target1)
})

test_that("out-of-dateness is passed along", {
  set_pipeline(Pipeline$new())
  order_filetimes(target1, source1, dependency, source2, target2)
  make_with_source(source1, target1, dependency, quiet = TRUE)
  expect_outofdate(target1)
  make_with_source(source2, target2, target1, quiet = TRUE)
  expect_outofdate(target2)
})

test_that("targets not rebuilt if newer than dependency and source", {
  set_pipeline(Pipeline$new())
  order_filetimes(source1, dependency, target1)
  make_with_source(source1, target1, dependency, quiet = TRUE)
  expect_uptodate(target1)
})


test_that("source never out-of-dated", {
  set_pipeline(Pipeline$new())
  order_filetimes(dependency, source1, target1)
  make_with_recipe(source1, target1, dependency, quiet = TRUE)
  expect_uptodate(source1)
})

## make_with_recipe ------------------------------------------------------------
test_that("targets rebuilt if older than dependency", {
  set_pipeline(Pipeline$new())
  order_filetimes(target1, dependency)
  make_with_recipe({
    mt <- read.csv(dependency, check.names = FALSE)
    write.csv(mt, target1, row.names = FALSE)
  }, target1, dependency, quiet = TRUE)
  expect_outofdate(target1)
})

test_that("out-of-dateness is passed along", {
  set_pipeline(Pipeline$new())

  order_filetimes(target1, source1, dependency, source2, target2)
  make_with_recipe({
    mt <- read.csv(dependency, check.names = FALSE)
    write.csv(mt, target1, row.names = FALSE)
  }, target1, dependency, quiet = TRUE)
  expect_outofdate(target1)

  make_with_recipe({
    mt <- read.csv(target1)
    write.table(mt, target2, sep = "|")
  }, target2, target1, quiet = TRUE)
  expect_outofdate(target2)
})


# show_pipeline ----------------------------------------------------------------

test_that("out-of-dateness is kept up-to-date", {
  set_pipeline(Pipeline$new())
  order_filetimes(target1, source1, dependency, source2, target2)
  make_with_source(source1, target1, dependency, quiet = TRUE)
  make_with_source(source2, target2, target1, quiet = TRUE)

  order_filetimes(target1, source1, dependency, source2, target2)
  show_pipeline()
  expect_outofdate(target1)
})

# Annotations ------------------------------------------------------------------
set_pipeline(Pipeline$new())
order_filetimes(dependency, target1, source1)
make_with_source(source1, target1, dependency, quiet = TRUE)
pipe <- get_pipeline()

test_that("duplicate annotations are disallowed", {
  annotation <- c("one", "two")
  names(annotation) <- rep(dependency, 2)

  expect_error(annotate_pipeline(
    pipe,
    tooltips = annotation,
    labels = NULL
  ), regexp = "must not be duplicated")

  expect_error(annotate_pipeline(
    pipe,
    labels = annotation,
    tooltips = NULL
  ), regexp = "must not be duplicated")
})

# Non-existent
test_that("annotations cannot be applied to nodes that don't exist", {
  expect_error(annotate_pipeline(
    pipe,
    tooltips = c("R/aaa.R" = "input"),
    labels = NULL
  ), regexp = "not nodes in `pipeline`")
})

# Non-character
test_that("annotations cannot must be character", {
  tooltips <- c(1)
  names(tooltips) <- dependency

  expect_error(annotate_pipeline(
    pipe,
    tooltips = tooltips,
    labels = NULL
  ), regexp = "is.character")
})


