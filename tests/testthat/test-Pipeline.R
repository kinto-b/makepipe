
# Invalidation -----------------------------------------------------------------

test_that("Nodes correctly categorised as out-of-date", {
  source <- withr::local_tempfile(fileext = ".R")
  writeLines("2+2", source)

  # Simple pipeline:
  # dependency -> source -> target (out-of-date)
  withr::with_tempfile(c("dependency", "target"), code = {
    set_pipeline(Pipeline$new())
    saveRDS(iris, target)
    Sys.sleep(0.1)
    saveRDS(iris, dependency)

    make_with_source(source, target, dependency, quiet = TRUE)

    nodes <- get_pipeline()$nodes
    expect(
      all(nodes[nodes$id %in% target, "group"] %in% "Out-of-date"),
      "`target` should be out of date but isn't"
    )
  })

  # multi-stage pipeline:
  # dependency1 -> source -> target1 (out-of-date) --> source + dependency2 --> target2
  withr::with_tempfile(c("dependency1", "dependency2", "target1", "target2"), code = {
    set_pipeline(Pipeline$new())
    saveRDS(iris, dependency2)
    saveRDS(iris, target1)
    saveRDS(iris, target2)
    Sys.sleep(0.1)
    saveRDS(iris, dependency1)

    make_with_source(source, target1, dependency1, quiet = TRUE)
    make_with_source(source, target2, c(target1, dependency2), quiet = TRUE)

    nodes <- get_pipeline()$nodes
    expect(
      all(nodes[nodes$id %in% c(target1, target2), "group"] %in% "Out-of-date"),
      "`target` should be out of date but isn't"
    )
  })

  # multi-stage pipeline:
  # dependency1 -> source -> target1 (out-of-date) --> recipe + dependency2 --> target2
  withr::with_tempfile(c("dependency1", "dependency2", "target1", "target2"), code = {
    set_pipeline(Pipeline$new())
    saveRDS(iris, dependency2)
    saveRDS(iris, target1)
    saveRDS(iris, target2)
    Sys.sleep(0.1)
    saveRDS(iris, dependency1)

    make_with_source(source, target1, dependency1, quiet = TRUE)
    make_with_recipe(NULL, target2, c(target1, dependency2), quiet = TRUE)

    nodes <- get_pipeline()$nodes
    expect(
      all(nodes[nodes$id %in% c(target1, target2), "group"] %in% "Out-of-date"),
      "`target` should be out of date but isn't"
    )
  })
})

test_that("source files not invalidated", {
  # Source older than dependencies
  withr::with_tempfile(c("dependency", "target"), code = {
    set_pipeline(Pipeline$new())
    saveRDS(iris, dependency)
    Sys.sleep(0.1)
    source <- withr::local_tempfile(fileext = ".R")
    writeLines("2+2", source)
    Sys.sleep(0.1)
    saveRDS(iris, target)

    make_with_source(source, target, dependency, quiet = TRUE)

    nodes <- get_pipeline()$nodes

    expect(
      all(nodes[nodes$id %in% target, "group"] %in% "Up-to-date"),
      "`target` should be up to date but isn't"
    )
  })
})



# Annotations ------------------------------------------------------------------
withr::with_tempfile(c("dependency", "target"), code = {
  saveRDS(iris, target)
  saveRDS(iris, dependency)

  set_pipeline(Pipeline$new())
  make_with_recipe(
    targets = target,
    dependencies = dependency,
    recipe = {2+2},
    quiet = TRUE
  )
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

  test_that("annotations are stored correctly in nodes", {
    annotation1 <- "annotation1"
    names(annotation1) <- dependency

    annotation2 <- "annotation2"
    names(annotation2) <- target

    x <- annotate_pipeline(
      pipe,
      tooltips = annotation1,
      labels = annotation2
    )
    x <- x$nodes

    expect_equal(
      x[x$id %in% dependency, "title"],
      "annotation1"
    )

    expect_equal(
      x[x$id %in% target, "label"],
      "annotation2"
    )
  })
})


