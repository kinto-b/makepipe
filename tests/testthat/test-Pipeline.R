test_that("Nodes correctly categorised as out-of-date", {
  source <- withr::local_tempfile(fileext = ".R")
  writeLines("2+2", source)

  # Simple pipeline:
  # dependency -> source -> target (out-of-date)
  withr::with_tempfile(c("dependency", "target"), code = {
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

