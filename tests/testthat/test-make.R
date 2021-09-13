test_that("make_with_source", {
  source <- withr::local_tempfile(fileext = ".R")
  writeLines("2+2", source)

  # Target older than dependencies
  withr::with_tempfile(c("dependency", "target"), code = {
    saveRDS(iris, target)
    Sys.sleep(0.1)
    saveRDS(iris, dependency)

    expect_condition(
      expect_condition(
        make_with_source(source, target, dependency),
        regexp = "Targets are out of date. Updating..."
      ),
      regexp = "Finished updating"
    )
  })

  # Target newer than dependencies
  withr::with_tempfile(c("dependency", "target"), {
    saveRDS(iris, dependency)
    Sys.sleep(0.1)
    saveRDS(iris, target)

    expect_condition(
      make_with_source(source, target, dependency),
      regexp = "Targets are up to date"
    )
  })

  # Target doesn't exist
  withr::with_tempfile(c("dependency"), {
    saveRDS(iris, dependency)

    expect_condition(
      expect_condition(
        make_with_source(source, "target.Rds", dependency),
        regexp = "Targets are out of date. Updating..."
      ),
      regexp = "Finished updating"
    )
  })

  # Dependency doesn't exist
  withr::with_tempfile(c("target"), {
    saveRDS(iris, target)

    expect_error(make_with_source(
      source, target, "dependency.Rds"
    ))
  })

  # Source doesn't exist
  withr::with_tempfile(c("dependency", "target"), {
    saveRDS(iris, dependency)
    Sys.sleep(0.1)
    saveRDS(iris, target)

    expect_error(make_with_source(
      "script2", target, dependency
    ))
  })
})

test_that("make_with_recipe", {
  # Target older than dependencies
  withr::with_tempfile(c("dependency", "target"), {
    saveRDS(iris, target)
    Sys.sleep(0.1)
    saveRDS(iris, dependency)

    expect_condition(
      expect_condition(
        make_with_recipe(2 + 2, target, dependency),
        regexp = "Targets are out of date. Updating..."
      ),
      regexp = "Finished updating"
    )
  })

  # Target newer than dependencies
  withr::with_tempfile(c("dependency", "target"), {
    saveRDS(iris, dependency)
    Sys.sleep(0.1)
    saveRDS(iris, target)

    expect_condition(
      make_with_recipe(2 + 2, target, dependency),
      regexp = "Targets are up to date"
    )
  })

  # Target doesn't exist
  withr::with_tempfile(c("dependency"), {
    saveRDS(iris, dependency)

    expect_condition(
      expect_condition(
        make_with_recipe(2 + 2, "target.Rds", dependency),
        regexp = "Targets are out of date. Updating..."
      ),
      regexp = "Finished updating"
    )
  })

  # Dependency doesn't exist
  withr::with_tempfile(c("target"), {
    saveRDS(iris, target)

    expect_error(make_with_recipe(
      2 + 2, target, "dependency.Rds"
    ))
  })
})
