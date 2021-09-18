# Test files -------------------------------------------------------------------
dependency <- system.file("tests", "mtcars.Rds", package = "makepipe")
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

expect_outofdate <- function(expr) {
  expect_condition(
    expect_condition(
      eval(expr),
      regexp = "Targets are out of date. Updating..."
    ),
    regexp = "Finished updating"
  )
}

expect_uptodate <- function(expr) {
  expect_condition(
    eval(expr),
    regexp = "Targets are up to date"
  )
}

# Out-of-dateness --------------------------------------------------------------
## make_with_source ------------------------------------------------------------
test_that("targets rebuilt if older than source", {
  order_filetimes(dependency, target1, source1)
  expect_outofdate(quote(make_with_source(source1, target1, dependency)))
})

test_that("targets rebuilt if older than dependency", {
  order_filetimes(source1, target1, dependency)
  expect_outofdate(quote(make_with_source(source1, target1, dependency)))
})

test_that("targets rebuilt if older than dependency and source", {
  order_filetimes(target1, source1, dependency)
  expect_outofdate(quote(make_with_source(source1, target1, dependency)))
})

test_that("targets rebuilt if non-existent", {
  expect_outofdate(quote(make_with_source(
    source1,
    "filedoesntexist.Rds",
    dependency
  )))
})

test_that("targets not rebuilt if newer than dependency and source", {
  order_filetimes(source1, dependency, target1)
  expect_uptodate(quote(make_with_source(source1, target1, dependency)))
})

test_that("targets not rebuilt if newer than package", {
  order_filetimes(source1, dependency, target1)
  expect_uptodate(quote(make_with_source(
    source1,
    target1,
    dependency,
    packages = "base"
  )))
})

## make_with_recipe ------------------------------------------------------------
test_that("targets rebuilt if older than dependency", {
  order_filetimes(target1, dependency)
  expect_outofdate(quote(
    make_with_recipe({
      mt <- readRDS(dependency)
      write.csv(mt, target1)
    }, target1, dependency)
  ))
})

test_that("targets not rebuilt if newer than dependency", {
  order_filetimes(dependency, target1)
  expect_uptodate(quote(
    make_with_recipe({
      mt <- readRDS(dependency)
      write.csv(mt, target1)
    }, target1, dependency)
  ))
})

test_that("targets not rebuilt if newer than package", {
  order_filetimes(source1, dependency, target1)
  expect_uptodate(quote(make_with_recipe(
    {1+1},
    target1,
    dependency,
    packages = "base"
  )))
})

# Errors -----------------------------------------------------------------------
## make_with_source ------------------------------------------------------------
test_that("error thrown if dependency doesn't exist", {
  expect_error(make_with_source(
    source1,
    target1,
    "filedoesntexist.Rds"
  ))
})

test_that("error thrown if source doesn't exist", {
  expect_error(make_with_source(
    "filedoesntexist.Rds",
    target1,
    dependency
  ))
})

## make_with_recipe ------------------------------------------------------------
test_that("error thrown if dependency doesn't exist", {
  expect_error(make_with_recipe(
    {1+1},
    target1,
    "filedoesntexist.Rds"
  ))
})

