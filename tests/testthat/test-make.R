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
      mt <- read.csv(dependency, check.names = FALSE)
      write.csv(mt, target1, row.names = FALSE)
    }, target1, dependency)
  ))
})

test_that("targets not rebuilt if newer than dependency", {
  order_filetimes(dependency, target1)
  expect_uptodate(quote(
    make_with_recipe({
      mt <- read.csv(dependency, check.names = FALSE)
      write.csv(mt, target1, row.names = FALSE)
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


test_that("error thrown if make_*() contains loops", {
  expect_error(
    make_with_recipe(1+1, target1, target1),
    "`dependencies` must not be among the `targets`"
  )

  expect_error(
    make_with_source(source1, target1, target1),
    "`dependencies` must not be among the `targets`"
  )

  expect_error(
    make_with_source(source1, source1, target1),
    "`source` must not be among the `targets`"
  )
})

## make_with_recipe ------------------------------------------------------------
test_that("error thrown if dependency doesn't exist", {
  expect_error(make_with_recipe(
    {1+1},
    target1,
    "filedoesntexist.Rds"
  ))
})

## Environment -----------------------------------------------------------------

test_that("by default source eval'd in fresh env whose parent is calling env", {
  source <- withr::local_tempfile(fileext = ".R")
  writeLines(".res <- object_from_parent_environment*2", source)
  expect_error(make_with_source(source, target1, dependency, quiet = TRUE))

  object_from_parent_environment <- 5
  make_with_source(source, target1, dependency, quiet = TRUE)
  expect_error(.res, regexp = "object '.res' not found")
})

test_that("by default recipe eval'd in fresh env whose parent is calling env", {
  order_filetimes(target1, dependency)

  expect_error(make_with_recipe({
    .res <- object_from_parent_environment*2
  }, target1, dependency, quiet = TRUE))

  object_from_parent_environment <- 5
  make_with_recipe({
    .res <- object_from_parent_environment*2
  }, target1, dependency, quiet = TRUE)
  expect_error(.res, regexp = "object '.res' not found")
})

test_that("obj available when eval source in calling env", {
  source <- withr::local_tempfile(fileext = ".R")
  object_from_parent_environment <- 5
  writeLines(".res <- object_from_parent_environment*2", source)
  make_with_source(source, target1, dependency, envir = environment(), quiet = TRUE)
  expect_identical(.res, object_from_parent_environment*2)
})

test_that("source evaluated in supplied environment", {
  source <- withr::local_tempfile(fileext = ".R")
  writeLines(".res <- object_from_parent_environment*2", source)

  object_from_parent_environment <- 5 # Decoy

  my_env <- new.env()
  my_env$object_from_parent_environment <- 500

  make_with_source(source, target1, dependency, envir = my_env, quiet = TRUE)
  expect_equal(my_env$.res, 1000)
})

test_that("recipe evaluated in supplied environment", {
  order_filetimes(target1, dependency)

  object_from_parent_environment <- 5 # Decoy

  my_env <- new.env()
  my_env$object_from_parent_environment <- 500

  x <- make_with_recipe({
    .res <- object_from_parent_environment*2
    .res
  }, target1, dependency, envir = my_env, quiet = TRUE)

  expect_equal(my_env$.res, 1000)
  expect_equal(x$result, 1000)
})

# Returns ----------------------------------------------------------------------

test_that("make_with_recipe returns what's returned", {
  order_filetimes(target1, dependency)
  x <- make_with_recipe({
    res <- 1+1
    res
  }, target1, dependency, quiet = TRUE)

  expect_equal(x$result, 2)

  order_filetimes(target1, dependency)
  x <- make_with_recipe({
    res <- 1+1
    if (res == 2) return(5)
    res
  }, target1, dependency, quiet = TRUE)

  expect_equal(x$result, 5)
})

test_that("make_with_source returns what's registered", {
  order_filetimes(dependency, target1, source1)
  res <- make_with_source(source1, target1, dependency, quiet = TRUE)

  expect_equal(res$result$five, 5)
})


# Printing ---------------------------------------------------------------------
test_that("make_with_recipe result prints nicely", {
  order_filetimes(target1, dependency)
  x <- make_with_recipe({
    res <- 1+1
    res
  }, target1, dependency, quiet = TRUE)

  expect_output(print(x), regexp = "# makepipe segment")
  expect_output(print(x), regexp = "* Recipe: ")
  expect_output(print(x), regexp = "* Targets: '.*'")
  expect_output(print(x), regexp = "* File dependencies: '.*'")
  expect_output(print(x), regexp = "* Executed: TRUE")
  expect_output(print(x), regexp = "* Result: 1 object")
  expect_output(print(x), regexp = "* Environment: ")

  order_filetimes(dependency, target1)

  x <- make_with_recipe({
    res <- 1+1
    res
  }, target1, dependency, quiet = TRUE)
  x
  expect_output(print(x), regexp = "* Executed: FALSE")
})

test_that("make_with_source result prints nicely", {
  order_filetimes(dependency, target1, source1)
  x <- make_with_source(source1, target1, dependency, quiet = TRUE)

  expect_output(print(x), regexp = "# makepipe segment")
  expect_output(print(x), regexp = "* Source: '.*'")
  expect_output(print(x), regexp = "* Targets: '.*'")
  expect_output(print(x), regexp = "* File dependencies: '.*'")
  expect_output(print(x), regexp = "* Executed: TRUE")
  expect_output(print(x), regexp = "* Result: 1 object")
  expect_output(print(x), regexp = "* Environment: ")

  order_filetimes(dependency, source1, target1)
  x <- make_with_source(source1, target1, dependency, quiet = TRUE)
  expect_output(print(x), regexp = "* Executed: FALSE")
})


# Unlink ------------------------------------------------------------------

unlink(c("dependency", "target1", "target2", "source1", "source2"))


