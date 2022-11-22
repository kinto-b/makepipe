
# Tempfiles --------------------------------------------------------------------
target1 <- tempfile(fileext=".csv")
target2 <- tempfile(fileext=".txt")
target3 <- tempfile(fileext=".txt")

# Success tests ----------------------------------------------------------------
test_that("make_with_dir ignores scripts without @makepipe", {
  p <- make_with_dir(
    system.file("tests", "make_with_dir_ok", package = "makepipe"),
    build=FALSE
  )

  # redundant has been ignored
  expect_length(p$segments, 2)

  # Targets have been evaluated
  expect_equal(
    p$segments[[1]]$targets,
    target1
  )
  expect_equal(
    p$segments[[2]]$targets,
    target2
  )
})

test_that("make_with_dir evaluates tags using parent environment", {
  p <- make_with_dir(
    system.file("tests", "make_with_dir_ok", package = "makepipe"),
    build=FALSE
  )

  expect_equal(
    p$segments[[1]]$targets,
    target1
  )

  expect_equal(
    p$segments[[2]]$targets,
    target2
  )

  expect_error(
    make_with_dir(
      system.file("tests", "make_with_dir_ok", package = "makepipe"),
      build=FALSE, envir = new.env(parent=emptyenv()) # Doesn't have target1 etc
    )
  )
})

test_that("make_with_dir finds scripts in subdir if told to", {
  p <- make_with_dir(
    system.file("tests", "make_with_dir_ok", package = "makepipe"),
    build=FALSE, recursive = TRUE
  )

  # source3 has been ignored
  expect_length(p$segments, 3)

  # Targets have been evaluated
  expect_equal(
    p$segments[[3]]$targets,
    target3
  )
})

# Failure tests ----------------------------------------------------------------

test_that("make_with_dir warns if more than one @makepipe tag", {
  expect_warning(make_with_dir(
    system.file("tests", "make_with_dir_warn", package = "makepipe"),
    build=FALSE
  ), "More than one")
})


# Unlink -----------------------------------------------------------------------
unlink(c("target1", "target2", "target3"))
