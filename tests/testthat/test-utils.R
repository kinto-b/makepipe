test_that("robust_deparse handles !! nicely", {
  x <- robust_deparse(quote(`$`(a, !!b) + `$`(a, b)))
  expect_identical(
    x,
    "`$`(a, !!b) + a$b"
  )
})

test_that("robust_deparse mirrors deparse otherwise", {
  x <- quote({
    b <- c(0, 9, 6.2, 10.1)
    XtXc <- diag(c(0, 1/100, 1/85, 1/90))
    n <- 100+65+90
    s2 <- 110.15
  })

  expect_identical(
    robust_deparse(x),
    paste(deparse(x), collapse = "\n")
  )
})
