test_that("text_summary looks as expected", {
  reset_pipeline()
  p <- make_with_dir(system.file("tests", "Segment", package = "makepipe"), build = FALSE)

  ts_actual <- capture.output(p$text_summary())
  ts_expected <-
    c(
      "# Pipeline",
      "",
      "## One",
      "",
      "The first script in a pipeline. This one doesn't have any",
      " dependencies.",
      "",
      "* Source: '.*'",
      "* Targets: 'one.txt'",
      "* Executed: FALSE",
      "* Environment: .*",
      "",
      "## Two",
      "",
      "The second script in a pipeline",
      "",
      "* Source: '.*'",
      "* Targets: 'two.txt'",
      "* File dependencies: 'one.txt'",
      "* Executed: FALSE",
      "* Environment: .*"
    )


  expect_identical(ts_actual[1:7], ts_expected[1:7])
  expect_match(ts_actual[8], ts_expected[8])
  expect_identical(ts_actual[9:10], ts_expected[9:10])
  expect_match(ts_actual[11], ts_expected[11])

  expect_identical(ts_actual[12:16], ts_expected[12:16])
  expect_match(ts_actual[17], ts_expected[17])
  expect_identical(ts_actual[18:20], ts_expected[18:20])
  expect_match(ts_actual[21], ts_expected[21])
})
