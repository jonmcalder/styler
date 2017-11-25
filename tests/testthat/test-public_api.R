context("public API")



test_that("styler can style package", {
  expect_false(all(style_pkg(testthat_file("public-api", "xyzpackage"))))
})

test_that("styler can style directory", {
  expect_false(all(style_dir(testthat_file("public-api", "xyzdir"))))
})

test_that("styler can style files", {
  expect_false(
    style_file(testthat_file("public-api", "xyzfile", "random-script.R"), strict = FALSE)
  )
  expect_false(any(style_file(
    rep(testthat_file("public-api", "xyzfile", "random-script.R"), 2),
    strict = FALSE
  )))
})


test_that("styler does not return error when there is no file to style", {
  expect_error(style_dir(testthat_file("public-api", "xyzemptydir"), strict = FALSE), NA)
})

context("public API - Rmd in style_file()")

test_that("styler can style Rmd file", {
  expect_false(
    style_file(testthat_file("public-api", "xyzfile_rmd", "random.Rmd"), strict = FALSE)
  )
  expect_warning(
    styled <- style_file(testthat_file("public-api", "xyzfile_rmd", "random2.Rmd"), strict = FALSE)
  )
  expect_false(styled)
})

test_that("styler handles malformed Rmd file and invalid R code in chunk", {
  expect_warning(
    style_file(testthat_file("public-api", "xyzfile_rmd", "random3.Rmd"), strict = FALSE)
  )
  expect_warning(
    style_file(testthat_file("public-api", "xyzfile_rmd", "random4.Rmd"), strict = FALSE)
  )
})

context("public API - Rmd in style_dir()")

test_that("styler can style Rmd fie via style_dir()", {
  msg <- capture_messages(
    style_dir(testthat_file("public-api", "xyz-r-and-rmd-dir"),
              filetype = c("R", "Rmd"))
  )
  expect_true(any(grepl("random-script-in-sub-dir.R", msg, fixed = TRUE)))
  expect_true(any(grepl("random-rmd-script.Rmd", msg, fixed = TRUE)))
})
