context("Check psr functions")

subject <- c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5)
trial <- c(1, 2, 1, 2, 1, 2, 1, 2, 1, 2)
metric_1 <- c(62, 67, 78, 76, 81, 87, 55, 55, 66, 63)

test_that("TE", {
  expect_equal(round(TE(subject, trial, metric_1), 1), 2.9)
})

test_that("TE", {
  expect_equal(round(TE(subject, trial, metric_1), 1), 2.9)
})
