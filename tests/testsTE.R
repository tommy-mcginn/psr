context("Check psr functions")

subject_1 <- c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5)
trial_1 <- c(1, 2, 1, 2, 1, 2, 1, 2, 1, 2)
metric_1 <- c(62, 67, 78, 76, 81, 87, 55, 55, 66, 63)

subject_2 <- c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8, 9, 9, 10, 10)
trial_2 <- c(1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2)
metric_2 <- c(57.5, 57.4, 65.6, 63.2, 67.0, 66.5, 68.5, 69.9, 70.8, 72.8, 72.2, 70.1, 74.9, 75.6, 76.0, 75.2, 76.1, 72.8, 83.1, 79.0)

test_that("TE", {
  expect_equal(round(TE(subject_1, trial_1, metric_1), 1)[1, 1], 2.9)
})

test_that("TE", {
  expect_equal(round(TE(subject_2, trial_2, metric_2), 1)[1, 1], 1.4)
})
