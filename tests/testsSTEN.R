library(psr)

subject_1 <- c(1, 1, 1, 2, 2, 2, 3, 3, 3)
trial_1 <- c(1, 2, 3, 1, 2, 3, 1, 2, 3)
metric_1 <- c(0, 25, 75, 100, 50, 12.5, 87.5, 62.5, 37.5)

subject_2 <- c(1, 1, 1, 2, 2, 2, 3, 3, 3)
trial_2 <- c(1, 2, 3, 1, 2, 3, 1, 2, 3)
metric_2 <- c(1, 2, 3, 4, 5.5, 7, 8, 9, 10)

testthat::test_that("STEN", {
  testthat::expect_equal(round(STEN(subject_1, trial_1, metric_1)[5, 3], 2), 5.50)
})

testthat::test_that("STEN", {
  testthat::expect_equal(round(STEN(subject_2, trial_2, metric_2)[5, 3], 2), 5.50)
})
