library(psr)

subject_1 <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24)
trial_1 <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
metric_1 <- c(5.31, 5.00, 6.62, 4.56, 4.79, 5.17, 5.35, 5.23, 4.91, 4.86, 5.65, 5.01, 4.60, 5.06, 4.78, 4.85, 4.88, 4.95, 4.56, 5.28,
              5.73, 5.58, 5.17, 5.21)

subject_2 <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15)
trial_2 <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
metric_2 <- c(5.25, 5.24, 5.52, 5.63, 5.39, 5.39, 5.16, 5.5, 5.07, 5.32, 5.41, 5.25, 5.16, 5.31, 5.14)

testthat::test_that("SWC", {
  testthat::expect_equal(round(SWC(subject_1, trial_1, metric_1, effect_size = 0.2, method = 'AVG')[1, 1], 2), 0.09)
})

testthat::test_that("SWC", {
  testthat::expect_equal(round(SWC(subject_2, trial_2, metric_2, effect_size = 0.2, method = 'AVG')[1, 1], 2), 0.03)
})
