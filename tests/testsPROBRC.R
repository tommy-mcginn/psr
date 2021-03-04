library(psr)

subject <- c(1, 1, 2, 2, 3, 3)
trial <- c(1, 2, 1, 2, 1, 2)
metric_1 <- c(1.94, 2.44, 1.68, 2.46, 3.17, 3.05)

testthat::test_that("PROBRC", {
  testthat::expect_equal(round(psr::PROBRC(subject, trial, metric_1, ICC = 0.94, initial = which(trial == 1),
                                           final = which(trial == 2))[4, 2], )
})


