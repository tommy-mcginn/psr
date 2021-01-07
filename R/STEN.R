#' @title Standard Ten (STEN) Scores for a set of athlete measurements
#'
#' @description Converts each score of each metric passed to the function into its corresponding STEN score, which is a way of
#'   putting scores that have different units on the same scale, namely that which has a minimum value of 1 and a maximum value of 10
#'   for all metrics. This serves the main purpose of allowing for easy comparison of an athlete's scores for different metrics that
#'   might be measured in different units (so that it can be identified which metrics are areas of strength and which are areas of
#'   weakness for the athlete). The STEN method fulfills the same purpose as standardizing the scores (i.e. converting all of the
#'   scores to z-scores), but has the advantage of potentially being easier to understand for some practitioners and coaches.
#'
#' @param subject The vector of athletes who recorded the results for each metric (can be a numeric or factor variable)
#' @param ... Numeric vectors that represent the metrics which scores should be computed to STEN scores. These vectors hold the
#'   scores that each athlete recorded for each respective metric (at least one metric must be passed to the function).
#'
#' @return The measurements of the metrics are converted from their respective original units of measurement to the unitless STEN
#'   score that the measurement represents.
#'
#' @example
#' subject <- c(1, 1, 1, 2, 2, 2, 3, 3, 3)
#' trial <- c('Trial 1', 'Trial 2', 'Trial 3', 'Trial 1', 'Trial 2', 'Trial 3', 'Trial 1', 'Trial 2', 'Trial 3')
#' metric_1 <- c(257, 268, 237, 275, 259, 263, 216, 287, 250)
#' metric_2 <- c(1.11, 1.24, 0.89, 1.37, 1.21, 1.30, 0.75, 1.42, 1.15)
#' metric_3 <- c(1272, 1493, 1072, 1046, 1198, 1165, 1478, 1370, 1335)
#' STEN(subject, trial, metric_1, metric_2, metric_3)
#'
#' @export
STEN <- function(subject, trial, ...) {

  # The inputs to this function are individual vectors, so I bring them all together into one data frame
  full_df <- data.frame(subject, trial, ...)

  # Calls the check_error function, which produces informative error messages if any of a variety of errors are made by the user
  check_error(subject, trial, ...)

  # A new data frame is created that will store the STEN values of each metric and will be the function output
  output_df <- data.frame(subject, trial)

  # Iterates over all of the arguments that correspond to the metrics passed to the function (all but the first "subject" argument)
  for (i in 3:nargs()) {

    # Each metric is the ith column of the first data frame created (ensures we only affect the metrics, not the subject)
    metric <- full_df[, i]

    # The formula to convert the actual scores to the STEN scores is applied here, with the results rounded to 2 decimals
    STEN <- round((((metric - mean(metric)) / sd(metric)) * 2) + 5.5, digits = 2)

    # These scores for each metric are added to the second data frame created, and named according to the metric they represent
    output_df <- cbind(output_df, unlist(STEN))
    colnames(output_df)[i] <- colnames(full_df)[i]

  }

  # I print the data frame in this way as the output so that I can hide the "1" that otherwise appears as the row number
  print.data.frame(output_df, row.names = FALSE)

}
