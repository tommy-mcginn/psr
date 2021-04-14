#' @title Standard Ten (STEN) Scores for a set of athlete measurements
#'
#' @description Converts each score of each metric passed to the function into its corresponding STEN score. The STEN score converts
#'   metrics that have different units to the same scale, with a minimum value of 1 and a maximum value of 10 for all metrics. This
#'   allows one to easily compare an athlete's scores for different metrics that are measured in different units, which enables one
#'   to identify which metrics are areas of strength and which are areas of weakness for each athlete. The STEN method fulfills the
#'   same purpose as standardizing the scores (i.e. converting all of the scores to z-scores), but might be easier for practitioners
#'   and coaches to understand.
#'
#' @param subject The vector of athletes who recorded the results for each metric (can be a numeric or factor variable)
#' @param trial The vector that represents which trial each measurement came from
#' @param ... Numeric vectors that represent the metrics which scores should be computed to STEN scores. These vectors hold the
#'   scores that each athlete recorded for each respective metric (at least one metric must be passed to the function).
#'
#' @return A data frame, with the subjects as rows and the metrics as columns, and each entry representing the original measurement
#'   of the given metric having been converted to the unit-less STEN score.
#'
#' @examples
#' subject <- c(1, 1, 1, 2, 2, 2, 3, 3, 3)
#' trial <- c(1, 2, 3, 1, 2, 3, 1, 2, 3)
#' metric_1 <- c(250, 258, 252, 279, 270, 277, 218, 213, 218)
#' metric_2 <- c(10, 7, 10, 14, 18, 17, 11, 7, 8)
#' metric_3 <- c(1214, 1276, 1289, 1037, 1010, 1069, 1481, 1465, 1443)
#' STEN(subject, trial, metric_1, metric_2, metric_3)
#'
#' @references Glen, S. (2015). Stephanie Glen. "STEN Score" From StatisticsHowTo.com: Elementary Statistics for the rest of us!
#'   https://www.statisticshowto.com/sten-score/
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
    STEN <- round((((metric - mean(metric)) / stats::sd(metric)) * 2) + 5.5, digits = 2)

    # These scores for each metric are added to the second data frame created, and named according to the metric they represent
    output_df <- cbind(output_df, unlist(STEN))
    colnames(output_df)[i] <- colnames(full_df)[i]

  }

  # I print the data frame in this way as the output so that I can hide the "1" that otherwise appears as the row number
  print.data.frame(output_df, row.names = FALSE)

}
