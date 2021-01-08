#' @title Reliable Change Index (RCI) for a set of athlete measurements
#'
#' @description For each metric passed to the function, vectors indicating the lower and upper bounds of the RCI of that metric are
#'   generated, with the length of each vector equaling the number of athletes passed to the function
#'
#' @param subject The subjects on which the RCI test should be run
#' @param trial The vector that represents which trial each measurement came from
#' @param ... Numeric vectors that represent the metrics for which the RCI should be computed. These vectors hold the
#'   scores that each athlete recorded for each respective metric (at least one metric must be passed to the function).
#' @param ICC A vector of the ICC's for each of the metrics included in the "..." argument. It is suggested that the reliability
#'   measure chosen be the ICC, but it can be any reliability measure, as long as it lies on a 0-1 scale (with higher values
#'   indicating a greater degree of reliability). This vector must contain the same number of elements as the number of metrics that
#'   have been passed to the function in the "..." argument, and the reliability values must appear in the same order as the metrics
#'   appear in the "..." argument.
#' @param baseline The measurements to be considered the "baseline" results for the calculation of the RCI
#' @param confidence The degree of confidence the user wants to have that the lower and upper bounds given can be
#'   interpreted as real change, and not the result of random or measurement error. Set to a default value of 0.95, this
#'   parameter is used to calculate the corresponding critical value from the standard normal distribution that is used to compute
#'   the lower and upper bounds of the RCI.
#'
#' @return A table for each metric with four columns: the athlete, the baseline measurement, the RCI lower bound, and the RCI upper
#'   bound.
#'
#' @example
#' subject <- c(1, 1, 1, 2, 2, 2, 3, 3, 3)
#' trial <- c('Trial 1', 'Trial 2', 'Trial 3', 'Trial 1', 'Trial 2', 'Trial 3', 'Trial 1', 'Trial 2', 'Trial 3')
#' metric_1 <- c(257, 268, 237, 275, 259, 263, 216, 287, 250)
#' metric_2 <- c(1.11, 1.24, 0.89, 1.37, 1.21, 1.30, 0.75, 1.42, 1.15)
#' metric_3 <- c(1272, 1493, 1072, 1046, 1198, 1165, 1478, 1370, 1335)
#' baseline <- which(trial == "Trial 1")
#' RCI(subject, trial, metric_1, metric_2, metric_3, ICC = c(0.92, 0.98, 0.95), baseline, confidence = 0.95)
#'
#' @references Ferger, K., & Büsch, D. (2018). Individual measurement of performance change in sports. Deutsche Zeitschrift Für
#'   Sportmedizin, 2018(02), 45-52.
#'
#' @export
RCI <- function(subject, trial, ..., ICC, baseline, confidence = 0.95) {

  # The individual subject and metric vectors that were passed to this function are brought together into a data frame
  RCI_df <- data.frame(subject, ...)

  # Calls the check_error function, which produces informative error messages if any of a variety of errors are made by the user
  check_error(subject, trial, ...)

  # We care only about the results of the trial we labeled as the "initial trial" in the function arguments
  RCI_df <- RCI_df[c(baseline), ]

  # For each column except that of the subjects in the data, we want to compute the between-athlete sd
  SD_baseline <- lapply(RCI_df[, -1], stats::sd)

  # Converting the ICC to a list (as opposed to a vector) makes the next for loop easier to calculate
  ICC <- as.list(ICC)

  # The confidence level the user entered should be in the form of its corresponding critical value in our computations
  crit_val <- stats::qnorm((1 + confidence) / 2)

  # This makes a list for the MDC values upfront, so that we can append to it in the next for loop
  list_MDC <- list()

  # The sequence along SD_baseline is really just the number of metrics that were entered into the function arguments
  for (i in seq_along(SD_baseline)) {

    # The MDC is computed for each metric, and then its values are appended into the list we created earlier
    MDC = SD_baseline[[i]] * sqrt(2 * (1 - ICC[[i]])) * crit_val
    list_MDC <- append(list_MDC, values = MDC)

  }

  # Iterates over all the columns of the RCI_df, excluding the subject column
  for (i in 2:ncol(RCI_df)) {

    # Creates RCI_metric, which holds just the subject and ith columns of RCI_df, and ensures it is a data frame
    RCI_metric <- RCI_df[, c(1, i)]
    RCI_metric <- data.frame(RCI_metric)

    # The RCI lower (upper) bound for each metric is just the data measurement minus (plus) the MDD of the metric
    RCI_min = RCI_df[, i] - list_MDC[[i - 1]]
    RCI_max = RCI_df[, i] + list_MDC[[i - 1]]

    # Make sure the RCI_min and RCI_max output are initially made into vectors
    list_RCI_min <- list(RCI_min)
    list_RCI_max <- list(RCI_max)

    # Now we have data frames (one for each metric), showing the lower and upper bounds side-by-side for each athlete
    RCI_threshold <- cbind(RCI_metric, unlist(list_RCI_min), unlist(list_RCI_max))
    colnames(RCI_threshold)[3] <- paste("RCI Min", colnames(RCI_df)[i], sep = "--")
    colnames(RCI_threshold)[4] <- paste("RCI Max", colnames(RCI_df)[i], sep = "--")

    # This print statement is necessary to ensure the data frames for all of the metrics are shown in the output
    print.data.frame(RCI_threshold, row.names = FALSE)

  }

}
