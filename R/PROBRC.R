#' @title Probability of a given change for a given metric being reliable from trial to trial, for each athlete in the data
#'
#' @description For each change that an athlete records from one trial to another, the probability that the change is reliable is
#'   generated, along with asterisks for athletes that have achieved given probabilities of change.
#'
#' @param subject The vector of athletes who recorded the results for each metric (can be a numeric or factor variable)
#' @param trial The vector that represents which trial each measurement came from
#' @param ... Numeric vectors that represent the metrics for which the probability of the given changes being reliable should be
#'   computed. These vectors hold the scores that each athlete recorded for each respective metric (at least one metric must be
#'   passed to the function).
#' @param ICC A vector of the ICC's for each of the metrics included in the "..." argument. This vector must contain the same number
#'   of elements as the number of metrics that have been passed to the function in the "..." argument, and the reliability values
#'   must appear in the same order as the metrics appear in the "..." argument.
#' @param initial The trial to be considered the "before" (or baseline) measurements
#' @param final The trial to be considered the "after" measurements (the trial we are comparing to the baseline)
#'
#' @return A data frame, with the subjects as rows and the metrics as columns, and each entry representing the probability that the
#'   final measurement was a reliable change from the individual measurement.
#'
#' @examples
#' subject <- c(1, 1, 1, 2, 2, 2, 3, 3, 3)
#' trial <- c(1, 2, 3, 1, 2, 3, 1, 2, 3)
#' first <- which(trial == 1)
#' second <- which(trial == 2)
#' metric_1 <- c(250, 258, 252, 279, 270, 277, 218, 213, 218)
#' metric_2 <- c(10, 7, 10, 14, 18, 17, 11, 7, 8)
#' PROBRC(subject, trial, metric_1, metric_2, ICC = c(0.92, 0.98), initial = first, final = second)
#'
#' @references Ferger, K., & Büsch, D. (2018). Individual measurement of performance change in sports. Deutsche Zeitschrift Für
#'   Sportmedizin, 2018(02), 45-52.
#'
#' @export
PROBRC <- function(subject, trial, ..., ICC, initial, final) {

  # The inputs to this function are individual vectors, so I bring them all together into one data frame
  input_df <- data.frame(subject, ...)

  # Calls the check_error function, which produces informative error messages if any of a variety of errors are made by the user
  check_error(subject, trial, ...)

  # We create separate data frames for the "initial trial" and "final trial" measurements, respectively
  initial_df <- input_df[c(initial), ]
  final_df <- input_df[c(final), ]

  # A new data frame is created, which will end up being the function output
  # It will store the probability that each final measurement was a reliable change from the initial measurement
  output_df <- data.frame(subject)

  # Make a list for the baseline standard deviation calculations for each metric
  SD_baseline <- list()

  # For each column except that of the subjects in the data, we want to compute the between-athlete sd
  for (i in 2:ncol(initial_df)) {

    SD <- stats::sd(initial_df[, i])
    SD_baseline <- append(SD_baseline, values = SD)

  }

  # Converting the ICC to a list (as opposed to a vector) makes the next for loop easier to calculate
  ICC <- as.list(ICC)

  # This makes a list for the SED values upfront, so that we can append to it in the next for loop
  list_SED <- list()

  # The sequence along SD_baseline is really just the number of metrics that were entered into the function arguments
  for (i in seq_along(SD_baseline)) {

    # The SED is computed for each metric, and then its values are appended into the list we created earlier
    SED = SD_baseline[[i]] * sqrt(2) * (1 - ICC[[i]])
    list_SED <- append(list_SED, values = SED)

  }

  for (i in 2:ncol(input_df)) {

    # The change to be analyzed is the difference of the athlete's final score and initial score
    change = final_df[, i] - initial_df[, i]

    # This change is divided by the standard error of difference of its metric, which is what that athlete's RCI is
    RCI = change / list_SED[[i - 1]]

    # The probability of interest comes from a two-tailed test of the Student's t-distribution
    prob = round(abs((2 * stats::pnorm(RCI)) - 1), 2)

    # Puts these vectors of probability values into a list and names each item of the list according to the metric it represents
    prob_list <- list(prob)
    output_df <- cbind(output_df, unlist(prob_list))

  }

  # The column names of the output data frame are assigned to be the metric names they represent
  for (i in 2:ncol(output_df)) {

    colnames(output_df)[i] <- colnames(input_df)[i]

  }

  # I print the data frame in this way as the output so that I can hide the "1" that otherwise appears as the row number
  print.data.frame(output_df, row.names = FALSE)

}
