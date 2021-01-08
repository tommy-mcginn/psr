#' @title Probability of a given change for a given metric being reliable from trial to trial, for each athlete in the data
#'
#' @description For each change that an athlete records from one trial to another, the probability that the change is reliable is
#'   generated, along with asterisks for athletes that have achieved given probabilities of change.
#'
#' @param subject The subjects for which the probabilities of reliable change should be computed
#' @param initial The set of measurements to be considered the "before" measurements
#' @param final The set of measurements to be considered the "after" measurements
#' @param ... Numeric vectors that represent the metrics for which the probability of the given changes being reliable should be
#'   computed. These vectors hold the scores that each athlete recorded for each respective metric (at least one metric must be
#'   passed to the function).
#' @param reliability A vector of the measures of reliability (i.e. the ICC's) for each of the metrics included in the
#'   "..." argument. This vector must contain the same number of elements as the number of metrics that have been
#'   passed to the function in the "..." argument, and the reliability values must appear in the same order as the
#'   metrics appear in the "..." argument.
#'
#' @return A table for each metric, with each row being an athlete and each entry being the probability that the change that has
#'   occurred is reliable, based on the RCI calculation.
#'
#' @example
#' subject <- c(1, 1, 1, 2, 2, 2, 3, 3, 3)
#' trial <- c('Trial 1', 'Trial 2', 'Trial 3', 'Trial 1', 'Trial 2', 'Trial 3', 'Trial 1', 'Trial 2', 'Trial 3')
#' metric_1 <- c(257, 268, 237, 275, 259, 263, 216, 287, 250)
#' metric_2 <- c(1.11, 1.24, 0.89, 1.37, 1.21, 1.30, 0.75, 1.42, 1.15)
#' metric_3 <- c(1272, 1493, 1072, 1046, 1198, 1165, 1478, 1370, 1335)
#' prel(subject, trial, metric_1, metric_2, metric_3, ICC = c(0.92, 0.98, 0.95), initial = which(trial == "Trial 1"), final = which(trial == "Trial 2"))
#'
#' @references Ferger, K., & Büsch, D. (2018). Individual measurement of performance change in sports. Deutsche Zeitschrift Für
#'   Sportmedizin, 2018(02), 45-52.
#'
#' @export
prel <- function(subject, trial, ..., ICC, initial, final) {

  # The individual subject and metric vectors that were passed to this function are brought together into a data frame
  df <- data.frame(subject, ...)

  # Calls the check_error function, which produces informative error messages if any of a variety of errors are made by the user
  check_error(subject, trial, ...)

  # We create separate data frames for the "initial trial" and "final trial" measurements, respectively
  initial_df <- df[c(initial), ]
  final_df <- df[c(final), ]

  # This data frame is created from the get-go, and it will be the output that is returned at the end
  output_df <- data.frame(Metric = paste("P(Reliable Change)"))

  #For each column except that of the subjects in the data, we want to compute the between-athlete sd
  SD_baseline <- lapply(initial_df[, -1], stats::sd)

  #Converting the ICC to a list (as opposed to a vector) makes the next for loop easier to calculate
  ICC <- as.list(ICC)

  #This makes a list for the SED values upfront, so that we can append to it in the next for loop
  list_SED <- list()

  #The sequence along SD_baseline is really just the number of metrics that were entered into the function arguments
  for (i in seq_along(SD_baseline)) {

    #The SED is computed for each metric, and then its values are appended into the list we created earlier
    SED = SD_baseline[[i]] * sqrt(2) * (1 - ICC[[i]])
    list_SED <- append(list_SED, values = SED)

  }

  for (i in 2:ncol(df)) {

    #The change to be analyzed is the difference of the athlete's final score and initial score
    change = final_df[, i] - initial_df[, i]

    #This change is divided by the standard error of difference of its metric, which is what that athlete's RCI is
    RCI = change / list_SED[[i - 1]]

    #The probability of interest comes from a two-tailed test of the Student's t-distribution
    prob = abs((2 * stats::pnorm(RCI)) - 1)

    #Puts these vectors of probability values into a list and names each item of the list according to the metric it represents
    prob_list <- list(prob)
    output_df <- cbind(output_df, unlist(prob_list))
    colnames(output_df)[i] <- colnames(df)[i]

  }

  #Explicitly prints the list to see its full contents
  print.data.frame(output_df, row.names = FALSE)

}
