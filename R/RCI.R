#' Reliable Change Index (RCI) for a set of athlete measurements
#'
#' For each metric passed to the function, vectors indicating the lower and upper bounds of the RCI of that metric are
#' generated, with the length of each vector equaling the number of athletes passed to the function
#'
#' @param subject The subjects on which the RCI test should be run
#' @param baseline The measurements to be considered the "baseline" results for the calculation of the RCI
#' @param reliability A vector of the measures of reliability (i.e. the ICC's) for each of the metrics included in the
#'   "..." argument. This vector must contain the same number of elements as the number of metrics that have been
#'   passed to the function in the "..." argument, and the reliability values must appear in the same order as the
#'   metrics appear in the "..." argument.
#' @param confidence The degree of confidence the user wants to have that an improvement exceeding the MDD can be
#'   interpreted as real change, and not the result of measurement error. Set to a default value of 0.95, this
#'   parameter is used to calculate the corresponding critical value from the standard normal distribution to which we
#'   compare the RCI.
#' @param ... Numeric vectors that represent the metrics for which the ICC should be computed. These vectors hold the
#'   scores that each athlete recorded for each respective metric (at least one metric must be passed to the function).
#' @return Two vectors for each metric, the first containing the RCI upper bound for each athlete and the second
#'   containing the RCI lower bound for each athlete. Both vectors corresponding to the first metric come before both
#'   vectors corresponding to the second metric, which come before both vectors corresponding to the third metric, etc.
#' @example
#' subject <- c(1, 1, 1, 2, 2, 2, 3, 3, 3)
#' trial <- c("Trial 1", "Trial 2", "Trial 3", "Trial 1", "Trial 2", "Trial 3", "Trial 1", "Trial 2", "Trial 3")
#' pre_test <- which(trial == "Trial 1")
#' final_trial <- which(trial == "Trial 2")
#' reliability <- c(0.93, 0.98, 0.95)
#' metric_1 <- c(257, 268, 237, 275, 259, 263, 216, 287, 250)
#' metric_2 <- c(1.11, 1.24, 0.89, 1.37, 1.21, 1.30, 0.75, 1.42, 1.15)
#' RCI(subject, initial_trial, final_trial, metric_1, metric_2)

#' @export
RCI <- function(subject, baseline, reliability, confidence = 0.95, ...) {

  #The individual subject and metric vectors that were passed to this function are brought together into a data frame
  RCI_df <- data.frame(subject, ...)

  #We care only about the results of the trial we labeled as the "initial trial" in the function arguments
  RCI_df <- RCI_df[c(baseline), ]

  #For each column except that of the subjects in the data, we want to compute the between-athlete sd
  SD_baseline <- lapply(RCI_df[, -1], sd)

  #Converting the ICC to a list (as opposed to a vector) makes the next for loop easier to calculate
  reliability <- as.list(reliability)

  #The confidence level the user entered should be in the form of its corresponding critical value in our computations
  crit_val <- qnorm((1 + confidence) / 2)

  #This makes a list for the MDD values upfront, so that we can append to it in the next for loop
  list_MDD <- list()

  #The sequence along SD_baseline is really just the number of metrics that were entered into the function arguments
  for (i in seq_along(SD_baseline)) {

    #The MDD is computed for each metric, and then its values are appended into the list we created earlier
    MDD = SD_baseline[[i]] * sqrt(2 * (1 - reliability[[i]])) * crit_val
    list_MDD <- append(list_MDD, values = MDD)

  }

  #Iterates over all the columns of the RCI_df, excluding the subject column
  for (i in 2:ncol(RCI_df)) {

    #The RCI lower (upper) bound for each metric is just the data measurement minus (plus) the MDD of the metric
    RCI_min = RCI_df[, i] - list_MDD[[i - 1]]
    RCI_max = RCI_df[, i] + list_MDD[[i - 1]]

    #Make sure the RCI_min and RCI_max output are initially made into vectors
    RCI_min <- as.vector(RCI_min)
    RCI_max <- as.vector(RCI_max)

    #Now we have data frames (one for each metric), showing the lower and upper bounds side-by-side for each athlete
    RCI_threshold <- data.frame(RCI_min, RCI_max)

    #This print statement is necessary to ensure the data frames for all of the metrics are shown in the output
    print(RCI_threshold)

  }

}
