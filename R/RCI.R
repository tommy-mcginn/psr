#' @title Reliable Change Index (RCI) for a set of athlete measurements
#'
#' @description For each metric passed to the function, vectors indicating the lower and upper bounds of the RCI of that metric are
#'   generated, with the length of each vector equaling the number of athletes passed to the function
#'
#' @param subject The subjects on which the RCI test should be run
#' @param ... Numeric vectors that represent the metrics for which the RCI should be computed. These vectors hold the
#'   scores that each athlete recorded for each respective metric (at least one metric must be passed to the function).
#' @param baseline The measurements to be considered the "baseline" results for the calculation of the RCI
#' @param reliability A vector of the measures of reliability (i.e. the ICC's) for each of the metrics included in the "..."
#'   argument. It is suggested that the reliability measure chosen be the ICC, but it can be any reliability measure, as long as the
#'   measure chosen lies on a 0-1 scale (with 1 indicating higher reliability). This vector must contain the same number of
#'   elements as the number of metrics that have been passed to the function in the "..." argument, and the reliability values must
#'   appear in the same order as the metrics appear in the "..." argument.
#' @param confidence The degree of confidence the user wants to have that the lower and upper bounds given can be
#'   interpreted as real change, and not the result of random or measurement error. Set to a default value of 0.95, this
#'   parameter is used to calculate the corresponding critical value from the standard normal distribution that is used to compute
#'   the lower and upper bounds of the RCI.
#'
#' @return A table for each metric, with each row being an athlete and two columns, one for the RCI lower bound and the other for
#'   the RCI upper bound.
#'
#' @example
#' subject <- c(1, 1, 1, 2, 2, 2, 3, 3, 3)
#' metric_1 <- c(257, 268, 237, 275, 259, 263, 216, 287, 250)
#' metric_2 <- c(1.11, 1.24, 0.89, 1.37, 1.21, 1.30, 0.75, 1.42, 1.15)
#' metric_3 <- c(1272, 1493, 1072, 1046, 1198, 1165, 1478, 1370, 1335)
#' baseline <- which(trial == "Trial 1")
#' reliability <- c(0.92, 0.98, 0.95)
#' RCI(subject, metric_1, metric_2, metric_3, baseline, reliability, confidence = 0.95)
#'
#' @references Ferger, K., & Büsch, D. (2018). Individual measurement of performance change in sports. Deutsche Zeitschrift Für
#'   Sportmedizin, 2018(02), 45-52.
#'
#' @export
RCI <- function(subject, baseline, ..., reliability, confidence = 0.95) {

  #The individual subject and metric vectors that were passed to this function are brought together into a data frame
  RCI_df <- data.frame(subject, ...)

  #We care only about the results of the trial we labeled as the "initial trial" in the function arguments
  RCI_df <- RCI_df[c(baseline), ]

  #For each column except that of the subjects in the data, we want to compute the between-athlete sd
  SD_baseline <- lapply(RCI_df[, -1], stats::sd)

  #Converting the ICC to a list (as opposed to a vector) makes the next for loop easier to calculate
  reliability <- as.list(reliability)

  #The confidence level the user entered should be in the form of its corresponding critical value in our computations
  crit_val <- stats::qnorm((1 + confidence) / 2)

  #This makes a list for the MDC values upfront, so that we can append to it in the next for loop
  list_MDC <- list()

  #The sequence along SD_baseline is really just the number of metrics that were entered into the function arguments
  for (i in seq_along(SD_baseline)) {

    #The MDD is computed for each metric, and then its values are appended into the list we created earlier
    MDC = SD_baseline[[i]] * sqrt(2 * (1 - reliability[[i]])) * crit_val
    list_MDC <- append(list_MDC, values = MDC)

  }

  #Iterates over all the columns of the RCI_df, excluding the subject column
  for (i in 2:ncol(RCI_df)) {

    #The RCI lower (upper) bound for each metric is just the data measurement minus (plus) the MDD of the metric
    RCI_min = RCI_df[, i] - list_MDC[[i - 1]]
    RCI_max = RCI_df[, i] + list_MDC[[i - 1]]

    #Make sure the RCI_min and RCI_max output are initially made into vectors
    RCI_min <- as.vector(RCI_min)
    RCI_max <- as.vector(RCI_max)

    #Now we have data frames (one for each metric), showing the lower and upper bounds side-by-side for each athlete
    RCI_threshold <- data.frame(RCI_min, RCI_max)
    colnames(RCI_threshold)[1] <- paste("RCI Min", colnames(RCI_df)[i], sep = "--")
    colnames(RCI_threshold)[2] <- paste("RCI Max", colnames(RCI_df)[i], sep = "--")

    #This print statement is necessary to ensure the data frames for all of the metrics are shown in the output
    print(RCI_threshold)

  }

}

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
#' initial <- which(trial == "Trial 1")
#' final <- which(trial == "Trial 2")
#' metric_1 <- c(257, 268, 237, 275, 259, 263, 216, 287, 250)
#' metric_2 <- c(1.11, 1.24, 0.89, 1.37, 1.21, 1.30, 0.75, 1.42, 1.15)
#' metric_3 <- c(1272, 1493, 1072, 1046, 1198, 1165, 1478, 1370, 1335)
#' reliability <- c(0.92, 0.98)
#' RCI(subject, initial, final, metric_1, metric_2, metric_3, reliability)
#'
#'
#' @references Ferger, K., & Büsch, D. (2018). Individual measurement of performance change in sports. Deutsche Zeitschrift Für
#'   Sportmedizin, 2018(02), 45-52.
#'
#' @export
prob_rel_chng <- function(subject, initial, final, ..., reliability) {

  df <- data.frame(subject, ...)

  #We care only about the results of the trial we labeled as the "initial trial" in the function arguments
  initial_df <- df[c(initial), ]

  final_df <- df[c(final), ]

  #For each column except that of the subjects in the data, we want to compute the between-athlete sd
  SD_baseline <- lapply(initial_df[, -1], stats::sd)

  #Converting the ICC to a list (as opposed to a vector) makes the next for loop easier to calculate
  reliability <- as.list(reliability)

  #This makes a list for the SED values upfront, so that we can append to it in the next for loop
  list_SED <- list()

  #The sequence along SD_baseline is really just the number of metrics that were entered into the function arguments
  for (i in seq_along(SD_baseline)) {

    #The SED is computed for each metric, and then its values are appended into the list we created earlier
    SED = SD_baseline[[i]] * sqrt(2) * (1 - reliability[[i]])
    list_SED <- append(list_SED, values = SED)

  }

  for (i in 2:ncol(df)) {

    #The change to be analyzed is the difference of the athlete's final score and initial score
    change = final_df[, i] - initial_df[, i]

    #This change is divided by the standard error of difference of its metric, which is what that athlete's RCI is
    RCI = change / list_SED[[i - 1]]

    #The probability of interest comes from a two-tailed test of the Student's t-distribution
    prob = abs((2 * stats::pt(RCI)) - 1)

    #Puts these vectors of probability values into a list and names each item of the list according to the metric it represents
    prob_list <- list(prob)
    names(prob_list) <- paste("Probability of Reliable Change", colnames(df)[i], sep = "--")

    #Explicitly prints the list to see its full contents
    print(prob_list)

  }

}
