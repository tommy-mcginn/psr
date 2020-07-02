#' Smallest Worthwhile Change (SWC) for a set of athlete measurements
#'
#' Computes the SWC for each vector of measurements corresponding to a vector of athletes that are its arguments
#'
#' @param subject The vector of athletes who recorded the results for each metric (can be a numeric or factor variable)
#' @param ... Numeric vectors that represent the metrics for which the SWC should be computed. These vectors hold the scores that
#'   each athlete recorded for each respective metric (at least one metric must be passed to the function)
#' @param max A logical, indicating what type of metrics comprise the preceding arguments. If TRUE (its default), each metric is
#'   assumed to have the property that higher values are seen as more desirable for athletes (e.g. for power and/or force metrics).
#'   If set to FALSE, it is assumed that, for each metric, lower values are more desirable (e.g. for metrics in which athletes are
#'   trying to complete a task in the least amount of time possible, such as 40-yard dash times)
#' @return A list, with each item labeled as the metric of which the SWC has been calculated, is the output of this function.
#' @example
#' subject <- c(1, 1, 1, 2, 2, 2, 3, 3, 3)
#' metric_1 <- c(257, 268, 237, 275, 259, 263, 216, 287, 250)
#' metric_2 <- c(1.11, 1.24, 0.89, 1.37, 1.21, 1.30, 0.75, 1.42, 1.15)
#' SWC(subject, metric_1, metric_2, max = TRUE)

#' @export
SWC <- function(subject, metric_1, ..., max = TRUE) {

  #The inputs to this function are individual vectors, so I bring them all together into one data frame
  df <- data.frame(subject, ...)

  #Subject needs to be a factor variable in order for the next part to work
  Subject <- as.factor(subject)

  #This part summarises the numeric columns of the data, by each Subject's "best score"
  if (max == TRUE) {

    #The default is for each subject's "best score" to be the maximum value he or she records (e.g. power/force)
    df <- group_by(df, Subject)
    df <- summarise(df, across(where(is.numeric), ~ max(.x)))

  } else {

    #Otherwise, each subject's "best score" is the minimum value he or she records (e.g. 40-yard dash times)
    df <- group_by(df, Subject)
    df <- summarise(df, across(where(is.numeric), ~ min(.x)))

  }

  #This line calculates the standard deviation of all of the columns (i.e. the between-subject SD of the best scores)
  sd_best = lapply(df[, -1], sd)

  #A for loop, iterating over all items in sd_best created above
  for (i in seq_along(sd_best)) {

    #Creates Smallest Worthwhile Change for each item in sd_best (the double brackets refer to an item in a list)
    SWC = 0.2 * sd_best[[i]]

    #Forms a list of these SWC values, as was previously done for the TE values
    list_SWC <- list(SWC)

    #Keeps the labeling of each item in this list consistent with the TE list
    names(list_SWC) <- paste("Smallest Worthwhile Change", colnames(df)[i+1], sep = "--")
    print(list_SWC)

  }

}

#' Minimal Detectable Difference (MDD) for a set of athlete measurements
#'
#' Computes the MDD for each metric that is passed to the function as a vector of athlete measurements
#'
#' @param subject The vector of athletes who recorded the results for each metric (can be a numeric or factor variable)
#' @param trial The vector that represents which trial each measurement came from
#' @param baseline A subset of the rows from the "trial" vector that the user defines as the baseline measurements
#' @param reliability A vector of the measures of reliability (i.e. the ICC's) for each of the metrics included in the "..."
#'   argument. This vector must contain the same number of elements as the number of metrics that have been passed to the function
#'   in the "..." argument, and the reliability values must appear in the same order as the metrics appear in the "..." argument.
#' @param alpha The p-value of the standard normal distribution for which the user wants the MDD to be computed.  This p-value
#'   should be given for a one-sided test, so if the user wants to know the change that one could be 95% confident is not due to
#'   measurement error, the user should input alpha = 0.975 to the function, which is also the default value for the function.
#' @param ... Numeric vectors that represent the metrics for which the SEM should be computed. These vectors hold the scores that
#'   each athlete recorded for each respective metric (at least one metric must be passed to the function).
#' @return A list, with its contents being the SEM of each metric
#' @example
#' subject <- c(1, 1, 1, 2, 2, 2, 3, 3, 3)
#' trial <- c("Trial 1", "Trial 2", "Trial 3", "Trial 1", "Trial 2", "Trial 3", "Trial 1", "Trial 2", "Trial 3")
#' baseline <- which(trial == "Trial 1")
#' reliability <- c(0.93, 0.98, 0.95)
#' alpha <- 0.975
#' metric_1 <- c(257, 268, 237, 275, 259, 263, 216, 287, 250)
#' metric_2 <- c(1.11, 1.24, 0.89, 1.37, 1.21, 1.30, 0.75, 1.42, 1.15)
#' MDD(subject, trial, baseline, reliability, alpha, metric_1, metric_2)

#' @export
MDD <- function(subject, trial, baseline, reliability, alpha = 0.975, ...) {

  #The inputs to this function are individual vectors, so here they are brought together into one data frame
  MDD_df <- data.frame(subject, trial, ...)

  #The data frame defined earlier can be altered to now only include the rows that are part of the baseline object
  MDD_df <- MDD_df[c(baseline), ]

  #Calculates the sd of only the columns that correspond to the metrics (not the subject and trial columns)
  SD_baseline <- lapply(MDD_df[, -c(1, 2)], sd)

  #Putting the reliability vector that is the final function argument is helpful to make its format consistent with SD_baseline
  reliability <- as.list(reliability)

  #This line turns the alpha value the user passed to the function into the critical value for which we use it later in the function
  crit_val <- qnorm((1 - alpha) / 2, lower = F)

  #Iterates over all items (i.e. all metrics) in the SD_baseline list created above
  for (i in seq_along(SD_baseline)) {

    #The SED for each metric is computed, according to its proper formula, and stored in a list
    MDD = SD_baseline[[i]] * sqrt(2 * (1 - reliability[[i]])) * crit_val
    list_MDD <- list(MDD)

    #The items of the list are named accordingly to their respective metrics, and its contents are printed as the function output
    names(list_MDD) <- paste("MDD", colnames(MDD_df)[i+2], sep = "--")
    print(list_MDD)

  }

}
