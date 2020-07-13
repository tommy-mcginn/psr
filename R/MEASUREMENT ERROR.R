#' Standard Error of Measurement (SEM) for a set of athlete measurements
#'
#' Computes the SEM for each metric that is passed to the function as a vector of measurements
#'
#' @param subject The vector of athletes who recorded the results for each metric (can be a numeric or factor variable)
#' @param ... Numeric vectors that represent the metrics for which the SEM should be computed. These vectors hold the scores that
#'   each athlete recorded for each respective metric (at least one metric must be passed to the function).
#' @param reliability A vector of the measures of reliability (i.e. the ICC's) for each of the metrics included in the "..."
#'   argument. This vector must contain the same number of elements as the number of metrics that have been passed to the function
#'   in the "..." argument, and the reliability values must appear in the same order as the metrics appear in the "..." argument.
#' @param method The user's choice of how the between-athlete SD should be computed in the formula for the SWC. If set to AVG (the
#'   default), each athlete's values will be averaged before the SD of these between-athlete averages is computed. If MAX is selected,
#'   then only the highest value each athlete records will be included in the computation of the between-athlete SD. Conversely, if
#'   the user indicates MIN, then only the lowest value each athlete records will be used to compute the between-athlete SD.
#' @return A list, with its contents being the SEM of each metric
#' @example
#' subject <- c(1, 1, 1, 2, 2, 2, 3, 3, 3)
#' trial <- c("Trial 1", "Trial 2", "Trial 3", "Trial 1", "Trial 2", "Trial 3", "Trial 1", "Trial 2", "Trial 3")
#' reliability <- c(0.93, 0.98, 0.95)
#' metric_1 <- c(257, 268, 237, 275, 259, 263, 216, 287, 250)
#' metric_2 <- c(1.11, 1.24, 0.89, 1.37, 1.21, 1.30, 0.75, 1.42, 1.15)
#' SEM(subject, metric_1, metric_2, reliability, method = AVG)

#' @export
SEM <- function(subject, reliability, ..., method = AVG) {

  #The inputs to this function are individual vectors, so here they are brought together into one data frame
  SEM_df <- data.frame(subject, ...)

  #Putting the reliability vector that is the final function argument is helpful to make its format consistent with SD_baseline
  reliability <- as.list(reliability)

  #This part compiles the values that should be used in the calculation of the between-subject SD, based on the user's choice
  if (method == AVG) {

    #The default is for the average value that each subject records to be included in the between-subject SD
    df <- group_by(df, subject)
    df <- summarise(df, across(where(is.numeric), ~ mean(.x)))

  } else if (method == MAX) {

    #The user also could have chosen to have only each subject's maximum value be included in the between-subject SD
    df <- group_by(df, subject)
    df <- summarise(df, across(where(is.numeric), ~ max(.x)))

  } else {

    #Otherwise, the lowest value that each subject records will be included in the between-subject SD
    df <- group_by(df, subject)
    df <- summarise(df, across(where(is.numeric), ~ min(.x)))

  }

  #Calculates the sd of only the columns that correspond to the metrics (not the subject and trial columns)
  SD_baseline <- lapply(df[, -1], sd)

  #Iterates over all items (i.e. all metrics) in the SD_baseline list created above
  for (i in seq_along(SD_baseline)) {

    #The SEM for each metric is computed, according to its proper formula, and stored in a list
    SEM = SD_baseline[[i]] * sqrt(1 - reliability[[i]])
    list_SEM <- list(SEM)

    #The items of the list are named accordingly to their respective metrics, and its contents are printed as the function output
    names(list_SEM) <- paste("SEM", colnames(df)[i+1], sep = "--")
    print(list_SEM)

  }

}
