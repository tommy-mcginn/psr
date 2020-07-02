#' Standard Error of Measurement (SEM) for a set of athlete measurements
#'
#' Computes the SEM for each metric that is passed to the function as a vector of measurements
#'
#' @param subject The vector of athletes who recorded the results for each metric (can be a numeric or factor variable)
#' @param trial The vector that represents which trial each measurement came from
#' @param baseline A subset of the rows from the "trial" vector that the user defines as the baseline measurements
#' @param reliability A vector of the measures of reliability (i.e. the ICC's) for each of the metrics included in the "..."
#'   argument. This vector must contain the same number of elements as the number of metrics that have been passed to the function
#'   in the "..." argument, and the reliability values must appear in the same order as the metrics appear in the "..." argument.
#' @param ... Numeric vectors that represent the metrics for which the SEM should be computed. These vectors hold the scores that
#'   each athlete recorded for each respective metric (at least one metric must be passed to the function).
#' @return A list, with its contents being the SEM of each metric
#' @example
#' subject <- c(1, 1, 1, 2, 2, 2, 3, 3, 3)
#' trial <- c("Trial 1", "Trial 2", "Trial 3", "Trial 1", "Trial 2", "Trial 3", "Trial 1", "Trial 2", "Trial 3")
#' baseline <- which(trial == "Trial 1")
#' reliability <- c(0.93, 0.98, 0.95)
#' metric_1 <- c(257, 268, 237, 275, 259, 263, 216, 287, 250)
#' metric_2 <- c(1.11, 1.24, 0.89, 1.37, 1.21, 1.30, 0.75, 1.42, 1.15)
#' SEM(subject, trial, baseline, reliability, metric_1, metric_2)

#' @export
SEM <- function(subject, trial, baseline, reliability, ...) {

  #The inputs to this function are individual vectors, so here they are brought together into one data frame
  SEM_df <- data.frame(subject, trial, ...)

  #The data frame defined earlier can be altered to now only include the rows that are part of the baseline object
  SEM_df <- SEM_df[c(baseline), ]

  #Calculates the sd of only the columns that correspond to the metrics (not the subject and trial columns)
  SD_baseline <- lapply(SEM_df[, -c(1, 2)], sd)

  #Putting the reliability vector that is the final function argument is helpful to make its format consistent with SD_baseline
  reliability <- as.list(reliability)

  #Iterates over all items (i.e. all metrics) in the SD_baseline list created above
  for (i in seq_along(SD_baseline)) {

    #The SEM for each metric is computed, according to its proper formula, and stored in a list
    SEM = SD_baseline[[i]] * sqrt(1 - reliability[[i]])
    list_SEM <- list(SEM)

    #The items of the list are named accordingly to their respective metrics, and its contents are printed as the function output
    names(list_SEM) <- paste("SEM", colnames(SEM_df)[i+2], sep = "--")
    print(list_SEM)

  }

}

#' Standard Error of Difference (SED) for a set of athlete measurements
#'
#' Computes the SED for each metric that is passed to the function as a vector of measurements
#'
#' @param subject The vector of athletes who recorded the results for each metric (can be a numeric or factor variable)
#' @param trial The vector that represents which trial each measurement came from
#' @param baseline A subset of the rows from the "trial" vector that the user defines as the baseline measurements
#' @param reliability A vector of the measures of reliability (i.e. the ICC's) for each of the metrics included in the "..."
#'   argument. This vector must contain the same number of elements as the number of metrics that have been passed to the function
#'   in the "..." argument, and the reliability values must appear in the same order as the metrics appear in the "..." argument.
#' @param ... Numeric vectors that represent the metrics for which the SEM should be computed. These vectors hold the scores that
#'   each athlete recorded for each respective metric (at least one metric must be passed to the function).
#' @return A list, with its contents being the SEM of each metric
#' @example
#' subject <- c(1, 1, 1, 2, 2, 2, 3, 3, 3)
#' trial <- c("Trial 1", "Trial 2", "Trial 3", "Trial 1", "Trial 2", "Trial 3", "Trial 1", "Trial 2", "Trial 3")
#' baseline <- which(trial == "Trial 1")
#' reliability <- c(0.93, 0.98, 0.95)
#' metric_1 <- c(257, 268, 237, 275, 259, 263, 216, 287, 250)
#' metric_2 <- c(1.11, 1.24, 0.89, 1.37, 1.21, 1.30, 0.75, 1.42, 1.15)
#' SED(subject, trial, metric_1, metric_2, baseline, reliability)

#' @export
SED <- function(subject, trial, baseline, reliability, ...) {

  #The inputs to this function are individual vectors, so here they are brought together into one data frame
  SED_df <- data.frame(subject, trial, ...)

  #The data frame defined earlier can be altered to now only include the rows that are part of the baseline object
  SED_df <- SED_df[c(baseline), ]

  #Calculates the sd of only the columns that correspond to the metrics (not the subject and trial columns)
  SD_baseline <- lapply(SED_df[, -c(1, 2)], sd)

  #Putting the reliability vector that is the final function argument is helpful to make its format consistent with SD_baseline
  reliability <- as.list(reliability)

  #Iterates over all items (i.e. all metrics) in the SD_baseline list created above
  for (i in seq_along(SD_baseline)) {

    #The SED for each metric is computed, according to its proper formula, and stored in a list
    SED = SD_baseline[[i]] * sqrt(2 * (1 - reliability[[i]]))
    list_SED <- list(SED)

    #The items of the list are named accordingly to their respective metrics, and its contents are printed as the function output
    names(list_SED) <- paste("SED", colnames(SED_df)[i+2], sep = "--")
    print(list_SED)

  }

}
