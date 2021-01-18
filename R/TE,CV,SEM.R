#' @title Typical Error (TE) for a set of athlete measurements
#'
#' @description Computes the TE for each vector of measurements that is passed to the function, for the vector of subjects in the
#'   first argument
#'
#' @param subject The vector of athletes who recorded the results for each metric (can be a numeric or factor variable)
#' @param trial The vector that represents which trial each measurement came from
#' @param ... Numeric vectors that represent the metrics for which the CV should be computed. These vectors hold the scores that
#'   each athlete recorded for each respective metric (at least one metric must be passed to the function)
#'
#' @return A table, with each metric being its own column and its entry being the TE of that metric, is the output of this function
#'
#' @example
#' subject <- c(1, 1, 1, 2, 2, 2, 3, 3, 3)
#' trial <- c('Trial 1', 'Trial 2', 'Trial 3', 'Trial 1', 'Trial 2', 'Trial 3', 'Trial 1', 'Trial 2', 'Trial 3')
#' metric_1 <- c(257, 268, 237, 275, 259, 263, 216, 287, 250)
#' metric_2 <- c(1.11, 1.24, 0.89, 1.37, 1.21, 1.30, 0.75, 1.42, 1.15)
#' metric_3 <- c(1272, 1493, 1072, 1046, 1198, 1165, 1478, 1370, 1335)
#' TE(subject, trial, metric_1, metric_2, metric_3)
#'
#' @references Hopkins, W. G. (2000). Measures of Reliability in Sports Medicine and Science. Sports Medicine, 30(5), 375-381.
#'
#' @export
TE <- function(subject, trial, ...) {

  # The inputs to this function are individual vectors, so here they are brought together into one data frame
  input_df <- data.frame(subject, trial, ...)

  # Calls the check_error function, which produces informative error messages if any of a variety of errors are made by the user
  check_error(subject, trial, ...)

  # Creates a list into which the TE values will be placed
  list_TE <- list()

  # This for loop iterates over the arguments passed to the function that represent the measurements for the various metrics
  for (i in 3:ncol(input_df)) {

    # We say that "metric" is the ith column of the data frame for each iteration, then use it in the linear model that follows
    metric <- input_df[, i]

    # The Typical Error is the residual standard error (which is what the sigma function computes) of the following regression
    lm_TE <- stats::lm(metric ~ as.factor(subject) + as.factor(trial))
    TE = stats::sigma(lm_TE)

    # Places the TE values into the list created earlier
    list_TE <- append(list_TE, values = TE)

  }

  # Converts the list to a data frame, which creates nicer output
  output_df <- data.frame(matrix(unlist(list_TE), nrow = 1))

  # The column names are assigned to be the metric names they represent
  for (i in seq_along(output_df)) {

    colnames(output_df)[i] <- colnames(input_df)[i + 2]

  }

  # The output data frame is printed
  print.data.frame(output_df, row.names = FALSE)

}

#' @title Coefficient of Variation (CV), or typical percentage error (as in Hopkins (2000)) for a set of athlete measurements,
#'   expressed as a percentage
#'
#' @description Computes the CV for each vector of measurements that is passed to the function, for the vector of subjects in the
#'   first argument and the vector of trials in the second argument
#'
#' @param subject The vector of athletes who recorded the results for each metric (can be a numeric or factor variable)
#' @param trial The vector that represents which trial each measurement came from
#' @param ... Numeric vectors that represent the metrics for which the CV should be computed. These vectors hold the scores that
#'   each athlete recorded for each respective metric (at least one metric must be passed to the function).
#'
#' @return A table, with each metric being its own column and its entry being the CV of that metric, is the output of this function
#'
#' @example
#' subject <- c(1, 1, 1, 2, 2, 2, 3, 3, 3)
#' trial <- c('Trial 1', 'Trial 2', 'Trial 3', 'Trial 1', 'Trial 2', 'Trial 3', 'Trial 1', 'Trial 2', 'Trial 3')
#' metric_1 <- c(257, 268, 237, 275, 259, 263, 216, 287, 250)
#' metric_2 <- c(1.11, 1.24, 0.89, 1.37, 1.21, 1.30, 0.75, 1.42, 1.15)
#' metric_3 <- c(1272, 1493, 1072, 1046, 1198, 1165, 1478, 1370, 1335)
#' CV(subject, trial, metric_1, metric_2, metric_3)
#'
#' @references Hopkins, W. G. (2000). Measures of Reliability in Sports Medicine and Science. Sports Medicine, 30(5), 375-381.
#'
#' @export
CV <- function(subject, trial, ...) {

  # The inputs to this function are individual vectors, so here they brought together into one data frame
  input_df <- data.frame(subject, trial, ...)

  # Calls the check_error function, which produces informative error messages if any of a variety of errors are made by the user
  check_error(subject, trial, ...)

  # Creates a list into which the CV values will be placed
  list_CV <- list()

  # This for loop iterates over the arguments passed to the function that represent the measurements for the various metrics
  for (i in 3:ncol(input_df)) {

    # We say that "metric" is the ith column of the data frame for each iteration, then use it in the linear model that follows
    metric <- input_df[, i]

    # The Typical Error is the residual standard error (which is what the sigma function computes) of the following regression
    lm_TE <- stats::lm(metric ~ as.factor(subject) + as.factor(trial))
    TE = stats::sigma(lm_TE)
    CV = (TE/mean(metric)) * 100

    # Places the TE values into the list created earlier
    list_CV <- append(list_CV, values = CV)

  }

  # Converts the list to a data frame, which creates nicer output
  output_df <- data.frame(matrix(unlist(list_CV), nrow = 1))

  # The column names are assigned to be the metric names they represent
  for (i in seq_along(output_df)) {

    colnames(output_df)[i] <- colnames(input_df)[i + 2]

  }

  # The output data frame is printed
  print.data.frame(output_df, row.names = FALSE)

}

#' @title Standard Error of Measurement (SEM) for a set of athlete measurements
#'
#' @description Computes the SEM for each metric that is passed to the function as a vector of measurements
#'
#' @param subject The vector of athletes who recorded the results for each metric (can be a numeric or factor variable)
#' @param ... Numeric vectors that represent the metrics for which the SEM should be computed. These vectors hold the scores that
#'   each athlete recorded for each respective metric (at least one metric must be passed to the function).
#' @param ICC A vector of the ICC's for each of the metrics included in the "..." argument. This vector must contain the same number
#'   of elements as the number of metrics that have been passed to the function in the "..." argument, and the reliability values
#'   must appear in the same order as the metrics appear in the "..." argument.
#' @param method The user's choice of how the between-athlete SD should be computed in the formula for the SEM. If set to AVG, each
#'   athlete's values will be averaged before the SD of these between-athlete averages is computed. If MAX is selected, then only
#'   the highest value each athlete records will be included in the computation of the between-athlete SD. Conversely, if the user
#'   indicates MIN, then only the lowest value each athlete records will be used to compute the between-athlete SD.
#'
#' @return A table, with each metric being its own column and its entry being the SEM of that metric, is the output of this function
#'
#' @example
#' subject <- c(1, 1, 1, 2, 2, 2, 3, 3, 3)
#' trial <- c('Trial 1', 'Trial 2', 'Trial 3', 'Trial 1', 'Trial 2', 'Trial 3', 'Trial 1', 'Trial 2', 'Trial 3')
#' metric_1 <- c(257, 268, 237, 275, 259, 263, 216, 287, 250)
#' metric_2 <- c(1.11, 1.24, 0.89, 1.37, 1.21, 1.30, 0.75, 1.42, 1.15)
#' metric_3 <- c(1272, 1493, 1072, 1046, 1198, 1165, 1478, 1370, 1335)
#' SEM(subject, trial, metric_1, metric_2, metric_3, ICC = c(0.92, 0.98, 0.95), method = 'AVG')
#'
#' @references Atkinson, G., & Nevill, A. M. (1998). Statistical Methods For Assessing Measurement Error (Reliability) in Variables
#'   Relevant to Sports Medicine. Sports Medicine, 26(4), 217-238.
#'
#' @export
SEM <- function(subject, trial, ..., ICC, method = c('AVG', 'MAX', 'MIN')) {

  # The subject variable must be converted to a factor variable in order for the function to work
  subject <- as.factor(subject)

  # The inputs to this function are individual vectors, so here they are brought together into one data frame
  input_df <- data.frame(subject, trial, ...)

  # Calls the check_error function, which produces informative error messages if any of a variety of errors are made by the user
  check_error(subject, trial, ...)

  # The trials are only needed for the error checking above, so they are deleted here
  input_df <- data.frame(subject, ...)

  # Creates a list into which the SEM values will be placed
  list_SEM <- list()

  # Putting the vector of ICC's that is the penultimate function argument is helpful to make its format consistent with SD_baseline
  ICC <- as.list(ICC)

  # This part compiles the values that should be used in the calculation of the between-subject SD, based on the user's choice
  if (method == 'AVG') {

    # The default is for the average value that each subject records to be included in the between-subject SD
    input_df <- dplyr::group_by(input_df, subject)
    input_df <- dplyr::summarise_if(input_df, is.numeric, mean)

  } else if (method == 'MAX') {

    # The user also could have chosen to have only each subject's maximum value be included in the between-subject SD
    input_df <- dplyr::group_by(input_df, subject)
    input_df <- dplyr::summarise_if(input_df, is.numeric, max)

  } else if (method == 'MIN') {

    # Otherwise, the lowest value that each subject records will be included in the between-subject SD
    input_df <- dplyr::group_by(input_df, subject)
    input_df <- dplyr::summarise_if(input_df, is.numeric, min)

  }

  # Calculates the sd of only the columns that correspond to the metrics (not the subject column)
  SD_baseline <- lapply(input_df[, -1], stats::sd)

  # Iterates over all items (i.e. all metrics) in the SD_baseline list created above
  for (i in seq_along(SD_baseline)) {

    # The SEM for each metric is computed, according to its proper formula, and stored in a list
    SEM = SD_baseline[[i]] * sqrt(1 - ICC[[i]])

    # Places the TE values into the list created earlier
    list_SEM <- append(list_SEM, values = SEM)

  }

  # Converts the list to a data frame, which creates nicer output
  output_df <- data.frame(matrix(unlist(list_SEM), nrow = 1))

  # The column names are assigned to be the metric names they represent
  for (i in seq_along(output_df)) {

    colnames(output_df)[i] <- colnames(input_df)[i + 1]

  }

  # The output data frame is printed
  print.data.frame(output_df, row.names = FALSE)

}
