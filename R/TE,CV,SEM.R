#' @title Typical Error (TE) for a set of athlete measurements
#'
#' @description Computes the TE for each vector of measurements that is passed to the function, for the subject and trial vectors
#'
#' @param subject The vector of athletes who recorded the results for each metric (can be a numeric or factor variable)
#' @param trial The vector that represents which trial each measurement came from
#' @param ... Numeric vectors that represent the metrics for which the CV should be computed. These vectors hold the scores that
#'   each athlete recorded for each respective metric (at least one metric must be passed to the function)
#'
#' @return A data frame, with the name of each metric situated above its calculated TE
#'
#' @examples
#' subject <- c(1, 1, 1, 2, 2, 2, 3, 3, 3)
#' trial <- c(1, 2, 3, 1, 2, 3, 1, 2, 3)
#' metric_1 <- c(250, 258, 252, 279, 270, 277, 218, 213, 218)
#' metric_2 <- c(10, 7, 10, 14, 18, 17, 11, 7, 8)
#' metric_3 <- c(1214, 1276, 1289, 1037, 1010, 1069, 1481, 1465, 1443)
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
  print(output_df, row.names = FALSE)

}

#' @title Coefficient of Variation (CV), or typical percentage error (as in Hopkins (2000)), for a set of athlete measurements
#'
#' @description Computes the CV for each vector of measurements that is passed to the function, for the subject and trial vectors
#'
#' @param subject The vector of athletes who recorded the results for each metric (can be a numeric or factor variable)
#' @param trial The vector that represents which trial each measurement came from
#' @param ... Numeric vectors that represent the metrics for which the CV should be computed. These vectors hold the scores that
#'   each athlete recorded for each respective metric (at least one metric must be passed to the function).
#'
#' @return A data frame, with the name of each metric situated above its calculated CV
#'
#' @examples
#' subject <- c(1, 1, 1, 2, 2, 2, 3, 3, 3)
#' trial <- c(1, 2, 3, 1, 2, 3, 1, 2, 3)
#' metric_1 <- c(250, 258, 252, 279, 270, 277, 218, 213, 218)
#' metric_2 <- c(10, 7, 10, 14, 18, 17, 11, 7, 8)
#' metric_3 <- c(1214, 1276, 1289, 1037, 1010, 1069, 1481, 1465, 1443)
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
  print(output_df, row.names = FALSE)

}

#' @title Standard Error of Measurement (SEM) for a set of athlete measurements
#'
#' @description Computes the SEM for each metric that is passed to the function as a vector of measurements, for the subject and
#'   trial vectors and the ICC's of the metrics
#'
#' @param subject The vector of athletes who recorded the results for each metric (can be a numeric or factor variable)
#' @param trial The vector that represents which trial each measurement came from
#' @param ... Numeric vectors that represent the metrics for which the SEM should be computed. These vectors hold the scores that
#'   each athlete recorded for each respective metric (at least one metric must be passed to the function).
#' @param ICC A vector of the ICC's for each of the metrics included in the "..." argument. This vector must contain the same
#'   number of elements as the number of metrics that have been passed to the function in the "..." argument, and the reliability
#'   values must appear in the same order as the metrics appear in the "..." argument.
#'
#' @return A data frame, with the name of each metric situated above its calculated SEM
#'
#' @examples
#' subject <- c(1, 1, 1, 2, 2, 2, 3, 3, 3)
#' trial <- c(1, 2, 3, 1, 2, 3, 1, 2, 3)
#' metric_1 <- c(250, 258, 252, 279, 270, 277, 218, 213, 218)
#' metric_2 <- c(10, 7, 10, 14, 18, 17, 11, 7, 8)
#' metric_3 <- c(1214, 1276, 1289, 1037, 1010, 1069, 1481, 1465, 1443)
#' SEM(subject, trial, metric_1, metric_2, metric_3, ICC = c(0.92, 0.98, 0.95))
#'
#' @references Atkinson, G., & Nevill, A. M. (1998). Statistical Methods For Assessing Measurement Error (Reliability) in Variables
#'   Relevant to Sports Medicine. Sports Medicine, 26(4), 217-238.
#'
#' @export
SEM <- function(subject, trial, ..., ICC) {

  # The subject variable must be converted to a factor variable in order for the function to work
  subject <- as.factor(subject)

  # Calls the check_error function, which produces informative error messages if any of a variety of errors are made by the user
  check_error(subject, trial, ...)

  # The inputs to this function are individual vectors, so here they are brought together into one data frame
  input_df <- data.frame(subject, ...)

  # Creates lists into which the SD and SEM values, respectively, will be placed
  list_SD <- list()
  list_SEM <- list()

  # Putting the vector of ICC's into a list is helpful to make its format consistent with list_SD and list_SEM
  ICC <- as.list(ICC)

  # Calculates the sd of each of the metric columns (not the subject column) and places it into list_SD
  for (i in 2:ncol(input_df)) {

    SD = stats::sd(input_df[, i])
    list_SD <- append(list_SD, values = SD)

  }

  # Iterates over all items (i.e. all metrics) in the SD list created above, and uses them to compute the SWC for each metric
  for (i in seq_along(list_SD)) {

    SEM = list_SD[[i]] * sqrt(1 - ICC[[i]])
    list_SEM <- append(list_SEM, values = SEM)

  }

  # Converts the list to a data frame, which creates nicer output
  output_df <- data.frame(matrix(unlist(list_SEM), nrow = 1))

  # The column names are assigned to be the metric names they represent
  for (i in seq_along(output_df)) {

    colnames(output_df)[i] <- colnames(input_df)[i + 1]

  }

  # The output data frame is printed
  print(output_df, row.names = FALSE)

}
