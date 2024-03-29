#' @title Smallest Worthwhile Change (SWC) for a set of athlete measurements
#'
#' @description Computes the SWC for each vector of measurements that is passed to the function, for the subject and trial vectors,
#'   the desired effect size of the change, and the desired method of calculating the between-subject standard deviation
#'
#' @param subject The vector of athletes who recorded the results for each metric (can be a numeric or factor variable)
#' @param trial The vector that represents which trial each measurement came from
#' @param ... Numeric vectors that represent the metrics for which the SWC should be computed. These vectors hold the scores that
#'   each athlete recorded for each respective metric (at least one metric must be passed to the function).
#' @param effect_size The proportion of the within-subject standard deviation that is defined as a worthwhile change. The default
#'   value is 0.2, but users can input any positive value into this argument.
#' @param method The user's choice of how the between-athlete SD should be computed in the formula for the SWC. If set to AVG, each
#'   athlete's values will be averaged before the SD of these between-athlete averages is computed. If MAX is selected, then only
#'   the highest value each athlete records will be included in the computation of the between-athlete SD. Conversely, if the user
#'   indicates MIN, then only the lowest value each athlete records will be used to compute the between-athlete SD.
#'
#' @return A data frame, with the name of each metric situated above its calculated SWC
#'
#' @examples
#' subject <- c(1, 1, 1, 2, 2, 2, 3, 3, 3)
#' trial <- c(1, 2, 3, 1, 2, 3, 1, 2, 3)
#' metric_1 <- c(250, 258, 252, 279, 270, 277, 218, 213, 218)
#' metric_2 <- c(10, 7, 10, 14, 18, 17, 11, 7, 8)
#' metric_3 <- c(1214, 1276, 1289, 1037, 1010, 1069, 1481, 1465, 1443)
#' SWC(subject, trial, metric_1, metric_2, metric_3, effect_size = 0.2, method = 'AVG')
#'
#' @references Bernards, J., Sato, K., Haff, G., & Bazyler, C. (2017). Current Research and Statistical Practices in Sport
#'   Science and a Need for Change. Sports, 5(4), 87.
#'
#' @export
SWC <- function(subject, trial, ..., effect_size = 0.2, method = c('AVG', 'MAX', 'MIN')) {

  # The subject variable must be converted to a factor variable in order for the function to work
  subject <- as.factor(subject)

  # The inputs to this function are individual vectors, so I bring them all together into one data frame
  input_df <- data.frame(subject, ...)

  # Calls the check_error function, which produces informative error messages if any of a variety of errors are made by the user
  check_error(subject, trial, ...)

  # Creates lists into which the SWC values will be placed
  list_SWC <- list()

  # This part compiles the values that should be used in the calculation of the between-subject SD, based on the user's choice
  # The default is for the average value that each subject records to be included in the between-subject SD
  if (method == 'AVG') {

    input_df <- dplyr::group_by(input_df, subject)
    input_df <- dplyr::summarise_if(input_df, is.numeric, mean)

  # The user also could have chosen to have only each subject's maximum value be included in the between-subject SD
  } else if (method == 'MAX') {

    input_df <- dplyr::group_by(input_df, subject)
    input_df <- dplyr::summarise_if(input_df, is.numeric, max)

  # Otherwise, the lowest value that each subject records will be included in the between-subject SD
  } else if (method == 'MIN') {

    input_df <- dplyr::group_by(input_df, subject)
    input_df <- dplyr::summarise_if(input_df, is.numeric, min)

  }

  # Calculates the standard deviation of each metric column (i.e. the between-subject standard deviation of the scores)
  SD = lapply(input_df[, -1], stats::sd)

  # Iterates over all items (i.e. all metrics) in the SD list created above, and uses them to compute the SWC for each metric
  for (i in seq_along(SD)) {

    SWC = effect_size * SD[[i]]
    list_SWC <- append(list_SWC, values = SWC)

  }

  # Converts the list to a data frame, which creates tidier output
  output_df <- data.frame(matrix(unlist(list_SWC), nrow = 1))

  # The column names are assigned to be the metric names they represent
  for (i in seq_along(output_df)) {

    colnames(output_df)[i] <- colnames(input_df)[i + 1]

  }

  # The output data frame is printed
  print(output_df, row.names = FALSE)

}

#' @title Minimal Detectable Change (MDC) for a set of athlete measurements
#'
#' @description Computes the MDC for each metric that is passed to the function as a vector of athlete measurements, for the
#'   subject and trial vectors, the ICC's of each metric, and the desired confidence level
#'
#' @param subject The vector of athletes who recorded the results for each metric (can be a numeric or factor variable)
#' @param trial The vector that represents which trial each measurement came from
#' @param ... Numeric vectors that represent the metrics for which the SEM should be computed. These vectors hold the scores that
#'   each athlete recorded for each respective metric (at least one metric must be passed to the function).
#' @param ICC A vector of the ICC's for each of the metrics included in the "..." argument. This vector must contain the same
#'   number of elements as the number of metrics that have been passed to the function in the "..." argument, and the reliability
#'   values must appear in the same order as the metrics appear in the "..." argument.
#' @param confidence The degree of confidence the user wants to have that an improvement exceeding the MDC can be interpreted as
#'   real change, and not the result of measurement error, based on the standard normal distribution. The default value is 0.95.
#'
#' @return A data frame, with the name of each metric situated above its calculated MDC
#'
#' @examples
#' subject <- c(1, 1, 1, 2, 2, 2, 3, 3, 3)
#' trial <- c(1, 2, 3, 1, 2, 3, 1, 2, 3)
#' metric_1 <- c(250, 258, 252, 279, 270, 277, 218, 213, 218)
#' metric_2 <- c(10, 7, 10, 14, 18, 17, 11, 7, 8)
#' metric_3 <- c(1214, 1276, 1289, 1037, 1010, 1069, 1481, 1465, 1443)
#' MDC(subject, trial, metric_1, metric_2, metric_3, ICC = c(0.92, 0.98, 0.95), confidence = 0.95)
#'
#' @references Riemann, B. L., &amp; Lininger, M. R. (2018). Statistical Primer for Athletic Trainers: The Essentials of
#'   Understanding Measures of Reliability and Minimal Important Change. Journal of Athletic Training, 53(1), 98-103.
#'
#' @export
MDC <- function(subject, trial, ..., ICC, confidence = 0.95) {

  # The subject variable must be converted to a factor variable in order for the function to work
  subject <- as.factor(subject)

  # Calls the check_error function, which produces informative error messages if any of a variety of errors are made by the user
  check_error(subject, trial, ...)

  # The inputs to this function are individual vectors, so here they are brought together into one data frame
  input_df <- data.frame(subject, ...)

  # Creates lists into which the SD and MDC values, respectively, will be placed
  list_SD <- list()
  list_MDC <- list()

  # Putting the vector of ICC's into a list is helpful to make its format consistent with list_SD and list_MDC
  ICC <- as.list(ICC)

  # This line turns the alpha value the user passed to the function into the critical value that we use later in the function
  crit_val <- stats::qnorm((1 + confidence) / 2)

  # Calculates the sd of each of the metric columns (not the subject column) and places it into list_SD
  for (i in 2:ncol(input_df)) {

    SD = stats::sd(input_df[, i])
    list_SD <- append(list_SD, values = SD)

  }

  # Iterates over all items (i.e. all metrics) in the SD list created above, and uses them to compute the SWC for each metric
  for (i in seq_along(list_SD)) {

    MDC = list_SD[[i]] * sqrt(2) * crit_val * sqrt(1 - ICC[[i]])
    list_MDC <- append(list_MDC, values = MDC)

  }

  # Converts the list to a data frame, which creates nicer output
  output_df <- data.frame(matrix(unlist(list_MDC), nrow = 1))

  # The column names are assigned to be the metric names they represent
  for (i in seq_along(output_df)) {

    colnames(output_df)[i] <- colnames(input_df)[i + 1]

  }

  # The output data frame is printed
  print(output_df, row.names = FALSE)

}
