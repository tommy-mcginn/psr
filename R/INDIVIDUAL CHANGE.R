#' Smallest Worthwhile Change (SWC) for a set of athlete measurements
#'
#' Computes the SWC for each vector of measurements corresponding to a vector of athletes that are its arguments
#'
#' @param subject The vector of athletes who recorded the results for each metric (can be a numeric or factor variable)
#' @param ... Numeric vectors that represent the metrics for which the SWC should be computed. These vectors hold the scores that
#'   each athlete recorded for each respective metric (at least one metric must be passed to the function)
#' @param method The user's choice of how the between-athlete SD should be computed in the formula for the SWC. If set to AVG (the
#'   default), each athlete's values will be averaged before the SD of these between-athlete averages is computed. If MAX is selected,
#'   then only the highest value each athlete records will be included in the computation of the between-athlete SD. Conversely, if
#'   the user indicates MIN, then only the lowest value each athlete records will be used to compute the between-athlete SD.
#' @return A list, with each item labeled as the metric of which the SWC has been calculated, is the output of this function.
#' @example
#' subject <- c(1, 1, 1, 2, 2, 2, 3, 3, 3)
#' metric_1 <- c(257, 268, 237, 275, 259, 263, 216, 287, 250)
#' metric_2 <- c(1.11, 1.24, 0.89, 1.37, 1.21, 1.30, 0.75, 1.42, 1.15)
#' SWC(subject, metric_1, metric_2, method = AVG)

#' @export
SWC <- function(subject, ..., method = AVG) {

  #The inputs to this function are individual vectors, so I bring them all together into one data frame
  df <- data.frame(subject, ...)

  #Subject needs to be a factor variable in order for the next part to work
  subject <- as.factor(subject)

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

  #This line calculates the standard deviation of all of the columns (i.e. the between-subject SD of the best scores)
  sd_btwn = lapply(df[, -1], sd)

  #A for loop, iterating over all items in sd_best created above
  for (i in seq_along(sd_btwn)) {

    #Creates Smallest Worthwhile Change for each item in sd_best (the double brackets refer to an item in a list)
    SWC = 0.2 * sd_btwn[[i]]

    #Forms a list of these SWC values, as was previously done for the TE values
    list_SWC <- list(SWC)

    #Keeps the labeling of each item in this list consistent with the TE list
    names(list_SWC) <- paste("Smallest Worthwhile Change", colnames(df)[i + 1], sep = "--")
    print(list_SWC)

  }

}

#' Minimal Detectable Change (MDC) for a set of athlete measurements
#'
#' Computes the MDC for each metric that is passed to the function as a vector of athlete measurements
#'
#' @param subject The vector of athletes who recorded the results for each metric (can be a numeric or factor variable)
#' @param ... Numeric vectors that represent the metrics for which the SEM should be computed. These vectors hold the scores that
#'   each athlete recorded for each respective metric (at least one metric must be passed to the function).
#' @param reliability A vector of the measures of reliability (i.e. the ICC's) for each of the metrics included in the "..."
#'   argument. This vector must contain the same number of elements as the number of metrics that have been passed to the function
#'   in the "..." argument, and the reliability values must appear in the same order as the metrics appear in the "..." argument.
#' @param confidence The degree of confidence the user wants to have that an improvement exceeding the MDD can be interpreted as
#'   real change, and not the result of measurement error. Set to a default value of 0.95, this parameter is used to calculate the
#'   corresponding critical value from the standard normal distribution to which we compare the RCI.
#' @return A list, with its contents being the MDC of each metric
#' @example
#' subject <- c(1, 1, 1, 2, 2, 2, 3, 3, 3)
#' metric_1 <- c(257, 268, 237, 275, 259, 263, 216, 287, 250)
#' metric_2 <- c(1.11, 1.24, 0.89, 1.37, 1.21, 1.30, 0.75, 1.42, 1.15)
#' reliability <- c(0.93, 0.98, 0.95)
#' MDC(subject, metric_1, metric_2, reliability, confidence = 0.95, method = AVG)

#' @export
MDC <- function(subject, ..., reliability, confidence = 0.95) {

  #The inputs to this function are individual vectors, so here they are brought together into one data frame
  df <- data.frame(subject, ...)

  #Subject needs to be a factor variable in order for the next part to work
  subject <- as.factor(subject)

  #Putting the reliability vector that is the final function argument is helpful to make its format consistent with SD_baseline
  reliability <- as.list(reliability)

  #This line turns the alpha value the user passed to the function into the critical value for which we use it later in the function
  crit_val <- qnorm(0.95)

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

  #Calculates the sd of only the columns that correspond to the metrics (not the subject column)
  SD_btwn <- lapply(df[, -1], sd)

  #Iterates over all items (i.e. all metrics) in the SD_baseline list created above
  for (i in seq_along(SD_btwn)) {

    #The SED for each metric is computed, according to its proper formula, and stored in a list
    MDC = SD_btwn[[i]] * sqrt(2 * (1 - reliability[[i]])) * crit_val
    list_MDC <- list(MDC)

    #The items of the list are named accordingly to their respective metrics, and its contents are printed as the function output
    names(list_MDC) <- paste("MDC", colnames(df)[i + 1], sep = "--")
    print(list_MDC)

  }

}
