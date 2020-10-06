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

  #The inputs to this function are individual vectors, so here they are brought together into one data frame
  full_df <- data.frame(subject, trial, ...)

  #This data frame is created from the get-go, and it will be the output that is returned at the end
  output_df <- data.frame(Metric = paste("TE"))

  #This for loop iterates over the arguments passed to the function that represent the measurements for the various metrics
  for (i in 3:ncol(full_df)) {

    #We say that "metric" is the ith column of the data frame for each iteration, then use it in the linear model that follows
    metric <- full_df[, i]

    #Checks to make sure the metric vector is numeric, producing an informative error message if it is not
    if (is.numeric(metric) == FALSE) {

      print("Each metric column must be a numeric vector")

    }

    #Produces an informative error message if each athlete has not recorded exactly one measurement for each trial
    if (length(unique(subject)) * length(unique(trial)) != length(metric)) {

      print("Each athlete must have recorded the same number of trials")

    }

    #The Typical Error is the residual standard error (which is what the sigma function computes) of the following regression
    lm_TE <- stats::lm(metric ~ as.factor(subject) + as.factor(trial))
    TE = stats::sigma(lm_TE)

    #Places the TE values into a table and names each row of the table by its metric, which makes for clean output
    output_df <- cbind(output_df, unlist(TE))

    #Sets the column names as the metrics that their TE values in the table represent
    colnames(output_df)[i - 1] <- colnames(full_df)[i]

  }

  #I print the data frame in this way as the output so that I can hide the "1" that otherwise appears as the row number
  print.data.frame(output_df, row.names = FALSE)

}

#' @title Coefficient of Variation (CV) for a set of athlete measurements, expressed as a percentage of the mean
#'
#' @description Computes the CV for each vector of measurements that is passed to the function, for the vector of subjects in the
#'   first argument
#'
#' @param subject The vector of athletes who recorded the results for each metric (can be a numeric or factor variable)
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

  #The inputs to this function are individual vectors, so here they brought together into one data frame
  full_df <- data.frame(subject, trial, ...)

  #This for loop iterates over the arguments passed to the function that represent the measurements for the various metrics
  for (i in 3:ncol(full_df)) {

    #Assigns each metric to be the ith column in the full data frame
    metric <- full_df[, i]

    #Checks to make sure the metric vector is numeric, producing an informative error message if it is not
    if (is.numeric(metric) == FALSE) {

      print("Each metric column must be a numeric vector")

    }

    #Produces an informative error message if each athlete has not recorded exactly one measurement for each trial
    if (length(unique(subject)) * length(unique(trial)) != length(metric)) {

      print("Each athlete must have recorded the same number of trials")

    }

  }

  #We don't need the trials after the above error checking, so they are deleted from the full data frame
  full_df <- data.frame(subject, ...)

  #The subject variable should be a factor variable, in order for the summarize_if function below to work
  subject <- as.factor(subject)

  #This data frame is created from the get-go, and it will be the output that is returned at the end
  output_df <- data.frame(Metric = paste("CV"))

  #In order for the CV for the entire sample to be calculated, the within-subject sd and mean first need to be computed
  full_df <- dplyr::group_by(full_df, subject)
  CV_sd <- dplyr::summarise_if(full_df, is.numeric, stats::sd)
  CV_mean <- dplyr::summarise_if(full_df, is.numeric, mean)

  #This for loop iterates over all of the metrics passed to the function as its arguments
  for (i in 3:nargs()) {

    #The CV for each individual is the sd divided by the mean, multiplied by 100 (which expresses it as a percentage)
    CV_ind = (CV_sd[, i - 1] / CV_mean[, i - 1]) * 100

    #First create the list to hold each individual's CV value for each metric, then append the individual CV values into it
    list_CV_ind <- list()
    list_CV_ind <- append(list_CV_ind, values = CV_ind)

    #The CV for each metric for the entire group is simply the mean of the individual CV's for each metric
    CV_group <- lapply(list_CV_ind, mean)

    #Places the CV values into a table and names each row of the table by its metric, which makes for clean output
    output_df <- cbind(output_df, unlist(CV_group))
    colnames(output_df)[i - 1] <- colnames(full_df)[i - 1]

  }

  #I print the data frame in this way as the output so that I can hide the "1" that otherwise appears as the row number
  print.data.frame(output_df, row.names = FALSE)

}

#' @title Standard Error of Measurement (SEM) for a set of athlete measurements
#'
#' @description Computes the SEM for each metric that is passed to the function as a vector of measurements
#'
#' @param subject The vector of athletes who recorded the results for each metric (can be a numeric or factor variable)
#' @param ... Numeric vectors that represent the metrics for which the SEM should be computed. These vectors hold the scores that
#'   each athlete recorded for each respective metric (at least one metric must be passed to the function).
#' @param ICC A vector of the ICC's for each of the metrics included in the "..." argument. This vector must contain the same number of
#'   elements as the number of metrics that have been passed to the function in the "..." argument, and the reliability values must
#'   appear in the same order as the metrics appear in the "..." argument.
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
#' ICC <- c(0.92, 0.98, 0.95)
#' SEM(subject, trial, metric_1, metric_2, metric_3, ICC, method = 'AVG')
#'
#' @references Atkinson, G., & Nevill, A. M. (1998). Statistical Methods For Assessing Measurement Error (Reliability) in Variables
#'   Relevant to Sports Medicine. Sports Medicine, 26(4), 217-238.
#'
#' @export
SEM <- function(subject, trial, ..., reliability, method = c('AVG', 'MAX', 'MIN')) {

  #The inputs to this function are individual vectors, so here they are brought together into one data frame
  full_df <- data.frame(subject, trial, ...)

  #This for loop iterates over the arguments passed to the function that represent the measurements for the various metrics
  for (i in 3:ncol(full_df)) {

    metric <- full_df[, i]

    #Checks to make sure the metric vector is numeric, producing an informative error message if it is not
    if (is.numeric(metric) == FALSE) {

      print("Each metric column must be a numeric vector")

    }

    #Produces an informative error message if each athlete has not recorded exactly one measurement for each trial
    if (length(unique(subject)) * length(unique(trial)) != length(metric)) {

      print("Each athlete must have recorded the same number of trials")

    }

  }

  #The trials are only needed for the error checking above, so they are deleted here
  full_df <- data.frame(subject, ...)

  #Subject needs to be a factor variable in order for the next part to work
  subject <- as.factor(subject)

  #This data frame is created from the get-go, and it will be the output that is returned at the end
  output_df <- data.frame(Metric = paste("SEM"))

  #Putting the reliability vector that is the final function argument is helpful to make its format consistent with SD_baseline
  reliability <- as.list(reliability)

  #This part compiles the values that should be used in the calculation of the between-subject SD, based on the user's choice
  if (method == 'AVG') {

    #The default is for the average value that each subject records to be included in the between-subject SD
    df <- dplyr::group_by(full_df, subject)
    df <- dplyr::summarise_if(full_df, is.numeric, mean)

  } else if (method == 'MAX') {

    #The user also could have chosen to have only each subject's maximum value be included in the between-subject SD
    df <- dplyr::group_by(full_df, subject)
    df <- dplyr::summarise_if(full_df, is.numeric, max)

  } else if (method == 'MIN') {

    #Otherwise, the lowest value that each subject records will be included in the between-subject SD
    df <- dplyr::group_by(full_df, subject)
    df <- dplyr::summarise_if(full_df, is.numeric, min)

  }

  #Calculates the sd of only the columns that correspond to the metrics (not the subject column)
  SD_baseline <- lapply(full_df[, -1], stats::sd)

  #Iterates over all items (i.e. all metrics) in the SD_baseline list created above
  for (i in seq_along(SD_baseline)) {

    #The SEM for each metric is computed, according to its proper formula, and stored in a list
    SEM = SD_baseline[[i]] * sqrt(1 - reliability[[i]])

    #Places the SEM values into a table and names each row of the table by its metric, which makes for clean output
    output_df <- cbind(output_df, unlist(SEM))
    colnames(output_df)[i + 1] <- colnames(full_df)[i + 1]

  }

  #I print the data frame in this way as the output so that I can hide the "1" that otherwise appears as the row number
  print.data.frame(output_df, row.names = FALSE)

}
