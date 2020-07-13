#' Typical Error (TE) for a set of athlete measurements
#'
#' Computes the TE for each vector of measurements that is passed to the function, for the vector of subjects in the first argument
#'
#' @param subject The vector of athletes who recorded the results for each metric (can be a numeric or factor variable)
#' @param trial The vector that represents which trial each measurement came from
#' @param ... Numeric vectors that represent the metrics for which the CV should be computed. These vectors hold the scores that
#'   each athlete recorded for each respective metric (at least one metric must be passed to the function)
#' @return A list, with its contents being the group-level CV's of each metric (and labeled as such), is the output of this function
#' @example
#' subject <- c(1, 1, 1, 2, 2, 2, 3, 3, 3)
#' trial <- c('Trial 1', 'Trial 2', 'Trial 3', 'Trial 1', 'Trial 2', 'Trial 3', 'Trial 1', 'Trial 2', 'Trial 3')
#' metric_1 <- c(257, 268, 237, 275, 259, 263, 216, 287, 250)
#' metric_2 <- c(1.11, 1.24, 0.89, 1.37, 1.21, 1.30, 0.75, 1.42, 1.15)
#' TE(subject, trial, metric_1, metric_2)

#' @export
TE <- function(subject, trial, ...) {

  #The inputs to this function are individual vectors, so here they are brought together into one data frame
  TE_df <- data.frame(subject, trial, ...)

  #The subject and trial vectors must be factor variables in order for the linear model to work properly later on
  subject <- as.factor(subject)
  trial <- as.factor(trial)

  #This for loop iterates over the arguments passed to the function that represent the measurements for the various metrics
  for (i in 3:ncol(TE_df)) {

    #We say that "metric" is the ith column of the data frame for each iteration, then use it in the linear model that follows
    metric <- TE_df[, i]

    #The Typical Error is the residual standard error (which is what the sigma function computes) of the following regression
    lm_TE <- lm(metric ~ subject + trial)
    TE = sigma(lm_TE)

    #In order to express all of these Typical Error values in a way that is easy to follow, form a list that contains these values
    list_TE <- list(TE)

    #Labels each Typical Error value in the list according to the metric it represents, and then prints the contents of the list
    names(list_TE) <- paste("Typical Error", colnames(TE_df)[i], sep = "--")
    print(list_TE)

  }

}

#' Coefficient of Variation (CV) for a set of athlete measurements, expressed as a percentage of the mean
#'
#' Computes the CV for each vector of measurements that is passed to the function, for the vector of subjects in the first argument
#'
#' @param subject The vector of athletes who recorded the results for each metric (can be a numeric or factor variable)
#' @param ... Numeric vectors that represent the metrics for which the CV should be computed. These vectors hold the scores that
#'   each athlete recorded for each respective metric (at least one metric must be passed to the function)
#' @return A list, with its contents being the group-level CV's of each metric (and labeled as such), is the output of this function
#' @example
#' subject <- c(1, 1, 1, 2, 2, 2, 3, 3, 3)
#' metric_1 <- c(257, 268, 237, 275, 259, 263, 216, 287, 250)
#' metric_2 <- c(1.11, 1.24, 0.89, 1.37, 1.21, 1.30, 0.75, 1.42, 1.15)
#' CV(subject, metric_1, metric_2)

#' @export
CV <- function(subject, ...) {

  #The inputs to this function are individual vectors, so here they brought together into one data frame
  CV_df <- data.frame(subject, ...)

  #The subject variable should be a factor variable, in order for the summarize_if function below to work
  subject <- as.factor(subject)

  #In order for the CV for the entire sample to be calculated, the within-subject sd and mean first need to be computed
  CV_df <- group_by(CV_df, subject)
  CV_sd <- summarize_if(CV_df, is.numeric, sd)
  CV_mean <- summarize_if(CV_df, is.numeric, mean)

  #This for loop iterates over all of the metrics passed to the function as its arguments
  for (i in 2:nargs()) {

    #The CV for each individual is the sd divided by the mean, multiplied by 100 (which expresses it as a percentage)
    CV_ind = (CV_sd[, i] / CV_mean[, i]) * 100

    #First create the list to hold each individual's CV value for each metric, then append the individual CV values into it
    list_CV_ind <- list()
    list_CV_ind <- append(list_CV_ind, values = CV_ind)

    #The CV for each metric for the entire group is simply the mean of the individual CV's for each metric
    CV_group <- lapply(list_CV_ind, mean)

    #Names each element of the CV_group list by the metric it represents, and explicitly prints out the full contents of the list
    names(CV_group) <- paste("CV(%)", colnames(CV_df)[i], sep = "--")
    print(CV_group)

  }

}

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
  df <- data.frame(subject, ...)

  #Subject needs to be a factor variable in order for the next part to work
  subject <- as.factor(subject)

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
    names(list_SEM) <- paste("SEM", colnames(df)[i + 1], sep = "--")
    print(list_SEM)

  }

}
