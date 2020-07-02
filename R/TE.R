#' Typical Error (TE) for a set of athlete's measurements
#'
#' Computes the TE for each vector of measurements that is passed to the function, for the vector of subjects of the first argument
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
  ICC_df_full <- data.frame(subject, trial, ...)

  #This for loop iterates over the arguments passed to the function that represent the measurements for the various metrics
  for (i in 3:nargs()) {

    Metric <-

    #The Typical Error is the residual standard error (sigma) of the following regression
    lm_TE <- lm(colnames(ICC_df_full)[i] ~ as.factor(subject) + as.factor(trial), data)
    TE = sigma(lm_TE)

    #In order to express all of these Typical Error values in a way that is easy to follow, form a list that contains these values
    list_TE <- list(TE)

    #Ensures each Typical Error value in the list is labeled as "Typical Error--Metric"
    names(list_TE) <- paste("Typical Error", colnames(data)[i], sep = "--")

    #The print function must be utilized here to show the entire list in the output
    print(list_TE)

  }

}
