#' Coefficient of Variation (CV) for a set of athlete measurements, expressed as a percentage of the mean
#'
#' Computes the CV for each vector of measurements that is passed to the function, for the vector of subjects that is the first argument
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

    #Names each element of the CV_group list by the metric it represents, and explicitly prints out the list to see its full contents
    names(CV_group) <- paste("CV(%)", colnames(CV_df)[i], sep = "--")
    print(CV_group)

  }

}
