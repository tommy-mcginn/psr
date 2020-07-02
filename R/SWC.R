#' Smallest Worthwhile Change (SWC) for a set of athlete measurements
#'
#' Computes the SWC for each vector of measurements corresponding to a vector of athletes that are its arguments
#'
#' @param subject The vector of athletes who recorded the results for each metric (can be a numeric or factor variable)
#' @param ... Numeric vectors that represent the metrics for which the SWC should be computed. These vectors hold the scores that
#'   each athlete recorded for each respective metric (at least one metric must be passed to the function)
#' @param max A logical, indicating what type of metrics comprise the preceding arguments. If TRUE (its default), each metric is
#'   assumed to have the property that higher values are seen as more desirable for athletes (e.g. for power and/or force metrics).
#'   If set to FALSE, it is assumed that, for each metric, lower values are more desirable (e.g. for metrics in which athletes are
#'   trying to complete a task in the least amount of time possible, such as 40-yard dash times)
#' @return A list, with each item labeled as the metric of which the SWC has been calculated, is the output of this function.
#' @example
#' subject <- c(1, 1, 1, 2, 2, 2, 3, 3, 3)
#' metric_1 <- c(257, 268, 237, 275, 259, 263, 216, 287, 250)
#' metric_2 <- c(1.11, 1.24, 0.89, 1.37, 1.21, 1.30, 0.75, 1.42, 1.15)
#' SWC(subject, metric_1, metric_2, max = TRUE)

#' @export
SWC <- function(subject, metric_1, ..., max = TRUE) {

  #The inputs to this function are individual vectors, so I bring them all together into one data frame
  df <- data.frame(subject, ...)

  #Subject needs to be a factor variable in order for the next part to work
  Subject <- as.factor(subject)

  #This part summarises the numeric columns of the data, by each Subject's "best score"
  if (max == TRUE) {

    #The default is for each subject's "best score" to be the maximum value he or she records (e.g. power/force)
    df <- group_by(df, Subject)
    df <- summarise(df, across(where(is.numeric), ~ max(.x)))

  } else {

    #Otherwise, each subject's "best score" is the minimum value he or she records (e.g. 40-yard dash times)
    df <- group_by(df, Subject)
    df <- summarise(df, across(where(is.numeric), ~ min(.x)))

  }

  #This line calculates the standard deviation of all of the columns (i.e. the between-subject SD of the best scores)
  sd_best = lapply(df[, -1], sd)

  #A for loop, iterating over all items in sd_best created above
  for (i in seq_along(sd_best)) {

    #Creates Smallest Worthwhile Change for each item in sd_best (the double brackets refer to an item in a list)
    SWC = 0.2 * sd_best[[i]]

    #Forms a list of these SWC values, as was previously done for the TE values
    list_SWC <- list(SWC)

    #Keeps the labeling of each item in this list consistent with the TE list
    names(list_SWC) <- paste("Smallest Worthwhile Change", colnames(df)[i+1], sep = "--")
    print(list_SWC)

  }

}
