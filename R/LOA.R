#' Limits of Agreement (LoA) for a set of athlete measurements
#'
#' Computes the LoA for each vector of measurements that is passed to the function, for the vector of subjects in the first argument
#'
#' @param subject The vector of athletes who recorded the results for each metric (can be a numeric or factor variable)
#' @param trial The vector that represents which trial each measurement came from
#' @param ... Numeric vectors that represent the metrics for which the CV should be computed. These vectors hold the scores that
#'   each athlete recorded for each respective metric (at least one metric must be passed to the function)
#' @param confidence The degree of confidence the user wants to have that an improvement exceeding the MDD can be interpreted as
#'   real change, and not the result of measurement error. Set to a default value of 0.95, this parameter is used to calculate the
#'   corresponding critical value from the standard normal distribution to which we compare the RCI.
#' @return A list, with its contents being the group-level CV's of each metric (and labeled as such), is the output of this function
#' @example
#' subject <- c(1, 1, 1, 2, 2, 2, 3, 3, 3)
#' trial <- c('Trial 1', 'Trial 2', 'Trial 3', 'Trial 1', 'Trial 2', 'Trial 3', 'Trial 1', 'Trial 2', 'Trial 3')
#' metric_1 <- c(257, 268, 237, 275, 259, 263, 216, 287, 250)
#' metric_2 <- c(1.11, 1.24, 0.89, 1.37, 1.21, 1.30, 0.75, 1.42, 1.15)
#' LoA(subject, trial, metric_1, metric_2, confidence = 0.95)

LoA <- function(subject, trial, ..., confidence = 0.95) {

  #The inputs to this function are individual vectors, so here they are brought together into one data frame
  df <- data.frame(subject, trial, ...)

  #The subject and trial vectors must be factor variables in order for the linear model to work properly later on
  subject <- as.factor(subject)
  trial <- as.factor(trial)

  #A list to store the TE values is made up front outside of any for loop, and we append the TE values to it in the next for loop
  list_TE <- list()

  #This for loop iterates over the arguments passed to the function that represent the measurements for the various metrics
  for (i in 3:ncol(df)) {

    #We say that "metric" is the ith column of the data frame for each iteration, then use it in the linear model that follows
    metric <- df[, i]

    #The Typical Error is the residual standard error (which is what the sigma function computes) of the following regression
    lm_TE <- lm(metric ~ subject + trial)
    TE = sigma(lm_TE)

    #I now add the TE values to the list I created earlier for this purpose
    list_TE <- append(list_TE, values = TE)

  }

  #This line computes the critical value for the 95% limits of agreement, with the degrees of freedom equal to the sample size
  crit_val <- qt(confidence, df = length(unique(subject)))

  #Iterates over all of the contents in the list that contains our TE values
  for (i in seq_along(list_TE)) {

    #Computes the LoA for every metric in the list and subsequently puts these values into a list
    LoA = list_TE[[i]] * sqrt(2) * crit_val
    list_LoA <- list(LoA)

    #Labels each element of the list according to the metric it represents, and prints out the contents of the list
    names(list_LoA) <- paste("Limits of Agreement", colnames(df)[i + 2], sep = "--")
    print(list_LoA)

  }

}
