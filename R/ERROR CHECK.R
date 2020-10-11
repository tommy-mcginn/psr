#' @title Error Checking for a data frame of athlete measurements
#'
#' @description Performs error checking for a data frame that is passed to the function, which must include a vector of subjects, a
#'  vector of trials, and one or more numeric vectors containing the measurements of each metric. This function is meant to be called
#'  within each function, not by the user.

#' @param full_df The data frame with n columns, that contains the subject vector as its first column, the trial vector as its second
#'  column, and the metric vectors as its third through nth columns
#'
#' @return If no errors are produced, this function does not return anything to the user, but rather the main function that was
#'  called by the user returns whatever it is meant to return. If this function detects an error, however, then an informative error
#'  message is given to the user, and the main function being run will not work. This function thus has the purpose of telling the
#'  user what has gone wrong when the main function does not work properly, rather than it just crashing and producing an unhelpful
#'  error message.

check_error <- function(full_df) {

  # Iterates over the columns of full_df, with the exception of the subject and trial columns
  for (i in 3:ncol(full_df)) {

    # Assigns each column after the subject and trial columns of full_df to be the metric columns
    metric <- full_df[, i]

    # Checks to make sure the metric vector is numeric, producing an informative error message if it is not
    if (is.numeric(metric) == FALSE) {

      print("Each metric column must be a numeric vector")

    }

    # Produces an informative error message if each athlete has not recorded exactly one measurement for each trial
    if (length(unique(subject)) * length(unique(trial)) != length(metric)) {

      print("Each athlete must have recorded the same number of trials")

    }

  }

}
