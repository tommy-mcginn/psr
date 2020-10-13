# This function performs error checking for a given set of subjects and trials passed to the function
# It is meant to be inserted within each function to be computed automatically within the function, not to be called by the user
check_error <- function(subject, trial, ...) {

  # A data frame is made, containing all of the vectors of the measurements
  df <- data.frame(...)

  # Iterates over the columns of full_df, with the exception of the subject and trial columns
  for (i in seq_along(df)) {

    # Assigns each column after the subject and trial columns of full_df to be the metric columns
    metric <- df[, i]

    # Checks to make sure the metric vector is numeric, producing an informative error message if it is not
    if (is.numeric(metric) == FALSE) {

      print("Each metric column must be a numeric vector")

      stop()

    }

  }

  # Produces an informative error message if each athlete has not recorded exactly one measurement for each trial
  if (length(unique(subject)) * length(unique(trial)) != length(df)) {

    print("Each athlete must have recorded the same number of trials")

    stop()

  }

}
