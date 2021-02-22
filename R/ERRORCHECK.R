# This function performs error checking for a given set of subjects and trials passed to the function
# It is inserted within each function to be computed automatically within the function, and is never called by the user

check_error <- function(subject, trial, ...) {

    # A data frame is made, containing all of the vectors of the measurements
    df <- data.frame(...)

    # Iterates over the columns of df, with the exception of the subject and trial columns
    for (i in seq_along(df)) {

      # Assigns each column after the subject and trial columns of df to be the metric columns
      metric <- df[, i]

      # Checks to make sure the metric vector is numeric, producing an informative error message if it is not
      if (is.numeric(metric) == FALSE) {

        print("Each metric must be numeric")

        stop()

      }

    }

    # Produces an informative error message if each athlete has not recorded a measurement for each trial that appears in the data
    if (length(unique(subject)) * length(unique(trial)) != nrow(df)) {

      print("Each athlete must have recorded a measurement for each trial")

      stop()

    }

    # Produces an informative error message if any athlete has recorded two or more measurements for any trial
    data <- data.frame(subject, trial, ...)

    data <- dplyr::count(data, subject, trial)

    if (length(unique(data$n)) != 1) {

      print("Each athlete must not have recorded multiple measurements for any trial")

      stop()

    }

}
