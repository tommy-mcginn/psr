#These functions take the "Subject" vector as its first argument and any number of metrics as its following arguments
#It computes the Smallest Worthwhile Change (SWC) for each metric
#The logical "max" allows the user to specify whether the scores used to compute the SWC should be the maximum or minimum values

SWC <- function(Subject, ..., max = TRUE) {

  #The inputs to this function are individual vectors, so I bring them all together into one data frame
  df <- data.frame(Subject, ...)

  #Subject needs to be a factor variable in order for the next part to work
  Subject<-as.factor(Subject)

  #This part summarises the numeric columns of the data, by each Subject's "best score"
  if(max == TRUE) {
    #The default is for each subject's "best score" to be the maximum value he or she records (e.g. power/force)
    df <- group_by(df, Subject)
    df <- summarise(df, across(where(is.numeric), ~ max(.x)))
  } else {
    #Otherwise, each subject's "best score" is the minimum value he or she records (e.g. 40-yard dash times)
    df <- group_by(df, Subject)
    df <- summarise(df, across(where(is.numeric), ~ min(.x)))
  }

  #This line calculates the standard deviation of all of the columns (i.e. the between-subject SD of the best scores)
  sd_best = lapply(df[,-1], sd)

  #A for loop, iterating over all items in sd_best created above
  for(i in seq_along(sd_best)) {
    #Creates Smallest Worthwhile Change for each item in sd_best (the double brackets refer to an item in a list)
    SWC = 0.2*sd_best[[i]]

    #Forms a list of these SWC values, as was previously done for the TE values
    list_SWC <- list(SWC)

    #Keeps the labeling of each item in this list consistent with the TE list
    names(list_SWC) <- paste("Smallest Worthwhile Change", colnames(df)[i+1], sep = "--")
    print(list_SWC)
  }

}
