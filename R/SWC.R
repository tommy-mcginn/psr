#These functions allow for multiple metrics and compute the Smallest Worthwhile Change (SWC) for each metric
#The subject and trial must be the first and second columns, respectively, in the input data to the function
#The remaining columns should be the measurements of the various metrics for which we want to compute the SWC

#This function is meant for metrics where the highest score is best (e.g. measurements of force or power)
SWC_max <- function(Subject, Metric){
#Renames the first column as "Subject", so that it can be referred to in the group_by function later
colnames(data)[1] <- "Subject"
#These two lines calculate the maximum value ("best score") that each subject records for each numeric column (i.e. each metric)
data <- group_by(data, Subject)
data <- summarize_if(data, is.numeric, max)
#Applies the standard deviation function to all columns except the Subject column
sd_best = lapply(data[,-1], sd)

#A for loop, iterating over all items in sd_best created above
for(i in seq_along(sd_best)){
  #Creates Smallest Worthwhile Change for each item in sd_best (the double brackets refer to an item in a list)
  SWC = 0.2*sd_best[[i]]
  #Forms a list of these SWC values, as was previously done for the TE values
  list_SWC <- list(SWC)
  #Keeps the labeling of each item in this list consistent with the TE list
  names(list_SWC) <- paste("Smallest Worthwhile Change", colnames(data)[i+1], sep = "--")
  print(list_SWC)
  }
}

#This function is exactly the same as the one above, but it is meant for metrics where the lowest score is best (e.g. 40-yard dash times)
SWC_min <- function(data){
  colnames(data)[1] <- "Subject"
  #These two lines calculate the minimum value ("best score") that each subject records for each numeric column (i.e. each metric)
  data <- group_by(data, Subject)
  data <- summarize_if(data, is.numeric, min)
  sd_best = lapply(data[,-1], sd)

  for(i in seq_along(sd_best)){
    SWC = 0.2*sd_best[[i]]
    list_SWC <- list(SWC)
    names(list_SWC) <- paste("Smallest Worthwhile Change", colnames(data)[i+1], sep = "--")
    print(list_SWC)
  }
}
