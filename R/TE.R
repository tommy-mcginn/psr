#This function allows for multiple metrics and computes the Typical Error (TE) for each metric
#The subject and trial must be the first and second columns, respectively, in the input dataset to the function
#The remaining columns should be the measurements of the various metrics for which we want to compute the TE

library(tidyverse)

TE <- function(data){
#The first column must represent each subject, while the second column must correspond to which trial the measurement was taken from
  Subject <- data[,1]
  Trial <- data[,2]

#A for loop, which allows for iteration over all of the columns that represent the various metrics in the dataset
  for(i in 3:ncol(data)){
#For each iteration of the for loop, the ith column is the metric of interest
    Metric <- data[,i]
#The Typical Error is the residual standard error (sigma) of the following regression
    lm_TE <- lm(unlist(Metric) ~ as.factor(unlist(Subject)) + as.factor(unlist(Trial)), data)
    TE = sigma(lm_TE)
#In order to express all of these Typical Error values in a way that is easy to follow, form a list that contains these values
    list_TE <- list(TE)
#Ensures each Typical Error value in the list is labeled as "Typical Error--Metric"
    names(list_TE) <- paste("Typical Error", colnames(data)[i], sep = "--")
#The print function must be utilized here to show the entire list in the output
    print(list_TE)
  }
}
