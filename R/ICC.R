#The function that computes the ICC of each metric in the dataset
#The first column must represent the subject/person, the second column the trial, and the remaining columns the various metrics

library(tidyverse)
library(psych)

I_C_C <- function(data){
#This for loop iterates over the columns in the dataset that represent the metrics for which we want to compute the ICC
  for(i in 3:ncol(data)){
#Renames the first and second columns in a way that allows us to easily refer to them later in the for loop
    colnames(data)[1] <- "Subject"
    colnames(data)[2] <- "Trial"
#For each loop, we want to make a new dataset (data1) with only three columns: Subject, Trial, and the ith metric in the for loop
    data1 <- select(data, "Subject", "Trial", colnames(data)[i])
#This pivot_wider function is necessary in order to use the ICC function later
    data1 <- pivot_wider(data1, names_from = "Trial", values_from = colnames(data)[i])
#After this step, we now have a dataset with each column representing each trial, with the ith metric being the numeric values in the table
    data1 <- data1[,-1]
#The ICC function in the psych package that computes the ICC for each metric
    ICC = ICC(data1)
#Put each ICC output we get into a list, so that we can see it all at once upon calling the function
    list_ICC <- list(ICC)
#Names each item in the list properly, as the ICC Output of its respective metric (which is what colnames(data)[i] represents)
    names(list_ICC) <- paste("ICC Output", colnames(data)[i], sep = "--")
#We must specifically print the list in order to see all of its contents when making it inside of a for loop as we have done here
    print(list_ICC)
  }
}
