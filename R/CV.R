#This function computes the coefficient of variation for each athlete's measurements (expressed as a percentage)

library(tidyverse)

CV <- function(data){
#Renames the first column as "Subject", so that it can be referred to in the group_by function later
  colnames(data)[1] <- "Subject"
#These three lines calculate the standard deviation and mean that each subject records for each numeric column (i.e. for each metric)
  data <- group_by(data, Subject)
  data1 <- summarize_if(data, is.numeric, funs(sd, mean))
  data1 <- data1[,-1]

#Creates a variable that equals the number of metrics in the dataset, which makes the following for loop easier
  num_metrics = ncol(data1)/2
#I create the list now, which makes it easier to append it in the for loop that follows
  list_CV <- list()

#This for loop iterates over just the first half of the columns in the dataset
  for(i in 1:num_metrics){
#This line renames the first three columns of the new dataset in a way that makes the names of the list more intuitive
    colnames(data1)[i] <- paste("CV Percentage Change", colnames(data)[i+2], sep = "--")
#With "i + num_metrics" I match up the column that is the sd of a metric with the column that is the mean of the same metric
    CV = (data1[,i]/data1[,i+num_metrics])*100
#I append the CV's to the list that I created earlier
    list_CV <- append(list_CV, values = CV)
#This line calculates the mean value of each item in the list (i.e. the mean of all athlete-level CV's for each metric)
    mean_CV <- lapply(list_CV, mean)
  }

#Print out the result of the lapply function so that we can see all of its contents
  print(mean_CV)
}

#We again need to group the original dataset by subject, but this time calculate the maximum value that each subject recorded
  data2<-group_by(data, "Subject")
  data2<-summarize_if(data, is.numeric, max)
#The average of these best scores is computed via the line of code below
  best_avg = lapply(data2[,-1], mean)

#We need another for loop, this time to iterate over the entire best_avg list we just created
  for (i in seq_along(best_avg)){
#The CV change we care about is the average of the best scores divided by 100 and multiplied by the CV we calculated in the previous list
    CV_change = (best_avg[[i]]/100)*(mean_CV[[i]])
#Just multiply this value by 2 to obtain double the CV
    Dbl_CV_change = CV_change*2
#Create a list for each of these statistics
    list_CV_change<-list(CV_change)
    list_Dbl_CV_change<-list(Dbl_CV_change)
#Name each element of each list with the statistic it represents (i.e. CV Change or Double CV Change) and the metric it represents
    names(list_CV_change)<-paste("CV Change", colnames(data2)[i+1], sep = "--")
    names(list_Dbl_CV_change)<-paste("Double CV Change", colnames(data2)[i+1], sep = "--")
#Explicitly print both of the lists so that we see their full contents in the output of the function
    print(list_CV_change)
    print(list_Dbl_CV_change)
  }
