##GRIP @ The Sparks Foundation
#Task 2
#From the given 'Iris' dataset, 
#predict the optimum number of  clusters and represent it visually. 


##Important packages
install.packages("stats")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("ggfortify")
install.packages("NbClust")

##Important librarys
library(stats)
library(dplyr)
library(ggplot2)
library(ggfortify)
library(NbClust)

##Import Data
library(readr)
Iris <- read_csv("C:/Users/VIVEK CHAUHAN/Downloads/Iris.csv")
View(Iris)

##Select Relevent Column by creating Mydata
Mydata=select(Iris,c(2,3,4,5))
Mydata

##creating function wssplot (this below function is copied from google)
#With-in-Sum-of-Squares (WSS)
wssplot <- function(data, nc=30, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")}

##Predicting the no. of Clusters, 
#Spotting the kink in the Curve to choose Optimum No. Of Clusters
wssplot(Mydata)

##Using Kmeans for making Clusters
#Through Wssplot we can Predict that Optimum no. of Clusters are 3
KM=kmeans(Mydata,3)
KM
KM$cluster
KM$size
table(Iris$Species,KM$cluster)


##Ploting The Clusters for Visualization
autoplot(KM,Mydata,frame=TRUE)
#View Centers
KM$centers

  
        