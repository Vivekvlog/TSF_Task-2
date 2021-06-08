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

##Select Relevent Column by creating new data set "Mydata"
Mydata=select(Iris,c(2,3,4,5))
Mydata

##Looking for NA values
sum(is.na(Mydata))

plot(Mydata,col="blue")

##Checking for Outliers
boxplot(Mydata)
#We found Outliers in SepalWidthCm

##Removing Outliers
summary(Mydata$SepalWidthCm)

#Inner Quartile Range of SepalLengthCm
IQR_SepalWidthCm=3.300-2.800

#Upper Limit for Outliers SepalWidthCm
Upfen_SepalWidthCm=3.300+1.5*IQR_SepalWidthCm
Upfen_SepalWidthCm

#Removing Outliers above Upper Limit and Checking by Ploting data
summary(Mydata)
Mydata_clean1=subset(Mydata,SepalLengthCm<=7.900 & SepalWidthCm<=4.05 & PetalLengthCm <=6.900 & PetalWidthCm<=2.500)
boxplot(Mydata_clean1)

#Lower Limit for Outliers from SepalWidthCm
Lofen_SepalWidthCm=2.800-1.5*IQR_SepalWidthCm
Lofen_SepalWidthCm

#Removing Outliers below Lower Limit and checking by Ploting data
Mydata_clean2=subset(Mydata_clean1,SepalLengthCm<=7.900 & SepalWidthCm >=2.05 & PetalLengthCm <=6.900 & PetalWidthCm<=2.500)
Mydata_clean2
boxplot(Mydata_clean2)

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
wssplot(Mydata_clean2)
print("By looking at WSS plot, we can see that the kink in the curve is at point 3  
             So we can say that the Optimum number of Clusters are 3")

##Using Kmeans for making Clusters
#Through Wssplot we can Predict that Optimum no. of Clusters are 3
KM=kmeans(Mydata_clean2,3)
KM
KM$cluster
KM$size

##Ploting The Clusters for Visualization
autoplot(KM,Mydata_clean2,frame=TRUE)
#View Centers
KM$centers
