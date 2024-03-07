setwd("/Users/Carlo/OneDrive/CSU Global/MIS470 - Data Science Foundation")
my_train<-read.csv("MIS470HousingTraining(1000x25).csv",header=TRUE)
my_test<-read.csv("MIS470Housingtesting(460x25).csv",header=TRUE)

#2 Summary Statistics
summary(my_test$SalePrice) 
sd(my_test$SalePrice)

#3 Histogram of the Sales Price variable from the Testing dataset
options(scipen = 1000)
hist(my_test$SalePrice, 
     main ='Histogram of the Sales Price Variable from the Testing dataset',
     xlab = 'Sales Price')

## Combine the training and testing dataset
my_train1<-my_train[-c(1)]
my_test1<-my_test[-c(1)]
install.packages("gdata")
library(gdata)
my_data<-combine(my_train1, my_test1)

#3.1 Histogram of the Sales Price variable from the Combined dataset
options(scipen = 1000)
hist(my_data$SalePrice, 
     main ='Histogram of the Sales Price Variable from the Combined dataset',
     xlab = 'Sales Price')

#4 Linear regression model from the Training dataset
model<-lm(my_train1$SalePrice~.,data=my_train1)
summary(model)

## Create a 2nd testing dataset without rows with missing values (NA)
my_test2<-my_test1[complete.cases(my_test1),]

#5 Predict the sale price from the first 20 rows from 2nd testing dataset
predict(model, head(my_test2,20))

#5.1 Mean Absolute Percentage Error (MAPE) for Estimate
error<-head(my_test2$SalePrice,20)-predict(model, head(my_test2,20))
MAPE<-mean(abs(100*error/head(my_test2$SalePrice,20)))
MAPE