#UBER CASE STUDY

#1.Setting file path & importing required libraries
setwd("/media/newhd/Data Science/PGDDA IIITB Docs/Course2_StatisticsAndEDA/Assignment_Uber_Supply_Demand_Gap")
getwd()
library(ggplot2)
library(stringr)
library(dplyr)
library(tidyr)
library(lubridate) 

#2.Reading data in R data frame & obeserving it 
uber=read.csv("Uber Request Data.csv")
View(uber)
summary(uber)
str(uber)

#3.Data Cleaning
#Removing Request ID column as its not relevent for data analysis
uber=uber[,2:6]

#Changing Request & Drop Timestamps to a consistent format
uber$Request.timestamp=parse_date_time(x = uber$Request.timestamp, orders = c("d/m/Y H:M","d-m-Y H:M:S"),locale = "eng")
uber$Drop.timestamp=parse_date_time(x = uber$Drop.timestamp, orders = c("d/m/Y H:M","d-m-Y H:M:S"),locale = "eng")
uber$Request.timestamp=strptime(uber$Request.timestamp, "%Y-%m-%d %H:%M:%OS")
uber$Drop.timestamp=strptime(uber$Drop.timestamp, "%Y-%m-%d %H:%M:%OS")

#Creating TripTime column as difference between drop timestamp & request timestamp
uber$TripTime=uber$Drop.timestamp-uber$Request.timestamp

#Creating new column RequestMeet, Here 'No Cars Available' & 'Cancelled' status are Failure(0) & 'Trip Completed' is Success(1)
uber$RequestMeet=ifelse(uber$Status=="Trip Completed",1,0)

#4.Exploratory Data Analysis
#Note: I have covered large part of EDA through Tableau, please check ppt & Tableau file for complete EDA details
#Univariate analysis
#Calculating demand supply gap
gap=sum(uber$RequestMeet==1)/6745
gap #gap is .4197 which shows that only 42% requests were meet successfully

#Barplot showing gap with total number of request meet to those which were not meet
ggplot(uber, aes(x = RequestMeet)) + geom_histogram(binwidth = 1) 

#Average time of trips between airport & city
average_trip_time=mean(uber$TripTime,na.rm=TRUE)
average_trip_time
#Trip time have almost normal distribution with mean at 52.41
ggplot(uber, aes(x = TripTime)) + geom_histogram(binwidth = 1)

#Bivariate Analysis
#Requests Meet when pickup point was Airport or City
ggplot(uber, aes(x = RequestMeet, fill = Pickup.point))+ geom_histogram(binwidth = 1) 

uber_airport_pickup=subset(uber,uber$Pickup.point=="Airport") #3283 observations
uber_city_pickup=subset(uber,uber$Pickup.point=="City") #3507 observations

#There is more supply demand gap for cab availability from airport, but overall there is not much diffrence, this is further investegated by time of day in Tableau
gap_airport=sum(uber_airport_pickup$RequestMeet==1)/3283
gap_airport #40.42 percent
gap_city=sum(uber_city_pickup$RequestMeet==1)/3507
gap_city #42.88 percent

#Writing data prepared in csv format for visualization in Tableau
write.table(uber, "uber.txt", sep=",")

