#Car Price Assignment
#------------------------------------------Data Sourcing -----------------------------------------------------
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(car)
library(MASS)

cars<-read.csv("CarPrice_Assignment.csv")
View(cars)

#------------------------------------------Data Understanding & EDA-----------------------------------------------------
#Checking structure & summary of dataset
str(cars) #Dataset contains many categorical variables which will be converted to dummy variables before regression
summary(cars) #Summary shows that NA values are absent from dataset

#Checking covariance & correlation matrix of data
cor(cars[sapply(cars, is.numeric)]) #enginesize,curbweight,horseposer,carwidth,carlength are highy correlated to price
cov(cars[sapply(cars, is.numeric)])

#Plotting the data
#1.Univariate Analysis: Boxplots
boxplot(cars$price) #price contains outliers
boxplot(price ~ symboling:highwaympg, data = cars)
boxplot(cars, main = "boxplot(*, horizontal = TRUE)", horizontal = TRUE)

#2.Bivariate Analysis between categorical variables & price
#a)Average car price by fueltype, Diesel cars have much higher average price
carprice_fueltype <- group_by(cars, fueltype) %>% summarise(mean(price, na.rm = T)) %>% setNames(.,c('Fuel_Type','Avg_Price'))
ggplot(carprice_fueltype, aes(x=Fuel_Type, y=Avg_Price)) + geom_boxplot()
#b)Average car price by drivewheel, rwd cars have much higher average price
carprice_drivewheel <- group_by(cars, drivewheel) %>% summarise(mean(price, na.rm = T)) %>% setNames(.,c('Drivewheel','Avg_Price'))
ggplot(carprice_drivewheel, aes(x=Drivewheel, y=Avg_Price)) + geom_boxplot()
#c)Average car price by enginelocation,cars with rear engines have much higher average price
carprice_enginelocation <- group_by(cars, enginelocation) %>% summarise(mean(price, na.rm = T)) %>% setNames(.,c('enginelocation','Avg_Price'))
ggplot(carprice_enginelocation, aes(x=enginelocation, y=Avg_Price)) + geom_boxplot()
#d)Average car price by cylindernumber,cars with 8 & 12 engines have much higher average price
carprice_cylindernumber <- group_by(cars, cylindernumber) %>% summarise(mean(price, na.rm = T)) %>% setNames(.,c('cylindernumber','Avg_Price'))
ggplot(carprice_cylindernumber, aes(x=cylindernumber, y=Avg_Price)) + geom_boxplot()
#e)Average car price by fuelsystem,cars with mpfi much higher average price & 2bbl,2bbl have much lower average price
carprice_fuelsystem <- group_by(cars, fuelsystem) %>% summarise(mean(price, na.rm = T)) %>% setNames(.,c('fuelsystem','Avg_Price'))
ggplot(carprice_fuelsystem, aes(x=fuelsystem, y=Avg_Price)) + geom_boxplot()
#f)Average car price by aspiration,turbo have higher average price
carprice_aspiration <- group_by(cars, aspiration) %>% summarise(mean(price, na.rm = T)) %>% setNames(.,c('aspiration','Avg_Price'))
ggplot(carprice_aspiration, aes(x=aspiration, y=Avg_Price)) + geom_boxplot()
#g)Average car price by carbody,convertible & hardtop have higher average price
carprice_carbody <- group_by(cars, carbody) %>% summarise(mean(price, na.rm = T)) %>% setNames(.,c('carbody','Avg_Price'))
ggplot(carprice_carbody, aes(x=carbody, y=Avg_Price)) + geom_boxplot()
#h)Average car price by doornumber,cars with four doors have higher average price
carprice_doornumber <- group_by(cars, doornumber) %>% summarise(mean(price, na.rm = T)) %>% setNames(.,c('doornumber','Avg_Price'))
ggplot(carprice_doornumber, aes(x=doornumber, y=Avg_Price)) + geom_boxplot()
#i)Average car price by enginetype,cars with dohcv have higher average price
carprice_enginetype <- group_by(cars, enginetype) %>% summarise(mean(price, na.rm = T)) %>% setNames(.,c('enginetype','Avg_Price'))
ggplot(carprice_enginetype, aes(x=enginetype, y=Avg_Price)) + geom_boxplot()

#------------------------------------------Data Prepration--------------------------------------------------------
#Checking unique values
sapply(cars[sapply(cars,is.factor)],levels)
length(levels(cars$CarName))==length(unique(cars$CarName))

#Dummmy Variables: Converting all categorical variables to numeric levels
summary(cars$fueltype)
levels(cars$fueltype)<-c(1,0)
cars$fueltype<-as.numeric(levels(cars$fueltype))[cars$fueltype]

summary(cars$doornumber)
levels(cars$doornumber)<-c(1,0)
cars$doornumber<-as.numeric(levels(cars$doornumber))[cars$doornumber]

summary(cars$aspiration)
levels(cars$aspiration)<-c(1,0)
cars$aspiration<-as.numeric(levels(cars$aspiration))[cars$aspiration]

summary(factor(cars$enginelocation))
levels(cars$enginelocation)<-c(1,0)
cars$enginelocation<-as.numeric(levels(cars$enginelocation))[cars$enginelocation]

summary(factor(cars$carbody))
dummy1 <- data.frame(model.matrix( ~carbody, data = cars))
dummy1 <- dummy1[,-1]
cars<-cbind(subset(cars, select=-c(carbody)),dummy1)

summary(factor(cars$drivewheel))
dummy2<-data.frame(model.matrix(~drivewheel,data = cars))
dummy2<-dummy2[,-1]
cars<-cbind(subset(cars, select=-c(drivewheel)),dummy2)

summary(factor(cars$enginetype))
dummy3<-data.frame(model.matrix(~enginetype,data = cars))
dummy3<-dummy3[,-1]
cars<-cbind(subset(cars, select=-c(enginetype)),dummy3)

summary(factor(cars$cylindernumber))
dummy4<-data.frame(model.matrix(~cylindernumber,data = cars))
dummy4<-dummy4[,-1]
cars<-cbind(subset(cars, select=-c(cylindernumber)),dummy4)

summary(cars$fuelsystem)
dummy5<-data.frame(model.matrix(~fuelsystem,data=cars))
dummy5<-dummy5[,-1]
cars<-cbind(subset(cars, select=-c(fuelsystem)),dummy5)

str(cars$symboling)
cars$symboling<-as.factor(cars$symboling)
dummy6<-data.frame(model.matrix(~symboling,data=cars))
dummy6<-dummy6[,-1]
cars<-cbind(subset(cars, select=-c(symboling)),dummy6)

#Again checking covariance & correlation matrix of data with all numeric variables
cor(cars[sapply(cars, is.numeric)]) #
cov(cars[sapply(cars, is.numeric)])

#Derived columns
#manufacturer colum is derived from car name using below regular expression
cars$manufacturer<-as.factor(gsub("([A-Za-z]+).*", "\\1", cars$CarName))
#cars <- cars[ ,c(1:25,27,26)]
unique(cars$manufacturer) #Manufacturer names still contain duplicates due to spelling difference            
length(unique(cars$manufacturer))             
cars$manufacturer[cars$manufacturer == "nissan"] <- "Nissan" 
cars$manufacturer[cars$manufacturer == "maxda"] <- "mazda"
cars$manufacturer[cars$manufacturer == "porcshce"] <- "porsche"
cars$manufacturer[cars$manufacturer == "toyouta"] <- "toyota"
cars$manufacturer[cars$manufacturer == "vokswagen"] <- "volkswagen"

#Average car price by manufacturer,buik, jaguar & prosche are most expensive brands(part from EDA as manufacturer derived here)
carprice_manufacturer <- group_by(cars, manufacturer) %>% summarise(mean(price, na.rm = T)) %>% setNames(.,c('manufacturer','Avg_Price'))
ggplot(carprice_manufacturer, aes(x=manufacturer, y=Avg_Price)) + geom_boxplot()
boxplot(price~manufacturer,data=cars, main="Cars Data", xlab="price", ylab="manufacturer")

#Converting manufacturer levels to numeric for regression analysis
#cars$manufacturer<-as.numeric(cars$manufacturer) 

summary(cars$manufacturer)
dummy7<-data.frame(model.matrix( ~manufacturer, data = cars))
dummy7 <- dummy7[,-1]
cars<-cbind(subset(cars, select=-c(CarName,manufacturer)),dummy7)

#Droping unneccessary column car_ID
cars<-subset(cars,TRUE,-c(car_ID))

#------------------------------------------Model Building---------------------------------------------------------
# Dividing into training and test data set
#set the seed to 100, let's run it 
set.seed(100)
#Randomly generating row indices for train dataset
trainindices= sample(1:nrow(cars), 0.7*nrow(cars))
#Generating the train data set
cars_train = cars[trainindices,]
#Similarly storing the rest of the observations into an object "cars_test".
cars_test = cars[-trainindices,]

#Modeling using Stepwise Selection Method using StepAIC function
model_1 <- lm(formula = price~., data = cars_train)
summary(model_1) #
vif(model_1)

step <- stepAIC(model_1, direction="both")

#Creating new model only keeping variables suggested by stepAIC function
cars_subset1<-subset(cars_train,TRUE,c(manufactureraudi,manufacturerjaguar,carbodyhardtop,manufacturerisuzu,carbodysedan,
                                               fuelsystemmpfi,carbodyhatchback,carbodywagon,symboling1,manufacturervw,cylindernumberfive,
                                               cylindernumberthree,citympg,fuelsystem2bbl,manufacturervolvo,manufacturerbuick,manufacturerhonda,
                                               manufacturersaab,manufacturermercury,stroke,curbweight,peakrpm,drivewheelrwd,manufacturerrenault,
                                               manufacturervolkswagen,carwidth,enginetypel,manufacturerbmw,manufacturertoyota,manufacturerNissan,
                                               manufacturerplymouth,manufacturermazda,manufacturerdodge,aspiration,enginetypeohcf,manufacturermitsubishi,
                                               enginetyperotor,enginesize,enginelocation,price))
model_2 <- lm(formula = price~., data = cars_subset1)
summary(model_2)  #Multiple R-squared:  0.981,	Adjusted R-squared:  0.9738 
sort(vif(model_2))

#Removing carbodysedan with high VIF(22.077148) & moderate p-value(0.127312)
cars_subset2<-subset(cars_subset1,TRUE,-c(carbodysedan))
model_3 <- lm(formula = price~., data = cars_subset2)
summary(model_3) #Multiple R-squared:  0.9806,	Adjusted R-squared:  0.9735 
sort(vif(model_3))

#Removing curbweight with high VIF(26.561139)
cars_subset3<-subset(cars_subset2,TRUE,-c(curbweight))
model_4 <- lm(formula = price~., data = cars_subset3)
summary(model_4) #  
sort(vif(model_4))

#Removing carbodywagon with high p value>.5(0.776918)
cars_subset4<-subset(cars_subset3,TRUE,-c(carbodywagon))
model_5 <- lm(formula = price~., data = cars_subset4)
summary(model_5) #Multiple R-squared:  0.9784,	Adjusted R-squared:  0.9711 
sort(vif(model_5))

#Removing symboling1 with high p value~.5(0.496684)
cars_subset5<-subset(cars_subset4,TRUE,-c(symboling1))
model_6 <- lm(formula = price~., data = cars_subset5)
summary(model_6) #Multiple R-squared:  0.9783,	Adjusted R-squared:  0.9712 
sort(vif(model_6))

#Removing citympg with high VIF(6.681703) & p value(0.408317)
cars_subset6<-subset(cars_subset5,TRUE,-c(citympg))
model_7 <- lm(formula = price~., data = cars_subset6)
summary(model_7) #Multiple R-squared:  0.9782,	Adjusted R-squared:  0.9713
sort(vif(model_7))

#Removing fuelsystemmpfi with high VIF(6.681703) & moderately high p value(0.288763)
cars_subset7<-subset(cars_subset6,TRUE,-c(fuelsystemmpfi))
model_8 <- lm(formula = price~., data = cars_subset7)
summary(model_8) #Multiple R-squared:  0.9779,	Adjusted R-squared:  0.9713 
sort(vif(model_8))

#Removing fuelsystem2bbl with moderate VIF(3.307561) & moderately high p value(0.197464)
cars_subset8<-subset(cars_subset7,TRUE,-c(fuelsystem2bbl))
model_9 <- lm(formula = price~., data = cars_subset8)
summary(model_9) #Multiple R-squared:  0.9776,	Adjusted R-squared:  0.9711 
sort(vif(model_9))

#Removing carbodyhardtop with moderately high p value(0.286611)
cars_subset9<-subset(cars_subset8,TRUE,-c(carbodyhardtop))
model_10 <- lm(formula = price~., data = cars_subset9)
summary(model_10) #Multiple R-squared:  0.9774,	Adjusted R-squared:  0.971 
sort(vif(model_10))

#Removing carbodyhatchback with moderately high p value(0.156304)
cars_subset10<-subset(cars_subset9,TRUE,-c(carbodyhatchback))
model_11 <- lm(formula = price~., data = cars_subset10)
summary(model_11) #Multiple R-squared:  0.977,	Adjusted R-squared:  0.9708 
sort(vif(model_11))

#Removing manufacturervw with moderately high p value(0.106843)
cars_subset11<-subset(cars_subset10,TRUE,-c(manufacturervw))
model_12 <- lm(formula = price~., data = cars_subset11)
summary(model_12) #Multiple R-squared:  0.9764,	Adjusted R-squared:  0.9704 
sort(vif(model_12))

#Removing manufactureraudi with moderately high p value(0.271704)
cars_subset12<-subset(cars_subset11,TRUE,-c(manufactureraudi))
model_13 <- lm(formula = price~., data = cars_subset12)
summary(model_13) #Multiple R-squared:  0.9762,	Adjusted R-squared:  0.9703 
sort(vif(model_13))

#Removing manufacturerisuzu with moderately high p value(0.198947)
cars_subset13<-subset(cars_subset12,TRUE,-c(manufacturerisuzu))
model_14 <- lm(formula = price~., data = cars_subset13)
summary(model_14) #Multiple R-squared:  0.9758,	Adjusted R-squared:  0.9701 
sort(vif(model_14))

#Removing manufacturervolvo with moderate VIF(2.559133) & moderate p value(0.031016)
cars_subset14<-subset(cars_subset13,TRUE,-c(manufacturervolvo))
model_15 <- lm(formula = price~., data = cars_subset14)
summary(model_15) #Multiple R-squared:  0.9748,	Adjusted R-squared:  0.9692 
sort(vif(model_15))

#Removing cylindernumberthree with moderate p value(0.050157)
cars_subset15<-subset(cars_subset14,TRUE,-c(cylindernumberthree))
model_16 <- lm(formula = price~., data = cars_subset15)
summary(model_16) #Multiple R-squared:  0.9739,	Adjusted R-squared:  0.9684  
sort(vif(model_16))

#Removing manufacturermercury with moderate p value(0.059828)
cars_subset16<-subset(cars_subset15,TRUE,-c(manufacturermercury))
model_17 <- lm(formula = price~., data = cars_subset16)
summary(model_17) #Multiple R-squared:  0.9731,	Adjusted R-squared:  0.9677  
sort(vif(model_17))

#Removing cylindernumberfive with moderate VIF(2.071614) & moderate p value(0.069994)
cars_subset17<-subset(cars_subset16,TRUE,-c(cylindernumberfive))
model_18 <- lm(formula = price~., data = cars_subset17)
summary(model_18) #Multiple R-squared:  0.9724,	Adjusted R-squared:  0.967  
sort(vif(model_18))

#Removing enginetypel which now have moderately high p value(0.109429)
cars_subset18<-subset(cars_subset17,TRUE,-c(enginetypel))
model_19 <- lm(formula = price~., data = cars_subset18)
summary(model_19) #Multiple R-squared:  0.9718,	Adjusted R-squared:  0.9666   
sort(vif(model_19))

#Removing manufacturersaab with moderate p value(0.079184)
cars_subset19<-subset(cars_subset18,TRUE,-c(manufacturersaab))
model_20 <- lm(formula = price~., data = cars_subset19)
summary(model_20) #Multiple R-squared:  0.971,	Adjusted R-squared:  0.966  
sort(vif(model_20))

#Removing manufacturervolkswagen with p value(0.001439**)
cars_subset20<-subset(cars_subset19,TRUE,-c(manufacturervolkswagen))
model_21 <- lm(formula = price~., data = cars_subset20)
summary(model_21) #Multiple R-squared:  0.9685,	Adjusted R-squared:  0.9633  
sort(vif(model_21))

#Removing manufacturerhonda which now have moderate VIF(2.080291) & moderate p value(0.028214)
cars_subset21<-subset(cars_subset20,TRUE,-c(manufacturerhonda))
model_22 <- lm(formula = price~., data = cars_subset21)
summary(model_22) #Multiple R-squared:  0.9672,	Adjusted R-squared:  0.9622  
sort(vif(model_22))

#Removing manufacturerrenault which now have moderately p value(0.08405)
cars_subset22<-subset(cars_subset21,TRUE,-c(manufacturerrenault))
model_23 <- lm(formula = price~., data = cars_subset22)
summary(model_23) #Multiple R-squared:  0.9664,	Adjusted R-squared:  0.9615   
sort(vif(model_23))

#Removing manufacturertoyota which now have moderate p value(0.01488)
cars_subset23<-subset(cars_subset22,TRUE,-c(manufacturertoyota))
model_24 <- lm(formula = price~., data = cars_subset23)
summary(model_24) #Multiple R-squared:  0.9648,	Adjusted R-squared:   0.96  
sort(vif(model_24))

#Removing manufacturerNissan which now have moderate p value(0.068691)
cars_subset24<-subset(cars_subset23,TRUE,-c(manufacturerNissan))
model_25 <- lm(formula = price~., data = cars_subset24)
summary(model_25) #Multiple R-squared:  0.9638,	Adjusted R-squared:  0.9592 
sort(vif(model_25))

#Removing manufacturermazda which now have moderate p value(0.049745)
cars_subset25<-subset(cars_subset24,TRUE,-c(manufacturermazda))
model_26 <- lm(formula = price~., data = cars_subset25)
summary(model_26) #Multiple R-squared:  0.9627,	Adjusted R-squared:  0.9583 
sort(vif(model_26))

#Removing manufacturerplymouth which now have moderate p value(0.032350)
cars_subset26<-subset(cars_subset25,TRUE,-c(manufacturerplymouth))
model_27 <- lm(formula = price~., data = cars_subset26)
summary(model_27) #Multiple R-squared:  0.9613,	Adjusted R-squared:  0.9571 
sort(vif(model_27))

#Removing manufacturerdodge which now have moderate p value(0.034691)
cars_subset27<-subset(cars_subset26,TRUE,-c(manufacturerdodge))
model_28 <- lm(formula = price~., data = cars_subset27)
summary(model_28) #Multiple R-squared:  0.9599,	Adjusted R-squared:  0.9559 
sort(vif(model_28))

#Stopping at model_28 as all p-values are significant now & Adjusted R Squared is very good 0.9559, with 13 independent variables
#Testing model
#Predicting the house prices in the testing dataset
Predict_1 <- predict(model_12,cars_test)
cars_test$test_price_stepAIC <- Predict_1
#Accuracy of the predictions
#Calculating correlation
r <- cor(cars_test$price,cars_test$test_price_stepAIC)
#Calculating R squared by squaring correlation
rsquared <- cor(cars_test$price,cars_test$test_price_stepAIC)^2
#Checking R-squared
rsquared #0.8186977


