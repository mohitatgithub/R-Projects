#--------------------------------------HR Analytics Case Study--------------------------------------------------
library(dplyr)
library(ggplot2)
library(stringr)
library(cowplot)
library(caTools)
library(plotly)
#------------------------------------------Data Sourcing--------------------------------------------------------
setwd("/media/newhd/Data Science/PGDDA IIITB Docs/Course3_PredictiveAnalytics1/HR Analytics Case Study")
emp_survey<-read.csv("employee_survey_data.csv")
manager_survey<-read.csv("manager_survey_data.csv")
general_data<-read.csv("general_data.csv")
in_time<-read.csv("in_time.csv")
out_time<-read.csv("out_time.csv")

View(emp_survey)
View(manager_survey)
View(general_data)
View(in_time)
View(out_time)

#-------------------------------------EDA & Data Prepration----------------------------------------------------
#Checking missing values
sum(is.na(emp_survey))  #83 NA values found
summary(emp_survey) #EnvironmentSatisfaction, JobSatisfaction, WorkLifeBalance have 25, 20 & 38 NA values respectively
sum(is.na(manager_survey)) #No NA values found
sum(is.na(general_data)) #28 NA values found
summary(general_data) #NumCompaniesWorked, TotalWorkingYears have NA 19 & 9 NA values respectively

#Checking duplicate values
which(duplicated(emp_survey))
which(duplicated(manager_survey))
which(duplicated(general_data))
#Notes: No duplicate value found

#Collate the data together in one single file
length(unique(tolower(emp_survey$EmployeeID)))     #4410, confirming EmployeeID is key in emp_survey 
length(unique(tolower(manager_survey$EmployeeID))) #4410, confirming EmployeeID is key in manager_survey
length(unique(tolower(general_data$EmployeeID)))   #4410, confirming EmployeeID is key in general_data

setdiff(emp_survey$EmployeeID,manager_survey$EmployeeID)   # Identical EmployeeID across these datasets
setdiff(manager_survey$EmployeeID,general_data$EmployeeID) # Identical EmployeeID across these datasets

hr<- merge(emp_survey,manager_survey, by="EmployeeID", all = F)
hr<- merge(hr,general_data, by="EmployeeID", all = F)

View(hr) #master file
#write.csv(hr, file = "hr.csv")

#Outlier detection
outlierKD <- function(dt, var) {
  var_name <- eval(substitute(var),eval(dt))
  na1 <- sum(is.na(var_name))
  m1 <- mean(var_name, na.rm = T)
  par(mfrow=c(2, 2), oma=c(0,0,3,0))
  boxplot(var_name, main="With outliers")
  hist(var_name, main="With outliers", xlab=NA, ylab=NA)
  outlier <- boxplot.stats(var_name)$out
  mo <- mean(outlier)
  var_name <- ifelse(var_name %in% outlier, NA, var_name)
  boxplot(var_name, main="Without outliers")
  hist(var_name, main="Without outliers", xlab=NA, ylab=NA)
  title("Outlier Check", outer=TRUE)
  na2 <- sum(is.na(var_name))
  cat("Outliers identified:", na2 - na1, "n")
  cat("Propotion (%) of outliers:", round((na2 - na1) / sum(!is.na(var_name))*100, 1), "n")
  cat("Mean of the outliers:", round(mo, 2), "n")
  m2 <- mean(var_name, na.rm = T)
  cat("Mean without removing outliers:", round(m1, 2), "n")
  cat("Mean if we remove outliers:", round(m2, 2), "n")
}
outlierKD(hr,Age)                #No outliers
outlierKD(hr,DistanceFromHome)   #No outliers
outlierKD(hr,MonthlyIncome)      #Contain Outliers
outlierKD(hr,PercentSalaryHike)  #No outliers
outlierKD(hr,TotalWorkingYears)  #Contain Outliers
outlierKD(hr,YearsAtCompany)     #Contain Outliers
#Notes: We have not removed outliers as they are not very significant & all values are important for analysis

#######################################################################################################
#Processing in_time & out_time
#In & out time data have 1 year working days data of login & logout times, assuming holidays & leave days are NA 
#NA value treatment
#Removing columns with all NA values assuming that those days were holidays
to_remove_in<-c(colnames(in_time)[colSums(is.na(in_time))==4410])
to_remove_out<-c(colnames(out_time)[colSums(is.na(out_time))==4410])
in_time<-in_time[,-which(names(in_time)%in%to_remove_in)]
out_time<-out_time[,-which(names(out_time)%in%to_remove_out)]

#Converting Date time to correct format
str(in_time)  #dates are in factor form
str(out_time) #dates are in factor form
temp<-as.data.frame(apply(in_time[,-1],2,as.POSIXlt))
in_time<-cbind(in_time[,1],temp)
names(in_time)[names(in_time) == "in_time[, 1]"] <- "EmployeeID" #Renaming first column as 'EmployeeID'

temp<-as.data.frame(apply(out_time[,-1],2,as.POSIXlt))
out_time<-cbind(out_time[,1],temp)
names(out_time)[names(out_time) == "out_time[, 1]"] <- "EmployeeID" #Renaming first column as 'EmployeeID'

######################################################################################################## 
#Finding Leave Patterns
#Deriving total Leaves taken by employee assuming remaining NA are Leaves taken by employee
Leaves<-c(apply(in_time, 1, function(x) sum(is.na(x))))
LeavesPerMonth<-vector()

for(i in 1:12){
LeavesMonthly<-in_time[,c(which(as.numeric(substr(names(in_time[,-1]),7,8))==i))]
LeavesPerMonth<-cbind(LeavesPerMonth,apply(LeavesMonthly, 1, function(x) sum(is.na(x))))}

LeavesPerMonth<-as.data.frame(LeavesPerMonth)
LeavesPerMonth<-cbind(LeavesPerMonth,general_data$Attrition)
AverageMonthlyLeavesYes<-c(apply(subset(LeavesPerMonth,LeavesPerMonth$`general_data$Attrition`=="Yes",select = -c(`general_data$Attrition`)),2,mean))
AverageMonthlyLeavesNo<-c(apply(subset(LeavesPerMonth,LeavesPerMonth$`general_data$Attrition`=="No",select = -c(`general_data$Attrition`)),2,mean))
AverageMonthlyLeaves<-as.data.frame(t(rbind(AverageMonthlyLeavesYes,AverageMonthlyLeavesNo)))
ggplot() +
  #Attrition=Yes, blue plot
  geom_point(data=AverageMonthlyLeaves, aes(seq_along(AverageMonthlyLeavesYes),AverageMonthlyLeavesYes)) + 
  geom_smooth(data=AverageMonthlyLeaves, aes(seq_along(AverageMonthlyLeavesYes),AverageMonthlyLeavesYes), 
              colour="blue", size=1) +
  #Attrition=No, red plot
  geom_point(data=AverageMonthlyLeaves, aes(seq_along(AverageMonthlyLeavesNo),AverageMonthlyLeavesNo)) + 
  geom_smooth(data=AverageMonthlyLeaves, aes(seq_along(AverageMonthlyLeavesNo),AverageMonthlyLeavesNo), 
              colour="red", size=1)
#Notes: Monthly Leave pattern shows that employees who leave took less leaves on average, but difference is not very significant  

############################################################################################################
#Calculating employees stay time in office
work_hours<-out_time[,-1]-in_time[,-1]
AverageWorkHours<-vector()
for(i in 1:nrow(work_hours)){AverageWorkHours<-c(AverageWorkHours,mean(as.numeric(work_hours[i,]),na.rm=TRUE))}
#apply(as.numeric(unlist(work_hours)),1,mean,na.rm=TRUE)
work_hours<-cbind(work_hours,AverageWorkHours)
work_hours<-cbind(in_time$EmployeeID,work_hours)
names(work_hours)[names(work_hours) == "in_time$EmployeeID"] <- "EmployeeID"
work_hours<-cbind(work_hours,Leaves)
work_hours<-work_hours[,c(1,251,252)]

#Merging Employee Leaves & Average stay time with hr dataframe
hr<-merge(hr, work_hours, by="EmployeeID", all = F)

#Checking percentage of missing values
pMiss <- function(x){sum(is.na(x))/length(x)*100}
sapply(hr,pMiss)

##################################################################################################################
#---------------------------------------Data Visualizations-------------------------------------------------------

#Checking Disstribution & Correlation among non categorical numeric variables
library(GGally)
ggpairs(hr[, c("AverageWorkHours","Leaves","Age","DistanceFromHome","MonthlyIncome","PercentSalaryHike")])

#Boxplots & distribion histograms of few variables
box_theme<- theme(axis.line=element_blank(),axis.title=element_blank(), 
                  axis.ticks=element_blank(), axis.text=element_blank())

box_theme_y<- theme(axis.line.y=element_blank(),axis.title.y=element_blank(), 
                    axis.ticks.y=element_blank(), axis.text.y=element_blank(),
                    legend.position="none")

plot_grid(ggplot(hr, aes(AverageWorkHours))+ geom_histogram(binwidth = 10),
          ggplot(hr, aes(x="",y=AverageWorkHours))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

plot_grid(ggplot(hr, aes(Leaves))+ geom_histogram(binwidth = 10),
          ggplot(hr, aes(x="",y=Leaves))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

plot_grid(ggplot(work_hours, aes(AverageWorkHours))+ geom_histogram(binwidth = 10),
          ggplot(work_hours, aes(x="",y=AverageWorkHours))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)


#Boxplots comparing AverageWorkHours, Leaves with Attrition status
plot_grid(ggplot(hr, aes(x=Attrition,y=AverageWorkHours, fill=Attrition))+ geom_boxplot(width=0.2)+ 
            coord_flip() +theme(legend.position="none"),
          ggplot(hr, aes(x=Attrition,y=Leaves, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          align = "v",nrow = 1)
#Notes: AverageWorkHours with Attrition=Yes is much higher compared to employees with Attrition=No but this category also have few outliers  

#Boxplots comparing Employee ratings with Attrition status
plot_grid(ggplot(hr, aes(x=Attrition,y=EnvironmentSatisfaction, fill=Attrition))+ geom_boxplot(width=0.2)+ 
            coord_flip() +theme(legend.position="none"),
          ggplot(hr, aes(x=Attrition,y=JobSatisfaction, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          ggplot(hr, aes(x=Attrition,y=WorkLifeBalance, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          align = "v",nrow = 1)
#Notes: EnvironmentSatisfaction, JobSatisfaction was low in employees who left, WorkLifeBalance was almost same for both

#Boxplots comparing Manager feedback with Attrition status
plot_grid(ggplot(hr, aes(x=Attrition,y=JobInvolvement, fill=Attrition))+ geom_boxplot(width=0.2)+ 
            coord_flip() +theme(legend.position="none"),
          ggplot(hr, aes(x=Attrition,y=PerformanceRating, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          align = "v",nrow = 1)
#Notes: Nothing interesting here?

#Boxplots comparing general attributes with Attrition status
plot_grid(ggplot(hr, aes(x=Attrition,y=Age, fill=Attrition))+ geom_boxplot(width=0.2)+ 
            coord_flip() +theme(legend.position="none"),
          ggplot(hr, aes(x=Attrition,y=NumCompaniesWorked, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          ggplot(hr, aes(x=Attrition,y=DistanceFromHome, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          ggplot(hr, aes(x=Attrition,y=Education, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          ggplot(hr, aes(x=Attrition,y=JobLevel, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          ggplot(hr, aes(x=Attrition,y=MonthlyIncome, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          ggplot(hr, aes(x=Attrition,y=PercentSalaryHike, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          align = "v",nrow = 1)
#Notes: Age, NoCompaniesWorked, JobLevel, Percentagehike seems to affect attrition

#Boxplots comparing employee stay time attributes with Attrition status
plot_grid(ggplot(hr, aes(x=Attrition,y=TotalWorkingYears, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + theme(legend.position="none"),
          ggplot(hr, aes(x=Attrition,y=YearsAtCompany, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          ggplot(hr, aes(x=Attrition,y=YearsSinceLastPromotion, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          ggplot(hr, aes(x=Attrition,y=YearsWithCurrManager, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          align = "v",nrow = 1)
#Notes: Old employees seems to stay in same company 

#Interactive plots generated using plotly 
p <- plot_ly(y = ~AverageWorkHours, type = "box", boxpoints = "all", jitter = 0.3, pointpos = -1.8)
p

p1 <- ggplot(data=hr, aes(x=AverageWorkHours, fill = "red", alpha = 0.2)) + geom_vline(xintercept=c(.5,1)) + geom_histogram(binwidth=.5, position="dodge")
p1<- ggplotly(p1)
p1

p2 <- ggplot(data=hr, aes(x=Leaves, fill = "red", alpha = 0.2)) + geom_vline(xintercept=c(.5,1)) + geom_histogram(binwidth=.5, position="dodge")
p2<- ggplotly(p2)
p2

p4 <- plot_ly(type = 'box') %>%
  add_boxplot(y = AverageWorkHours, jitter = 0.3, pointpos = -1.8, boxpoints = 'all',
              marker = list(color = 'rgb(7,40,89)'), line = list(color = 'rgb(7,40,89)'),
              name = "AverageWorkHours") %>%
  add_boxplot(y = Leaves, name = "Leaves", boxpoints = FALSE, marker = list(color = 'rgb(9,56,125)'),
              line = list(color = 'rgb(9,56,125)')) %>%
  layout(title = "Box Plot Styling Outliers")
p4

p7<- plot_ly(type = 'box') %>%
  add_boxplot(y = hr$EnvironmentSatisfaction, jitter = 0.3, pointpos = -1.8, boxpoints = 'all',
              marker = list(color = 'rgb(7,40,89)'),
              line = list(color = 'rgb(7,40,89)'),
              name = "EnvironmentSatisfaction") %>%
  add_boxplot(y = hr$JobSatisfaction, name = "JobSatisfaction", boxpoints = FALSE,
              marker = list(color = 'rgb(9,56,125)'),
              line = list(color = 'rgb(9,56,125)')) %>%
  add_boxplot(y = hr$WorkLifeBalance, name = "WorkLifeBalance", boxpoints = 'suspectedoutliers',
              marker = list(color = 'rgb(8,81,156)',
                            outliercolor = 'rgba(219, 64, 82, 0.6)',
                            line = list(outliercolor = 'rgba(219, 64, 82, 1.0)',
                                        outlierwidth = 2)),
              line = list(color = 'rgb(8,81,156)')) %>%
  layout(title = "Box Plot Styling Outliers")
p7

p8<- plot_ly(type = 'box') %>%add_boxplot(y = hr$JobInvolvement, jitter = 0.3, pointpos = -1.8, boxpoints = 'all',
                                          marker = list(color = 'rgb(255, 182, 193, .4)'),
                                          line = list(color = 'rgb(152, 0, 0, .8)'),
                                          name = "JobInvolvement") %>%
  add_boxplot(y = hr$PerformanceRating, name = "PerformanceRating", boxpoints = FALSE,
              marker = list(color = 'rgb(9,56,121)'),
              line = list(color = 'rgb(9,56,110)')) %>%
  layout(title = "Box Plot Styling Outliers")
p8

p9<- plot_ly(type = 'box') %>%
  add_boxplot(y = hr$Age, jitter = 0.3, pointpos = -1.8, boxpoints = 'all',
              marker = list(color = 'rgb(7,40,89)'),
              line = list(color = 'rgb(7,40,89)'),
              name = "Age") %>%
  add_boxplot(y = hr$NumCompaniesWorked, name = "NumCompaniesWorked", boxpoints = FALSE,
              marker = list(color = 'rgb(9,56,125)'),
              line = list(color = 'rgb(9,56,125)')) %>%
  add_boxplot(y = hr$DistanceFromHome, name = "DistanceFromHome", boxpoints = 'suspectedoutliers',
              marker = list(color = 'rgb(8,81,156)',
                            outliercolor = 'rgba(219, 64, 82, 0.6)',
                            line = list(outliercolor = 'rgba(219, 64, 82, 1.0)',
                                        outlierwidth = 2)),
              line = list(color = 'rgb(8,81,156)')) %>%
  add_boxplot(y = hr$Education, name = "Education", boxpoints = 'suspectedoutliers',
              marker = list(color = 'rgb(8,81,156)',
                            outliercolor = 'rgba(219, 64, 82, 0.6)',
                            line = list(outliercolor = 'rgba(219, 64, 72, 2.0)',
                                        outlierwidth = 2)),
              line = list(color = 'rgb(7,81,156)')) %>%
  add_boxplot(y = hr$JobLevel, name = "JobLevel", boxpoints = FALSE,
              marker = list(color = 'rgb(9,50,121)'),
              line = list(color = 'rgb(9,52,123)')) %>%
  add_boxplot(y = hr$PercentSalaryHike, name = "PercentSalaryHike", boxpoints = 'outliers',
              marker = list(color = 'rgb(219, 64, 72, 2.0)'),
              line = list(color = 'rgb(219, 64, 72, 2.0)')) %>%
  layout(title = "Box Plot Styling Outliers")
p9

##################################################################################################################
#---------------------------------------Bulding Logistic Regression Model-----------------------------------------

#Standardising Data
#Rounding AverageWorkHours
hr$AverageWorkHours<-round(hr$AverageWorkHours, digits = 2)

#Creating dummy variables for categorical variables
#dummies<- data.frame(sapply(hr, function(x) data.frame(model.matrix(~x-1,data =hr))[,-1]))
summary(hr$BusinessTravel)
dummy_1 <- data.frame(model.matrix( ~BusinessTravel, data = hr))
dummy_1<-dummy_1[,-1]
hr<-cbind(subset(hr, select=-c(BusinessTravel)),dummy_1)

summary(hr$Department)
dummy_2 <- data.frame(model.matrix( ~Department, data = hr))
dummy_2<-dummy_2[,-1]
hr<-cbind(subset(hr, select=-c(Department)),dummy_2)

summary(hr$EducationField)
dummy_3 <- data.frame(model.matrix( ~EducationField, data = hr))
dummy_3<-dummy_3[,-1]
hr<-cbind(subset(hr, select=-c(EducationField)),dummy_3)

summary(hr$JobRole)
dummy_4 <- data.frame(model.matrix( ~JobRole, data = hr))
dummy_4<-dummy_4[,-1]
hr<-cbind(subset(hr, select=-c(JobRole)),dummy_4)

summary(hr$MaritalStatus)
dummy_5 <- data.frame(model.matrix( ~MaritalStatus, data = hr))
dummy_5<-dummy_5[,-1]
hr<-cbind(subset(hr, select=-c(MaritalStatus)),dummy_5)

#Creating dummy for binary variables
summary(hr$Gender)
hr$Gender<- ifelse(hr$Gender=="Male",1,0)

summary(hr$Attrition)
hr$Attrition<- ifelse(hr$Attrition=="Yes",1,0)

#Again checking structure, covariance & correlation matrix of data with all numeric variables
str(hr)
summary(hr)
cor(hr[sapply(hr, is.numeric)]) 
cov(hr[sapply(hr, is.numeric)])

#Normalising continuous features 
hr$Age<- scale(hr$Age)
hr$DistanceFromHome<- scale(hr$DistanceFromHome)
hr$MonthlyIncome<- scale(hr$DistanceFromHome)
hr$PercentSalaryHike<- scale(hr$PercentSalaryHike)
hr$TotalWorkingYears<- scale(hr$TotalWorkingYears)
hr$TrainingTimesLastYear<- scale(hr$TrainingTimesLastYear)
hr$YearsAtCompany<- scale(hr$YearsAtCompany)
hr$YearsSinceLastPromotion<- scale(hr$YearsSinceLastPromotion)
hr$YearsWithCurrManager<- scale(hr$YearsWithCurrManager)
hr$AverageWorkHours<- scale(hr$AverageWorkHours)
hr$Leaves<- scale(hr$Leaves)

# Checking Attrition Rate of prospect customer
AttritionRate <- sum(hr$Attrition)/nrow(hr)
AttritionRate # 16.1% attrition rate. 

#Removing columns irrelevent in model building
hr<- hr[,-c(1)]  #Removing EmployeeID column as its irrelevent for model building
hr<- subset(hr,select = -c(Over18)) #Removing as this column have only 1 level "Y"
str(hr)

#Splitting the data between train and test
set.seed(100)
indices = sample.split(hr$Attrition, SplitRatio = 0.7)
train = hr[indices,]
test = hr[!(indices),]

###################################################################################################
#Initial model
model_1 = glm(Attrition ~ ., data = hr, family = "binomial")
summary(model_1) #

#Stepwise selection
library("MASS")
model_2<- stepAIC(model_1, direction="both")
summary(model_2)

#Removing multicollinearity through VIF check
library(car)
sort(vif(model_2))

hr_subset<- subset(hr,select = c(EnvironmentSatisfaction,JobSatisfaction, 
  WorkLifeBalance, Age, JobLevel, NumCompaniesWorked, TotalWorkingYears, 
  TrainingTimesLastYear, YearsAtCompany, YearsSinceLastPromotion, 
  YearsWithCurrManager, AverageWorkHours, BusinessTravelTravel_Frequently, 
  BusinessTravelTravel_Rarely, DepartmentResearch...Development, 
  DepartmentSales, EducationFieldLife.Sciences, EducationFieldMarketing, 
  EducationFieldMedical, EducationFieldOther, EducationFieldTechnical.Degree, 
  JobRoleManager, JobRoleManufacturing.Director, JobRoleResearch.Director, 
  JobRoleSales.Executive, MaritalStatusMarried, MaritalStatusSingle, Attrition))

#Excluding EducationFieldLife.Sciences
hr_subset2<-subset(hr_subset,TRUE,-c(EducationFieldLife.Sciences))
model_3<- glm(formula = Attrition ~ ., family = "binomial", data = hr_subset2) 
summary(model_3) 
sort(vif(model_3)) 

#Excluding EducationFieldMedical with p value(0.380857)
hr_subset3<-subset(hr_subset2,TRUE,-c(EducationFieldMedical))
model_4<- glm(formula = Attrition ~ ., family = "binomial", data = hr_subset3) 
summary(model_4) 
sort(vif(model_4)) 

#Excluding EducationFieldMarketing with p value(0.273993)
hr_subset4<-subset(hr_subset3,TRUE,-c(EducationFieldMarketing))
model_5<- glm(formula = Attrition ~ ., family = "binomial", data = hr_subset4) 
summary(model_5) 
sort(vif(model_5)) 

#Excluding EducationFieldOther with p value(0.122855)
hr_subset5<-subset(hr_subset4,TRUE,-c(EducationFieldOther))
model_6<- glm(formula = Attrition ~ ., family = "binomial", data = hr_subset5) 
summary(model_6) 
sort(vif(model_6)) 

#Excluding EducationFieldTechnical.Degree with p value(0.134386)
hr_subset6<-subset(hr_subset5,TRUE,-c(EducationFieldTechnical.Degree))
model_7<- glm(formula = Attrition ~ ., family = "binomial", data = hr_subset6) 
summary(model_7) 
sort(vif(model_7)) 

#Excluding YearsAtCompany with high VIF() 
hr_subset7<-subset(hr_subset6,TRUE,-c(YearsAtCompany))
model_8<- glm(formula = Attrition ~ ., family = "binomial", data = hr_subset7) 
summary(model_8) 
sort(vif(model_8)) 

#Excluding JobRoleSales.Executive with p value(0.111574)
hr_subset8<-subset(hr_subset7,TRUE,-c(JobRoleSales.Executive))
model_9<- glm(formula = Attrition ~ ., family = "binomial", data = hr_subset8) 
summary(model_9) 
sort(vif(model_9)) 

#Excluding JobLevel with p value(0.090519)
hr_subset9<-subset(hr_subset8,TRUE,-c(JobLevel))
model_10<- glm(formula = Attrition ~ ., family = "binomial", data = hr_subset9) 
summary(model_10) 
sort(vif(model_10)) 

#Excluding MaritalStatusMarried with p value(0.05886)
hr_subset10<-subset(hr_subset9,TRUE,-c(MaritalStatusMarried))
model_11<- glm(formula = Attrition ~ ., family = "binomial", data = hr_subset10) 
summary(model_11) 
sort(vif(model_11)) 

#Excluding JobRoleResearch.Director with p value(0.034746)
hr_subset11<-subset(hr_subset10,TRUE,-c(JobRoleResearch.Director))
model_12<- glm(formula = Attrition ~ ., family = "binomial", data = hr_subset11) 
summary(model_12) 
sort(vif(model_12)) 

#Excluding JobRoleManager with p value(0.034746)
hr_subset12<-subset(hr_subset11,TRUE,-c(JobRoleManager))
model_13<- glm(formula = Attrition ~ ., family = "binomial", data = hr_subset12) 
summary(model_13) 
sort(vif(model_13)) 

final_model<- model_13

################################################################################################################
#----------------------------------------------Model Evaluation-------------------------------------------------
#predicted probabilities of Churn 1 for test data
test_pred = predict(final_model, type = "response", newdata = test)
summary(test_pred)
test$prob <- test_pred
View(test)

#Let's use the probability cutoff of 50%.
test_pred_attrition <- factor(ifelse(test_pred >= 0.5, "Yes", "No"))
test_actual_attrition <- factor(ifelse(test$Attrition==1,"Yes","No"))
table(test_actual_attrition,test_pred_attrition)

library(caret)
test_conf <- confusionMatrix(test_pred_attrition, test_actual_attrition, positive = "Yes")
test_conf

###########################################################################################################
##Let's Choose the cutoff value, Let's find out the optimal probalility cutoff 

perform_fn <- function(cutoff) 
{
  predicted_attrition <- factor(ifelse(test_pred >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_attrition, test_actual_attrition, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

# Summary of test probability
summary(test_pred)
s = seq(.01,.80,length=100)
OUT = matrix(0,100,3)

for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 

plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))
cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.01)]

# Let's choose a cutoff value of 0.3132 for final model
test_cutoff_attrition <- factor(ifelse(test_pred >=0.15, "Yes", "No"))
conf_final <- confusionMatrix(test_cutoff_attrition, test_actual_attrition, positive = "Yes")
acc <- conf_final$overall[1]
sens <- conf_final$byClass[1]
spec <- conf_final$byClass[2]
conf_final
acc   #0.7183642
sens  #0.8009479
spec  #0.7023041

##################################################################################################
##KS -statistic - Test Data 

test_cutoff_attrition <- ifelse(test_cutoff_attrition=="Yes",1,0)
test_actual_attrition <- ifelse(test_actual_attrition=="Yes",1,0)

library(ROCR)
#on testing  data
pred_object_test<- prediction(test_cutoff_attrition, test_actual_attrition)
performance_measures_test<- performance(pred_object_test, "tpr", "fpr")

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - (attr(performance_measures_test, "x.values")[[1]])
ks_table_test
max(ks_table_test)

###################################################################################################
##Lift & Gain Chart 
#plotting the lift chart
lift <- function(labels , predicted_prob,groups=10) {
  
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp=sum(., na.rm = TRUE))) %>%
    
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups))) 
  return(gaintable)
}

Attrition_decile = lift(test_actual_attrition, test_pred, groups = 10)
Attrition_decile

###################################################################################################
##ROC Curve
library(ROCR)
#calculating the values for ROC curve
pred <- prediction(test$prob, test$Attrition)
perf <- performance(pred,"tpr","fpr")

# changing params for the ROC plot - width, etc
par(mar=c(5,5,2,2),xaxs = "i",yaxs = "i",cex.axis=1.3,cex.lab=1.4)

# plotting the ROC curve
plot(perf,col="black",lty=3, lwd=3)

# calculating AUC
auc <- performance(pred,"auc")
auc

#---------------------------------------------Conclusion---------------------------------------------------------
##1. Based on logistic regression 16 out of 43 variables were found highly significant for attrition, these were: 
#TrainingTimesLastYear, JobRoleManufacturing.Director, WorkLifeBalance, JobSatisfaction,EnvironmentSatisfaction, 
#MaritalStatusSingle, AverageWorkHours, NumCompaniesWorked,YearsWithCurrManager, Age, YearsSinceLastPromotion, 
#TotalWorkingYears, BusinessTravelTravel_Rarely,  BusinessTravelTravel_Frequently, DepartmentSales, DepartmentResearch...Development. 

##2. Optimal Logistic Regression model can predict Employees who are likely to leave organization based on above 
#16 attributes correctly 80 percept of times.

##3. As we suspected during EDA factors like WorkLifeBalance, JobSatisfaction,EnvironmentSatisfaction, AverageWorkHours, 
#NumCompaniesWorked, Age does affect attrition as found by the model along with other factors like BuisnessTravelFrequency,DepartmentR&D,
#JobRoleManufacturing etc. These hidden aspects were only revealed after through analysis & prediction usinglogistic regression model.

##4. XYZ can now focus on these 16 significant factors found by model to control attrition. 

