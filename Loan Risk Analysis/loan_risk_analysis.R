#-------------------------------Gramener Case Study-------------------------------------------

##Setting path & importing neccessary libraries
setwd("/media/newhd/Data Science/PGDDA IIITB Docs/Course2_StatisticsAndEDA/GramenerCaseStudy")
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)

##Fetching & Analysing dataset
loan=read.csv("loan.csv")
View(loan)     #View dataset
str(loan)      #To check datatype of all variables
summary(loan)  #To check NA values & statistical measures

##-----------------------------Data Cleaning-----------------------------------------------
#1.Deleting/Filter unnecessary columns with all NA values
allNA<-c(which(sapply(loan, function(x)all(is.na(x)))))
allNA<-c(sapply(loan, function(x)all(is.na(x))))
allNA<-as.data.frame(allNA)
toRemove<-allNA[,1]
loan[c(toRemove)] <- list(NULL) 

#2.Imputing NA values
#chargeoff_within_12_mths,pub_rec_bankruptcies,tax_liens,title,collections_12_mths_ex_med can be imputed
#is.na(loan$chargeoff_within_12_mths)<-0 #Ignored as imputation not relevent for below EDA

#3.Removing irrelevant columns
loan$pymnt_plan<-NULL  #Removed as all rows have same value 'n'
loan$url<-NULL #Not relevent for EDA
loan$initial_list_status<-NULL #Removed as all rows have same value 'f'
loan$application_type<-NULL #Removed as all rows have same value 'INDIVIDUAL'
loan$desc<-NULL #As text description not relevent for EDA without processing text 
loan$policy_code<-NULL #Removed as all rows have same value '1'

#4.Adding relevant column names
colnames(loan)[1] <- "loan_id"

#5.Standardising Values
#a)Rounding total_pymnt,total_rec_late_fee,funded_amnt_inv to 2 decimal places
round(loan$total_pymnt, digits = 2)
round(loan$total_rec_late_fee, digits = 2)
round(loan$funded_amnt_inv, digits = 2)
round(loan$collection_recovery_fee, digits = 2)

#b)Converting emp_length to numeric as per data dictionary
pattern="\\d{1,2}"
convert_emp_length <- function(x){
  if(!is.na(str_extract(x,"< 1"))){
    return(0)
  }
  exp=as.numeric(str_extract(x,pattern))
  return(exp)
}
loan$emp_length<-c(sapply(loan$emp_length, convert_emp_length))

#c)Removing Outliers
#Outliers are not removed as this data is relevent, only removed in few plots for clarity 

#d)Converting issue_d, earliest_cr_line, last_pymnt_d, last_credit_pull_d to Date
library(lubridate) 
loan$earliest_cr_line<-parse_date_time(paste("01-",loan$earliest_cr_line),orders = c("d-b-y"))
loan$last_pymnt_d<-parse_date_time(paste("01-",loan$last_pymnt_d),orders = c("d-b-y"))
loan$issue_d<-parse_date_time(paste("01-",loan$issue_d),orders = c("d-b-y"))
loan$last_credit_pull_d<-parse_date_time(paste("01-",loan$last_credit_pull_d),orders = c("d-b-y"))

#e)Converting percentages to fractions for correlation analysis 
loan$int_rate=as.numeric(sub("%", "", loan$int_rate))
loan$revol_util=as.numeric(sub("%", "", loan$revol_util))

#f)Converting grades to numeric
levels(loan$sub_grade)
values<-seq(1,7,by=0.2)
#values[loan$sub_grade]
loan$rank<-values[loan$sub_grade]

###---------------------------------Univariate Analysis---------------------------------------------

#1.Distribution of variables:
#a)Distribution of loan amount 
ggplot(loan, aes(x = loan_amnt)) + geom_histogram(binwidth = 1000) + labs(title = "Loan Amount Distribution")
ggplot(loan, aes(x=loan_amnt)) + geom_bar(aes(fill = purpose))

#b)Distribution by purpose of loan of few types: 
#(i)Car Loans disribution
car_loan<-filter(loan, purpose == "car")
ggplot(car_loan, aes(x = loan_amnt)) + geom_histogram() + labs(title = "Car Loan Amount Distribution")
#(ii)House loan distribution
house_loan<-filter(loan, purpose == "house")
ggplot(house_loan, aes(x = loan_amnt)) + geom_histogram() + labs(title = "House Loan Amount Distribution")
#(iii)Credit Card loan distribution
credit_card_loan<-filter(loan, purpose == "credit_card")
ggplot(credit_card_loan, aes(x = loan_amnt)) + geom_histogram() + labs(title = "Credit Card Loan Amount Distribution")
#(iv)Small Business loan distribution
small_business_loan<-filter(loan, purpose == "small_business")
ggplot(small_business_loan, aes(x = loan_amnt)) + geom_histogram() + labs(title = "Small Business Loan Amount Distribution")
#(v)Educational loan distribution
educational_loan<-filter(loan, purpose == "educational")
ggplot(educational_loan, aes(x = loan_amnt)) + geom_histogram() + labs(title = "Educational Loan Amount Distribution")
#(vi)Debt Consolidation loan distribution
debt_consolidation_loan<-filter(loan, purpose == "debt_consolidation")
ggplot(debt_consolidation_loan, aes(x = loan_amnt)) + geom_histogram() + labs(title = "Debt Consolidation Amount Distribution")

#c)Distribution of interest rates
ggplot(loan, aes(x = int_rate)) + geom_histogram() 

#d)Distribution of annual income
ggplot(loan, aes(x = annual_inc)) + geom_histogram() #outliers present
mean(loan$annual_inc)
median(loan$annual_inc)

#2.Boxplots
#z <- loan[loan$loan_amnt > quantile(loan$loan_amnt, .25) - 1.5*IQR(loan$loan_amnt) & loan$loan_amnt < quantile(loan$loan_amnt, .75) + 1.5*IQR(loan$loan_amnt)]
#a)
boxplot(loan$annual_inc) #Clearly shows outliers
boxplot(loan$annual_inc,xlab="Annual Income",horizontal = TRUE,outline=FALSE) #With outliers removed

#b)
boxplot(loan$loan_amnt)
boxplot(loan$loan_amnt,xlab="Loan Amount",horizontal = TRUE,outline=FALSE) #With outliers removed

#c)
boxplot(loan$int_rate)
boxplot(loan$int_rate,ylab="Interest Rate",outline=FALSE) #With outliers removed

#d)
boxplot(loan$installment) 
boxplot(loan$installment,ylab="Installment",outline=FALSE) #With outliers removed

#e)
boxplot(loan$emp_length,ylab="Employment Length") #Have median around 4 years of experience  

#f)
boxplot(loan$dti,ylab="Debt to Income Ratio")

#g)
boxplot(loan$rank,xlab="Grade Distribution(calculated as rank)")

#3.Descriptive Data Summary
str(loan) #Will print min, max, mean, median, 1st Quartile, 3rd Quartile for numerical variables 

###----------------------------------Bivariate Analysis----------------------------------------------

#1.Many outliers found in 'Charged Off' & 'Fully Paid' category
boxplot(loan_amnt~loan_status,data=loan, main="Loan Data", 
        xlab="Loan Status", ylab="Loan Amount")
#With outlier removed 
boxplot(loan_amnt~loan_status,data=loan, main="Loan Data", 
        xlab="Loan Status", ylab="Loan Amount",outline=FALSE)

#2.Plots to explore relationship between categorical & quantitative variables
#a)Average loan amount by purpose
loan_amt_by_purpose <- group_by(loan, purpose) %>% summarise(mean(loan_amnt, na.rm = T)) %>% setNames(.,c('Purpose','Avg'))
ggplot(loan_amt_by_purpose, aes(x=Purpose, y=Avg )) + geom_boxplot()

#b)Loan dates difference vs loan status? #Correction required
#date_diff=loan$last_pymnt_d-loan$issue_d
date_diff=loan$last_credit_pull_d-loan$last_pymnt_d
date_diff_avg <- group_by(loan, loan_status) %>% summarise(mean(date_diff, na.rm = T)) %>% setNames(.,c('Diff','Avg'))
ggplot(date_diff_avg, aes(x=Diff, y=Avg )) + geom_boxplot()
typeof(date_diff_avg)

#c)*funded_amnt_inv/loan_amount ratio vs loan status
loan$ratio_fa_la=loan$funded_amnt_inv/loan$loan_amnt
ratio_avg <- group_by(loan, loan_status) %>% summarise(mean(ratio_fa_la, na.rm = T)) %>% setNames(.,c('ratio','Avg'))
#plot shows that people who charged off had small ratio i.e they were often not funded  
ggplot(ratio_avg, aes(x=ratio, y=Avg )) + geom_boxplot() + ylab("Average of funded_amnt/loan_amount ratio")
 
#d)*last_payment_amt vs loan_status
last_payment_amnt <- group_by(loan, loan_status) %>% summarise(mean(last_pymnt_amnt, na.rm = T)) %>% setNames(.,c('Last_Payment','Avg'))
ggplot(last_payment_amnt, aes(x=Last_Payment, y=Avg )) + geom_boxplot() #Very high for fully paid

#e)intallment/loan_amnt vs loan status
installment_loan_ratio=loan$installment/loan$loan_amnt
ilratio_avg <- group_by(loan, loan_status) %>% summarise(mean(installment_loan_ratio, na.rm = T)) %>% setNames(.,c('ILRatio','Avg'))
ggplot(ilratio_avg, aes(x=ILRatio, y=Avg )) + geom_boxplot() #Same for all categories

#3.Checking correlation among quatitatiive variables
cor(loan$loan_amnt,loan$funded_amnt) #Highly positve
cor(loan$loan_amnt,loan$int_rate) #positive
cor(loan$loan_amnt,loan$annual_inc)
cor(loan$annual_inc,loan$int_rate) #Slightlty Positive
cor(loan$emp_length,loan$loan_amnt)

#4.Correlation Heat Map
library(reshape2) 
loan_nums <- sapply(loan, is.numeric)
loan_nums<-loan[ , loan_nums]
loan_data <- c(loan_nums) 
qplot(x=Var1, y=Var2, data=melt(cor(loan_nums)), fill=value, geom="tile")+theme(axis.text.x = element_text(angle = 90, hjust = 1))

#5.Barplots to explore relationship among categorical variables
#a)*loan status vs grade:
ggplot(loan, aes(x=loan_status)) + geom_bar(aes(fill = grade), position = "dodge")

ggplot(loan, aes(x=loan_status)) + geom_bar(aes(fill = grade), position = position_stack(reverse = TRUE)) +
  coord_flip()+theme(legend.position = "top")

#b)loan_status vs emp_length:
ggplot(loan, aes(x=loan_status)) + geom_bar(aes(fill = emp_length))

#c)loan_status vs purpose:
ggplot(loan, aes(x=loan_status)) + geom_bar(aes(fill = purpose), position = "dodge") +
  coord_flip()+theme(legend.position = "top")

#d)*loan_status vs verification_status:
ggplot(loan, aes(x=loan_status)) + geom_bar(aes(fill = verification_status), position = "dodge")

#e)*count of different loan_status across us states
ggplot(loan, aes(x=loan_status)) + geom_bar(aes(fill = addr_state), position = "dodge")
ggplot(subset(loan,loan_status=="Fully Paid"), aes(x=loan_status)) + geom_bar(aes(fill = addr_state), position = "dodge")
ggplot(subset(loan,loan_status=="Charged Off"), aes(x=loan_status)) + geom_bar(aes(fill = addr_state), position = "dodge")

#f)*term & loan status
ggplot(loan, aes(x=loan_status)) + geom_bar(aes(fill = term), position = "dodge") +theme(axis.text.x = element_text(angle = 90, hjust = 1))#Large proportion of fully paid loans had 36 months term

#g)home_ownership & loan_status
home_status_subset<-subset(loan,home_ownership!="NONE")
ggplot(home_status_subset, aes(x=loan_status)) + geom_bar(aes(fill = home_ownership), position = "dodge") +theme(axis.text.x = element_text(angle = 90, hjust = 1))#Shows similar trend for both Chaarged_Off & Fully_Paid 

#6.Analysing fields from RejectStatus
#*Debt-To-Income Ratio
dti_avg <- group_by(loan, loan_status) %>% summarise(mean(dti, na.rm = T)) %>% setNames(.,c('Dti_Avg','Avg'))
ggplot(dti_avg, aes(x=Dti_Avg, y=Avg )) + geom_boxplot() #Charged_Off cndidates have much higher dti compared to Fully Paid

#*Amount Requested vs Loan Status: People who charged off applied for higher loan on average compared to Fully Paid
loan_amt_by_status <- group_by(loan, loan_status) %>% summarise(mean(loan_amnt, na.rm = T)) %>% setNames(.,c('Status','Avg'))
ggplot(loan_amt_by_status, aes(x=Status, y=Avg )) + geom_boxplot()

#Effect of Employment Length on Loan status already checked in Barplots above
#Policy code removed during data cleaning as it have single value


