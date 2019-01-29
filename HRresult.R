#Loading Required libraries
library(tidyr)
library(dplyr)
library(caTools)
library(ggplot2)
library(scales)
library(MASS)
library(car)
library(caret)
library(e1071)
library(ROCR)
#Load required files
emp_survey <- read.csv("employee_survey_data.csv", stringsAsFactors=FALSE)
gen_data <- read.csv("general_data.csv", stringsAsFactors=FALSE)
manager_survey<- read.csv("manager_survey_data.csv", stringsAsFactors=FALSE)
in_time <- read.csv("in_time.csv", stringsAsFactors=FALSE)
out_time<-read.csv("out_time.csv",stringsAsFactors = FALSE)

#Viewing the structure of dataframe
str(emp_survey)
str(gen_data)
str(manager_survey)
str(in_time)
str(out_time)

#Changing the first column name of in_time and out_time to EmployeeID

colnames(in_time)[1]<-"EmployeeID"
colnames(out_time)[1]<-"EmployeeID"

#Checking of duplicated entries in data sets

which(duplicated(emp_survey$EmployeeID))
which(duplicated(gen_data$EmployeeID))
which(duplicated(manager_survey$EmployeeID))
which(duplicated(in_time$EmployeeID))
which(duplicated(out_time$EmployeeID))

#All returned 0
#***************************************************************************#
#*********************** EDA and Data Preparation***************************#

#Checking for NAs in general data
sapply(gen_data, function(x) length(which(is.na(x))))

#NAs in NumCompanies worked:19
#NAs in TotalWorkingYears:9

#Dealing with NAs in NumCompanies
#If the difference between total working years and years at company
# is equal to zero,then the employee has not worked in any other company,hence
# NumCompanies worked =0,if not zero then NumCompanies worked =1,as no other info
#is provided abt other companies the employ worked

for(i in 1:nrow(gen_data))
{
  if(is.na(gen_data$NumCompaniesWorked[i])==TRUE){
    if(gen_data$TotalWorkingYears[i]-gen_data$YearsAtCompany[i]==0){
      gen_data$NumCompaniesWorked[i]<-0
    }else{
      gen_data$NumCompaniesWorked[i] <-1
    }
  }
}
#Checking again for NAs in numcompanies worked
which(is.na(gen_data$NumCompaniesWorked))
#return 0

#Dealing with NAs in total working years
#Replace it with years at Company

for(i in 1:nrow(gen_data))
{
  if(is.na(gen_data$TotalWorkingYears[i])==TRUE){
    gen_data$TotalWorkingYears[i]<-gen_data$YearsAtCompany[i]
     
  }
}

#Checking again for NAs in Total working Years
which(is.na(gen_data$TotalWorkingYears))
#return 0

#Checking for NAs in emp_survey
sapply(emp_survey, function(x) length(which(is.na(x))))
#No: of NAs in Enviornment satisifaction:25
#No: of NAs in Job satisifaction:20
#No: of NAs in Work life Balance:38

#Dealing with NAs in Employee Satisfaction
#Calculating Mode 
table(emp_survey$EnvironmentSatisfaction)
#Mode:Maximum no: of occurrences= 3
#Hence replacing NAs with 3
emp_survey$EnvironmentSatisfaction[which(is.na(emp_survey$EnvironmentSatisfaction))]<-3
#Checking again for NAs
which(is.na(emp_survey$EnvironmentSatisfaction))
#Return 0

#Dealing with NAs in JobSatisfaction
#Calculating Mode 
table(emp_survey$JobSatisfaction)
#Mode:Maximum no: of occurrences= 4
#Hence replacing NAs with 4
emp_survey$JobSatisfaction[which(is.na(emp_survey$JobSatisfaction))]<-4
#Checking again for NAs
which(is.na(emp_survey$JobSatisfaction))
#Return 0

#Dealing with NAs in Work Life Balance
#Calculating Mode 
table(emp_survey$WorkLifeBalance)
#Mode:Maximum no: of occurrences= 3
#Hence replacing NAs with 3
emp_survey$WorkLifeBalance[which(is.na(emp_survey$WorkLifeBalance))]<-3
#Checking again for NAs
which(is.na(emp_survey$WorkLifeBalance))
#Return 0

#Checking for NAs in manager_survey
sapply(manager_survey, function(x) length(which(is.na(x))))
#Return 0


#Data Preparation of in_time and out_time
#Finding the average working hours of each employee for the year

in_time<-gather(in_time, in_day, in_day_time, X2015.01.01 :X2015.12.31)

out_time<-gather(out_time, out_day, out_day_time, X2015.01.01 :X2015.12.31)

#NAs in in_time and out_time usually denotes holidays and leaves
#Checking whether NAs in in_time and out_time are on the same date
in_na<-which(is.na(in_time))
out_na<-which(is.na(out_time))
length(in_na)
length(out_na)
#Both are equal to 109080
#if the index is same then both NAs are on same date
for(i in 1:length(in_na))
{if(in_na[i]!=out_na[i]){print("NAs not on same date")}}

#Hence NAs on in_time and out_time are on same date
#Removing NAs
in_time <- in_time[(!is.na(in_time$in_day_time)), ]
out_time <- out_time[(!is.na(out_time$out_day_time)), ]

#Combining in and out time in one data set
in_out_time<-data.frame(cbind(in_time$EmployeeID,in_time$in_day_time,out_time$out_day_time))

#Converting time into required format
in_out_time$X2<-strptime(in_out_time$X2,"%Y-%m-%d %H:%M:%S")
in_out_time$X3<-strptime(in_out_time$X3,"%Y-%m-%d %H:%M:%S")

#Finding the difference in in_time and out_time for each date of each employee
in_out_time$diff<-difftime(in_out_time$X3,in_out_time$X2,units = "hours")

#Finding the average working hours for each employee
emp_avg_hrs<-aggregate(diff~X1,in_out_time,mean)

#Changing the column names of emp_avg_hrs
colnames(emp_avg_hrs)[1]<-"EmployeeID"
colnames(emp_avg_hrs)[2]<-"Average_working_Hours"

#Rounding the average working hours
emp_avg_hrs$Average_working_Hours<-round(emp_avg_hrs$Average_working_Hours,2)

#*************************Data Preparation Ends******************************#

#Key attribute for Merging the files is EmployeeID.Checking for any differences in Key attribute

setdiff(gen_data$EmployeeID,emp_survey$EmployeeID)
setdiff(gen_data$EmployeeID,manager_survey$EmployeeID)
setdiff(gen_data$EmployeeID,emp_avg_hrs$EmployeeID)

#All Return 0,Hence ok to merge
#Merging the files
master_file<-merge(gen_data,emp_survey,by="EmployeeID")
master_file<-merge(master_file,manager_survey,by="EmployeeID")
master_file<-merge(master_file,emp_avg_hrs,by="EmployeeID")

#**************Plotting of Variables**************************#

#Attrition V/s Categorical Variables

# Barcharts for categorical features 
#1.BusinessTravel
 ggplot(master_file, aes(x=BusinessTravel,  group=Attrition)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(x="Business Travel",y = "Percentage", title="Attrition Rate due to Business Travel") +
  facet_grid(~Attrition) +
  scale_y_continuous(labels = scales::percent)

#Attrition Rate is Higher for those who travel rare

#2.Department
 ggplot(master_file, aes(x=Department,  group=Attrition)) + 
   geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
   geom_text(aes( label = scales::percent(..prop..),
                  y= ..prop.. ), stat= "count", vjust = -.5) +
   labs(x="Department",y = "Percentage", title="Attrition Rate in Various Department ") +
   facet_grid(~Attrition) +
   scale_y_continuous(labels = scales::percent)

#Attrition Higher in Research and Development 
 
#3.Education Field

 ggplot(master_file, aes(x=EducationField,  group=Attrition)) + 
   geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
   geom_text(aes( label = scales::percent(..prop..),
                  y= ..prop.. ), stat= "count", vjust = -.5) +
   labs(x="Education Field",y = "Percentage", title="Attrition Rate due to Education Field ") +
   facet_grid(~Attrition) +
   scale_y_continuous(labels = scales::percent)
 
#Attrition Higher in Life Sciences 
#4.Gender
 ggplot(master_file, aes(x=Gender,  group=Attrition)) + 
   geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
   geom_text(aes( label = scales::percent(..prop..),
                  y= ..prop.. ), stat= "count", vjust = -.5) +
   labs(x="Gender",y = "Percentage", title="Attrition Rate in Gender") +
   facet_grid(~Attrition) +
   scale_y_continuous(labels = scales::percent)

#Attrition more among male employees
#5.Job Role 
 ggplot(master_file, aes(x=JobRole,  group=Attrition)) + 
   geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
   geom_text(aes( label = scales::percent(..prop..),
                  y= ..prop.. ), stat= "count", vjust = -.5) +
   labs(x="Job Role",y = "Percentage", title="Attrition Rate due to JobRole") +
   facet_grid(~Attrition) +
   scale_y_continuous(labels = scales::percent)

#6.MartialStatus 
 ggplot(master_file, aes(x=MaritalStatus,  group=Attrition)) + 
   geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
   geom_text(aes( label = scales::percent(..prop..),
                  y= ..prop.. ), stat= "count", vjust = -.5) +
   labs(x="Marital Status",y = "Percentage", title="Attrition Rate due to Martial Status") +
   facet_grid(~Attrition) +
   scale_y_continuous(labels = scales::percent)

#Higher Attrition Among Single
#7.StockOptionLevel 
 ggplot(master_file, aes(x=StockOptionLevel,  group=Attrition)) + 
   geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
   geom_text(aes( label = scales::percent(..prop..),
                  y= ..prop.. ), stat= "count", vjust = -.5) +
   labs(x="Stock Option Level",y = "Percentage", title="Attrition Rate due to Stock Option Level") +
   facet_grid(~Attrition) +
   scale_y_continuous(labels = scales::percent)
 
#Higher Attrition among low stock level
#8.TimesTrainingLastYear
 ggplot(master_file, aes(x=TrainingTimesLastYear,  group=Attrition)) + 
   geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
   geom_text(aes( label = scales::percent(..prop..),
                  y= ..prop.. ), stat= "count", vjust = -.5) +
   labs(x="Training Times Last Year",y = "Percentage", title="Attrition Rate due to Training Times Last Year") +
   facet_grid(~Attrition) +
   scale_y_continuous(labels = scales::percent)
# HigherAttrition for those with 2 to 3 Training last year
#9.Environment Satisfaction
 
 ggplot(master_file, aes(x=EnvironmentSatisfaction,  group=Attrition)) + 
   geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
   geom_text(aes( label = scales::percent(..prop..),
                  y= ..prop.. ), stat= "count", vjust = -.5) +
   labs(x="Environment Satisfaction",y = "Percentage", title="Attrition Rate due to Environment Satisfaction") +
   facet_grid(~Attrition) +
   scale_y_continuous(labels = scales::percent)
 #Attrition higher with level 1 of Environment Satisfaction

#10.Job Satisfaction
 ggplot(master_file, aes(x=JobSatisfaction,  group=Attrition)) + 
   geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
   geom_text(aes( label = scales::percent(..prop..),
                  y= ..prop.. ), stat= "count", vjust = -.5) +
   labs(x="Job Satisfaction",y = "Percentage", title="Attrition Rate due to Job Satisfaction") +
   facet_grid(~Attrition) +
   scale_y_continuous(labels = scales::percent)
#Attrition Higher for level 3 and 1 

#11.Work life Balance 
 ggplot(master_file, aes(x=WorkLifeBalance,  group=Attrition)) + 
   geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
   geom_text(aes( label = scales::percent(..prop..),
                  y= ..prop.. ), stat= "count", vjust = -.5) +
   labs(x="Work Life Balance",y = "Percentage", title="Attrition Rate due to Work Life Balance") +
   facet_grid(~Attrition) +
   scale_y_continuous(labels = scales::percent)
 #Attrition Higher in level 3(Work Life Balance Better)

#12.Job Involvement 
 ggplot(master_file, aes(x=JobInvolvement,  group=Attrition)) + 
   geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
   geom_text(aes( label = scales::percent(..prop..),
                  y= ..prop.. ), stat= "count", vjust = -.5) +
   labs(x="Job Involvement",y = "Percentage", title="Attrition Rate due to Job Involvement") +
   facet_grid(~Attrition) +
   scale_y_continuous(labels = scales::percent)
 #Attrition Higher in level 3(Involvement High)
 
 #13.Performance Rating
 ggplot(master_file, aes(x=PerformanceRating,  group=Attrition)) + 
   geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
   geom_text(aes( label = scales::percent(..prop..),
                  y= ..prop.. ), stat= "count", vjust = -.5) +
   labs(x="Performance Rating",y = "Percentage", title="Attrition Rate due to Performance Rating") +
   facet_grid(~Attrition) +
   scale_y_continuous(labels = scales::percent)
 #Rating 3 has high attrition rate

 #14.Education
 ggplot(master_file, aes(x=Education,  group=Attrition)) + 
   geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
   geom_text(aes( label = scales::percent(..prop..),
                  y= ..prop.. ), stat= "count", vjust = -.5) +
   labs(x="Education",y = "Percentage", title="Attrition Rate due to level of Education") +
   facet_grid(~Attrition) +
   scale_y_continuous(labels = scales::percent)
 #Attrition higher for those with level 3
 
 #15.Joblevel
 ggplot(master_file, aes(x=JobLevel,  group=Attrition)) + 
   geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
   geom_text(aes( label = scales::percent(..prop..),
                  y= ..prop.. ), stat= "count", vjust = -.5) +
   labs(x="Job Level",y = "Percentage", title="Attrition Rate due to level of Job") +
   facet_grid(~Attrition) +
   scale_y_continuous(labels = scales::percent)
 #Attrition higher for those with level 1 and 2
 

#Attrition V/S Continuous variables

#1.Monthly income
ggplot(master_file,aes(x=MonthlyIncome,fill=Attrition))+geom_density()

#Attrition higher among low monthly income

#2.Age

ggplot(master_file,aes(x=Age,fill=Attrition))+geom_density()

#Attrition higher among low age

#3.Distance from home 

ggplot(master_file,aes(x=DistanceFromHome,fill=Attrition))+geom_density()

#No visible pattern

#4.Percentage Salary Hike

ggplot(master_file,aes(x=PercentSalaryHike,fill=Attrition))+geom_density()

#No visible pattern

#5.Total Working Years

ggplot(master_file,aes(x=TotalWorkingYears,fill=Attrition))+geom_density()

#More Attrition Among those with low no. of working years

#6.YearsAtCompany

ggplot(master_file,aes(x=YearsAtCompany,fill=Attrition))+geom_density()

#More attrition among those with low no of years at Company

#7.YearsSince Last Promotion

ggplot(master_file,aes(x=YearsSinceLastPromotion,fill=Attrition))+geom_density()

#More attrition among those who promoted recently

#8.NumCompanies Worked
ggplot(master_file,aes(x=NumCompaniesWorked,fill=Attrition))+geom_density()

# No visible pattern

#9.Yearswith current manager
ggplot(master_file,aes(x=YearsWithCurrManager,fill=Attrition))+geom_density()

#More attrition among those with less no. of years under current manager

#10.Years since last promotion

ggplot(master_file,aes(x=YearsSinceLastPromotion,fill=Attrition))+geom_density()

#More attrition among those who are recently promoted

#11.Average Working Hours
#Changing the format of average working hours to numeric

master_file$Average_working_Hours<-as.numeric(master_file$Average_working_Hours)

ggplot(master_file,aes(x=Average_working_Hours,fill=Attrition))+geom_density()

#Attrition higher for those who work more than 8 hours

#Checking for outliers in continuous variables

boxplot(master_file$Age)
boxplot(master_file$PercentSalaryHike)
boxplot(master_file$DistanceFromHome)

#No outliers in the above variable

boxplot(master_file$TotalWorkingYears)
boxplot(master_file$YearsAtCompany)
boxplot(master_file$YearsSinceLastPromotion)
boxplot(master_file$YearsWithCurrManager)
boxplot(master_file$Average_working_Hours)
boxplot(master_file$NumCompaniesWorked)
boxplot(master_file$YearsAtCompany)
boxplot(master_file$MonthlyIncome)

#Few outliers in the above variable which is quite possible and will be treated 
#after the scaling operation on them


#************************Modelling*************************************#

#Structure of Master File
str(master_file)
#EmployeeId doesn't affect the attrition,hence it can be removed
#EmployeeCount=1 for all values,hence can be removed
#Over18 is not required as all employees are over 18 years of age
#Standard working =8 for all,hence can be removed
master_final<-subset(master_file,select = -c(EmployeeID,EmployeeCount,Over18,StandardHours))

# converting target variable Attrition from No/Yes character to factorwith levels 0/1 
master_final$Attrition<- ifelse(master_final$Attrition=="Yes",1,0)

# creating dataframe for categorical features and numerical features
master_final_cat<-subset(master_final,select=c(3:4,6:11,15,17,21:25))
master_final_num<-subset(master_final,select=-c(3:4,6:11,15,17,21:25))

#Changing all the character variable to factor
master_cat_fact<- data.frame(sapply(master_final_cat, function(x) factor(x)))
str(master_cat_fact)

# creating dummy variables for factor attributes
dummies<- data.frame(sapply(master_cat_fact, 
                            function(x) data.frame(model.matrix(~x-1,data =master_cat_fact))[,-1]))

#Scaling of Numerical variables
str(master_final_num)
#Changing Average working hours to numeric
master_num_scale<-subset(master_final_num,select=c(1,3:11))
scaled<- data.frame(sapply(master_num_scale, 
                            function(x) scale(x)))

#Final Data for Modelling
master_data <- cbind(master_final_num$Attrition, scaled, dummies) 
colnames(master_data)[1]<-"Attrition"

View(master_data)#4410 obs of 61 variables

# splitting the data between train and test
set.seed(100)

indices = sample.split(master_data$Attrition, SplitRatio = 0.7)

train = master_data[indices,]

test = master_data[!(indices),]

# Logistic Regression: 

#Initial model
model_1 = glm(Attrition~ ., data = train, family = "binomial")
summary(model_1) 

#Using Step AIC function
model_2<- stepAIC(model_1, direction="both")

summary(model_2)
# Removing multicollinearity through VIF check
vif(model_2)

#Removing Years at Company as it has High VIF
model_3 <-glm(formula = Attrition ~ Age + DistanceFromHome + MonthlyIncome + 
                NumCompaniesWorked + TotalWorkingYears + 
                YearsSinceLastPromotion + YearsWithCurrManager + Average_working_Hours + 
                BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                Department.xResearch...Development + Department.xSales + 
                Education.x3 + Education.x4 + Education.x5 + EducationField.xMarketing + 
                EducationField.xOther + Gender + JobLevel.x2 + JobRole.xLaboratory.Technician + 
                JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
                JobRole.xSales.Executive + MaritalStatus.xMarried + MaritalStatus.xSingle + 
                StockOptionLevel.x1 + TrainingTimesLastYear.x1 +TrainingTimesLastYear.x3 +
                TrainingTimesLastYear.x4 + TrainingTimesLastYear.x5 + TrainingTimesLastYear.x6 + 
                EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                WorkLifeBalance.x4 + JobInvolvement.x3, family = "binomial", 
              data = train)
summary(model_3)
vif(model_3)

#Removing BusinessTravel.xTravel_Rarely as it has High VIF and high p value compared to other
model_4 <-glm(formula = Attrition ~ Age + DistanceFromHome + MonthlyIncome + 
                NumCompaniesWorked + TotalWorkingYears + 
                YearsSinceLastPromotion + YearsWithCurrManager + Average_working_Hours + 
                BusinessTravel.xTravel_Frequently + 
                Department.xResearch...Development + Department.xSales + 
                Education.x3 + Education.x4 + Education.x5 + EducationField.xMarketing + 
                EducationField.xOther + Gender + JobLevel.x2 + JobRole.xLaboratory.Technician + 
                JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
                JobRole.xSales.Executive + MaritalStatus.xMarried + MaritalStatus.xSingle + 
                StockOptionLevel.x1 + TrainingTimesLastYear.x1 +TrainingTimesLastYear.x3 +
                TrainingTimesLastYear.x4 + TrainingTimesLastYear.x5 + TrainingTimesLastYear.x6 + 
                EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                WorkLifeBalance.x4 + JobInvolvement.x3, family = "binomial", 
              data = train)
summary(model_4)
vif(model_4)

#Removing Department.xsales it has High VIF and high p value compared to other
model_5 <-glm(formula = Attrition ~ Age + DistanceFromHome + MonthlyIncome + 
                NumCompaniesWorked + TotalWorkingYears + 
                YearsSinceLastPromotion + YearsWithCurrManager + Average_working_Hours + 
                BusinessTravel.xTravel_Frequently + 
                Department.xResearch...Development +  
                Education.x3 + Education.x4 + Education.x5 + EducationField.xMarketing + 
                EducationField.xOther + Gender + JobLevel.x2 + JobRole.xLaboratory.Technician + 
                JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
                JobRole.xSales.Executive + MaritalStatus.xMarried + MaritalStatus.xSingle + 
                StockOptionLevel.x1 + TrainingTimesLastYear.x1 +TrainingTimesLastYear.x3 +
                TrainingTimesLastYear.x4 + TrainingTimesLastYear.x5 + TrainingTimesLastYear.x6 + 
                EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                WorkLifeBalance.x4 + JobInvolvement.x3, family = "binomial", 
              data = train)
summary(model_5)
vif(model_5)
# cannot exclude any more variable based on vif 
#as most of them have low vif; those with higher vif are very significant and not correlated


#Removing TrainingTimesLastYear.x3 it has High P Value
model_6 <-glm(formula = Attrition ~ Age + DistanceFromHome + MonthlyIncome + 
                NumCompaniesWorked + TotalWorkingYears + 
                YearsSinceLastPromotion + YearsWithCurrManager + Average_working_Hours + 
                BusinessTravel.xTravel_Frequently + 
                Department.xResearch...Development +  
                Education.x3 + Education.x4 + Education.x5 + EducationField.xMarketing + 
                EducationField.xOther + Gender + JobLevel.x2 + JobRole.xLaboratory.Technician + 
                JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
                JobRole.xSales.Executive + MaritalStatus.xMarried + MaritalStatus.xSingle + 
                StockOptionLevel.x1 + TrainingTimesLastYear.x1 +
                TrainingTimesLastYear.x4 + TrainingTimesLastYear.x5 + TrainingTimesLastYear.x6 + 
                EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                WorkLifeBalance.x4 + JobInvolvement.x3, family = "binomial", 
              data = train)
summary(model_6)

#Removing Gender it has High P Value
model_7 <-glm(formula = Attrition ~ Age + DistanceFromHome + MonthlyIncome + 
                NumCompaniesWorked + TotalWorkingYears + 
                YearsSinceLastPromotion + YearsWithCurrManager + Average_working_Hours + 
                BusinessTravel.xTravel_Frequently + 
                Department.xResearch...Development +  
                Education.x3 + Education.x4 + Education.x5 + EducationField.xMarketing + 
                EducationField.xOther + JobLevel.x2 + JobRole.xLaboratory.Technician + 
                JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
                JobRole.xSales.Executive + MaritalStatus.xMarried + MaritalStatus.xSingle + 
                StockOptionLevel.x1 + TrainingTimesLastYear.x1 +
                TrainingTimesLastYear.x4 + TrainingTimesLastYear.x5 + TrainingTimesLastYear.x6 + 
                EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                WorkLifeBalance.x4 + JobInvolvement.x3, family = "binomial", 
              data = train)
summary(model_7)

#Removing JobInvolvement.X3 it has High P Value
model_8 <-glm(formula = Attrition ~ Age + DistanceFromHome + MonthlyIncome + 
                NumCompaniesWorked + TotalWorkingYears + 
                YearsSinceLastPromotion + YearsWithCurrManager + Average_working_Hours + 
                BusinessTravel.xTravel_Frequently + 
                Department.xResearch...Development +  
                Education.x3 + Education.x4 + Education.x5 + EducationField.xMarketing + 
                EducationField.xOther + JobLevel.x2 + JobRole.xLaboratory.Technician + 
                JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
                JobRole.xSales.Executive + MaritalStatus.xMarried + MaritalStatus.xSingle + 
                StockOptionLevel.x1 + TrainingTimesLastYear.x1 +
                TrainingTimesLastYear.x4 + TrainingTimesLastYear.x5 + TrainingTimesLastYear.x6 + 
                EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                WorkLifeBalance.x4 , family = "binomial", 
              data = train)
summary(model_8)

#Removing Distance from Home as it has High P Value
model_9 <-glm(formula = Attrition ~ Age + MonthlyIncome + 
                NumCompaniesWorked + TotalWorkingYears + 
                YearsSinceLastPromotion + YearsWithCurrManager + Average_working_Hours + 
                BusinessTravel.xTravel_Frequently + 
                Department.xResearch...Development +  
                Education.x3 + Education.x4 + Education.x5 + EducationField.xMarketing + 
                EducationField.xOther + JobLevel.x2 + JobRole.xLaboratory.Technician + 
                JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
                JobRole.xSales.Executive + MaritalStatus.xMarried + MaritalStatus.xSingle + 
                StockOptionLevel.x1 + TrainingTimesLastYear.x1 +
                TrainingTimesLastYear.x4 + TrainingTimesLastYear.x5 + TrainingTimesLastYear.x6 + 
                EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                WorkLifeBalance.x4 , family = "binomial", 
              data = train)
summary(model_9)
#Removing Trainingtimeslastyear.x1 as it has High P Value
model_10<-glm(formula = Attrition ~ Age + MonthlyIncome + 
                NumCompaniesWorked + TotalWorkingYears + 
                YearsSinceLastPromotion + YearsWithCurrManager + Average_working_Hours + 
                BusinessTravel.xTravel_Frequently + 
                Department.xResearch...Development +  
                Education.x3 + Education.x4 + Education.x5 + EducationField.xMarketing + 
                EducationField.xOther + JobLevel.x2 + JobRole.xLaboratory.Technician + 
                JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
                JobRole.xSales.Executive + MaritalStatus.xMarried + MaritalStatus.xSingle + 
                StockOptionLevel.x1 +
                TrainingTimesLastYear.x4 + TrainingTimesLastYear.x5 + TrainingTimesLastYear.x6 + 
                EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                WorkLifeBalance.x4 , family = "binomial", 
              data = train)
summary(model_10)

#Removing Trainingtimeslastyear.x1 as it has High P Value
model_10<-glm(formula = Attrition ~ Age + MonthlyIncome + 
                NumCompaniesWorked + TotalWorkingYears + 
                YearsSinceLastPromotion + YearsWithCurrManager + Average_working_Hours + 
                BusinessTravel.xTravel_Frequently + 
                Department.xResearch...Development +  
                Education.x3 + Education.x4 + Education.x5 + EducationField.xMarketing + 
                EducationField.xOther + JobLevel.x2 + JobRole.xLaboratory.Technician + 
                JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
                JobRole.xSales.Executive + MaritalStatus.xMarried + MaritalStatus.xSingle + 
                StockOptionLevel.x1 +
                TrainingTimesLastYear.x4 + TrainingTimesLastYear.x5 + TrainingTimesLastYear.x6 + 
                EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                WorkLifeBalance.x4 , family = "binomial", 
              data = train)
summary(model_10)

#Removing Education.x3 as it has High P Value
model_11<-glm(formula = Attrition ~ Age + MonthlyIncome + 
                NumCompaniesWorked + TotalWorkingYears + 
                YearsSinceLastPromotion + YearsWithCurrManager + Average_working_Hours + 
                BusinessTravel.xTravel_Frequently + 
                Department.xResearch...Development +  
                Education.x4 + Education.x5 + EducationField.xMarketing + 
                EducationField.xOther + JobLevel.x2 + JobRole.xLaboratory.Technician + 
                JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
                JobRole.xSales.Executive + MaritalStatus.xMarried + MaritalStatus.xSingle + 
                StockOptionLevel.x1 +
                TrainingTimesLastYear.x4 + TrainingTimesLastYear.x5 + TrainingTimesLastYear.x6 + 
                EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                WorkLifeBalance.x4 , family = "binomial", 
              data = train)
summary(model_11)

#Removing Education.x5 as it has High P Value
model_12<-glm(formula = Attrition ~ Age + MonthlyIncome + 
                NumCompaniesWorked + TotalWorkingYears + 
                YearsSinceLastPromotion + YearsWithCurrManager + Average_working_Hours + 
                BusinessTravel.xTravel_Frequently + 
                Department.xResearch...Development +  
                Education.x4 + EducationField.xMarketing + 
                EducationField.xOther + JobLevel.x2 + JobRole.xLaboratory.Technician + 
                JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
                JobRole.xSales.Executive + MaritalStatus.xMarried + MaritalStatus.xSingle + 
                StockOptionLevel.x1 +
                TrainingTimesLastYear.x4 + TrainingTimesLastYear.x5 + TrainingTimesLastYear.x6 + 
                EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                WorkLifeBalance.x4 , family = "binomial", 
              data = train)
summary(model_12)

#Removing Education.x4 as it has High P Value
model_13<-glm(formula = Attrition ~ Age + MonthlyIncome + 
                NumCompaniesWorked + TotalWorkingYears + 
                YearsSinceLastPromotion + YearsWithCurrManager + Average_working_Hours + 
                BusinessTravel.xTravel_Frequently + 
                Department.xResearch...Development +  
                EducationField.xMarketing + 
                EducationField.xOther + JobLevel.x2 + JobRole.xLaboratory.Technician + 
                JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
                JobRole.xSales.Executive + MaritalStatus.xMarried + MaritalStatus.xSingle + 
                StockOptionLevel.x1 +
                TrainingTimesLastYear.x4 + TrainingTimesLastYear.x5 + TrainingTimesLastYear.x6 + 
                EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                WorkLifeBalance.x4 , family = "binomial", 
              data = train)
summary(model_13)

#Removing MaritalStatus.xMarried as it has High P Value
model_14<-glm(formula = Attrition ~ Age + MonthlyIncome + 
                NumCompaniesWorked + TotalWorkingYears + 
                YearsSinceLastPromotion + YearsWithCurrManager + Average_working_Hours + 
                BusinessTravel.xTravel_Frequently + 
                Department.xResearch...Development +  
                EducationField.xMarketing + 
                EducationField.xOther + JobLevel.x2 + JobRole.xLaboratory.Technician + 
                JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
                JobRole.xSales.Executive + MaritalStatus.xSingle + 
                StockOptionLevel.x1 +
                TrainingTimesLastYear.x4 + TrainingTimesLastYear.x5 + TrainingTimesLastYear.x6 + 
                EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                WorkLifeBalance.x4 , family = "binomial", 
              data = train)
summary(model_14)

#Removing StockOptionLevelx1 as it has High P Value
model_15<-glm(formula = Attrition ~ Age + MonthlyIncome + 
                NumCompaniesWorked + TotalWorkingYears + 
                YearsSinceLastPromotion + YearsWithCurrManager + Average_working_Hours + 
                BusinessTravel.xTravel_Frequently + 
                Department.xResearch...Development +  
                EducationField.xMarketing + 
                EducationField.xOther + JobLevel.x2 + JobRole.xLaboratory.Technician + 
                JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
                JobRole.xSales.Executive + MaritalStatus.xSingle + 
                TrainingTimesLastYear.x4 + TrainingTimesLastYear.x5 + TrainingTimesLastYear.x6 + 
                EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                WorkLifeBalance.x4 , family = "binomial", 
              data = train)
summary(model_15)
#Removing Department Research and development as it is least significant
model_16<-glm(formula = Attrition ~ Age + MonthlyIncome + 
                NumCompaniesWorked + TotalWorkingYears + 
                YearsSinceLastPromotion + YearsWithCurrManager + Average_working_Hours + 
                BusinessTravel.xTravel_Frequently + EducationField.xMarketing + 
                EducationField.xOther + JobLevel.x2 + JobRole.xLaboratory.Technician + 
                JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
                JobRole.xSales.Executive + MaritalStatus.xSingle + 
                TrainingTimesLastYear.x4 + TrainingTimesLastYear.x5 + TrainingTimesLastYear.x6 + 
                EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                WorkLifeBalance.x4 , family = "binomial", 
              data = train)
summary(model_16)

#Removing EducationField.xMarketing as it has high p value
model_17<-glm(formula = Attrition ~ Age + MonthlyIncome + 
                NumCompaniesWorked + TotalWorkingYears + 
                YearsSinceLastPromotion + YearsWithCurrManager + Average_working_Hours + 
                BusinessTravel.xTravel_Frequently + 
                EducationField.xOther + JobLevel.x2 + JobRole.xLaboratory.Technician + 
                JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
                JobRole.xSales.Executive + MaritalStatus.xSingle + 
                TrainingTimesLastYear.x4 + TrainingTimesLastYear.x5 + TrainingTimesLastYear.x6 + 
                EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                WorkLifeBalance.x4 , family = "binomial", 
              data = train)
summary(model_17)

#Removing EducationField.xOther as it is high p value
model_18<-glm(formula = Attrition ~ Age + MonthlyIncome + 
                NumCompaniesWorked + TotalWorkingYears + 
                YearsSinceLastPromotion + YearsWithCurrManager + Average_working_Hours + 
                BusinessTravel.xTravel_Frequently + 
                JobLevel.x2 + JobRole.xLaboratory.Technician + 
                JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
                JobRole.xSales.Executive + MaritalStatus.xSingle + 
                TrainingTimesLastYear.x4 + TrainingTimesLastYear.x5 + TrainingTimesLastYear.x6 + 
                EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                WorkLifeBalance.x4 , family = "binomial", 
              data = train)
summary(model_18)

#Removing Joblevel.x2 as it is less significant
model_19<-glm(formula = Attrition ~ Age + MonthlyIncome + 
                NumCompaniesWorked + TotalWorkingYears + 
                YearsSinceLastPromotion + YearsWithCurrManager + Average_working_Hours + 
                BusinessTravel.xTravel_Frequently + 
                JobRole.xLaboratory.Technician + 
                JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
                JobRole.xSales.Executive + MaritalStatus.xSingle + 
                TrainingTimesLastYear.x4 + TrainingTimesLastYear.x5 + TrainingTimesLastYear.x6 + 
                EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                WorkLifeBalance.x4 , family = "binomial", 
              data = train)
summary(model_19)

#Removing Monthlyincome as it is less significant
model_20<-glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                YearsSinceLastPromotion + YearsWithCurrManager + Average_working_Hours + 
                BusinessTravel.xTravel_Frequently + 
                JobRole.xLaboratory.Technician + 
                JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
                JobRole.xSales.Executive + MaritalStatus.xSingle + 
                TrainingTimesLastYear.x4 + TrainingTimesLastYear.x5 + TrainingTimesLastYear.x6 + 
                EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                WorkLifeBalance.x4 , family = "binomial", 
              data = train)
summary(model_20)

#Removing TrainingTimeLastYear.x4 as it is less significant
model_21<-glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                YearsSinceLastPromotion + YearsWithCurrManager + Average_working_Hours + 
                BusinessTravel.xTravel_Frequently + 
                JobRole.xLaboratory.Technician + 
                JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
                JobRole.xSales.Executive + MaritalStatus.xSingle + 
                TrainingTimesLastYear.x5 + TrainingTimesLastYear.x6 + 
                EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                WorkLifeBalance.x4 , family = "binomial", 
              data = train)
summary(model_21)

#Removing TrainingTimeLastYear.x5 as it is less significant
model_22<-glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                YearsSinceLastPromotion + YearsWithCurrManager + Average_working_Hours + 
                BusinessTravel.xTravel_Frequently + 
                JobRole.xLaboratory.Technician + 
                JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
                JobRole.xSales.Executive + MaritalStatus.xSingle + 
                TrainingTimesLastYear.x6 + 
                EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                WorkLifeBalance.x4 , family = "binomial", 
              data = train)
summary(model_22)

#Removing TrainingTimeLastYear.x6 as it is less significant
model_23<-glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                YearsSinceLastPromotion + YearsWithCurrManager + Average_working_Hours + 
                BusinessTravel.xTravel_Frequently + 
                JobRole.xLaboratory.Technician + 
                JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
                JobRole.xSales.Executive + MaritalStatus.xSingle + 
                EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                WorkLifeBalance.x4 , family = "binomial", 
              data = train)
summary(model_23)
#Removing JobRole.Xlaboratory.Technician as it is less significant
model_24<-glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                YearsSinceLastPromotion + YearsWithCurrManager + Average_working_Hours + 
                BusinessTravel.xTravel_Frequently + 
                JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
                JobRole.xSales.Executive + MaritalStatus.xSingle + 
                EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                WorkLifeBalance.x4 , family = "binomial", 
              data = train)
summary(model_24)

#Removing JobRole.XResearch.Scientist as it is less significant
model_25<-glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                YearsSinceLastPromotion + YearsWithCurrManager + Average_working_Hours + 
                BusinessTravel.xTravel_Frequently + 
                JobRole.xResearch.Director + 
                JobRole.xSales.Executive + MaritalStatus.xSingle + 
                EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                WorkLifeBalance.x4 , family = "binomial", 
              data = train)
summary(model_25)

#Removing Worklifebalance.x4as it is less significant
model_26<-glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                YearsSinceLastPromotion + YearsWithCurrManager + Average_working_Hours + 
                BusinessTravel.xTravel_Frequently + 
                JobRole.xResearch.Director + 
                JobRole.xSales.Executive + MaritalStatus.xSingle + 
                EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 , family = "binomial", 
              data = train)
summary(model_26)
#Removing Worklifebalance.x2as it is less significant
model_26<-glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                YearsSinceLastPromotion + YearsWithCurrManager + Average_working_Hours + 
                BusinessTravel.xTravel_Frequently + 
                JobRole.xResearch.Director + 
                JobRole.xSales.Executive + MaritalStatus.xSingle + 
                EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x3 , family = "binomial", 
              data = train)
summary(model_26)

#Removing JobRole.Sales.Executive as it is less significant
model_27<-glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                YearsSinceLastPromotion + YearsWithCurrManager + Average_working_Hours + 
                BusinessTravel.xTravel_Frequently + 
                JobRole.xResearch.Director + 
                MaritalStatus.xSingle + 
                EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x3 , family = "binomial", 
              data = train)
summary(model_27)

#Removing JobRole.Research Director as it is less significant
model_27<-glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                YearsSinceLastPromotion + YearsWithCurrManager + Average_working_Hours + 
                BusinessTravel.xTravel_Frequently + 
                MaritalStatus.xSingle + 
                EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x3 , family = "binomial", 
              data = train)
summary(model_27)

#Removing WorkLifeBalance.x3 as it is less significant
model_28<-glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                YearsSinceLastPromotion + YearsWithCurrManager + Average_working_Hours + 
                BusinessTravel.xTravel_Frequently + 
                MaritalStatus.xSingle + 
                EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 , family = "binomial", 
              data = train)
summary(model_28)


final_model<-model_28

#Model Evaluation

#predicted probabilities of Attrition 1 for test data

test_pred = predict(final_model, type = "response", 
                    newdata = test[,-1])
summary(test_pred)

test$prob <- test_pred
View(test)

# Let's use the probability cutoff of 50%.

test_pred_attrition<- factor(ifelse(test_pred >= 0.50, "Yes", "No"))
test_actual_attrition <- factor(ifelse(test$Attrition==1,"Yes","No"))


table(test_actual_attrition,test_pred_attrition)

#Checking for other levels of Cutoff
# For the probability cutoff of 40%.

test_pred_attrition <- factor(ifelse(test_pred >= 0.50, "Yes", "No"))
test_conf <- confusionMatrix(test_pred_attrition, test_actual_attrition, positive = "Yes")
test_conf

# For the probability cutoff of 30%.

test_pred_attrition <- factor(ifelse(test_pred >= 0.30, "Yes", "No"))
test_conf <- confusionMatrix(test_pred_attrition, test_actual_attrition, positive = "Yes")
test_conf

# For the probability cutoff of 20%.

test_pred_attrition <- factor(ifelse(test_pred >= 0.20, "Yes", "No"))
test_conf <- confusionMatrix(test_pred_attrition, test_actual_attrition, positive = "Yes")
test_conf

#Finding the optimal value of probability

perform_fn <- function(cutoff) 
{
  predicted_attrition<- factor(ifelse(test_pred >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_attrition, test_actual_attrition, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}


#Summary of test probability

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

cutoff #0.1616

# Let's choose a cutoff value of 0.1616 for final model

test_cutoff_attrition<- factor(ifelse(test_pred >=0.1616, "Yes", "No"))

conf_final <- confusionMatrix(test_cutoff_attrition, test_actual_attrition, positive = "Yes")

acc <- conf_final$overall[1]

sens <- conf_final$byClass[1]

spec <- conf_final$byClass[2]

acc
#0.724
sens
#0.73
spec
#0.722

#K-Statistics-Test Data
test_cutoff_attrition <- ifelse(test_cutoff_attrition=="Yes",1,0)
test_actual_attrition <- ifelse(test_actual_attrition=="Yes",1,0)

#using testing  data
pred_object_test<- prediction(test_cutoff_attrition, test_actual_attrition)

performance_measures_test<- performance(pred_object_test, "tpr", "fpr")

#Plotting ROC Curve

plot(performance_measures_test)

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]])

max(ks_table_test)
#0.46

# Lift & Gain Chart 
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
#Majority of the resp is in top 3 deciles

#Conclusion
#1.Age:Attrition Higher on lower age group
#2.Average Working Hours:Attrition Rate is higher for those working more than 8 hours
#Company need to look into that
#3.NumCompanies worked:If the no:of companies worked is more,Attrition rate increases
#4.Business Travel Frequently:Frequent Business Travel is a reason for attrition
#5.Enviornment and Job Satisfaction is important for employee
#6.Those who are single tend to leave the company more
#7.Total Working Years:As the number of years increases,attrition tend to be lower
#8.Years With Current Manager:Higher attrition for those with lower years under a manager
#9.Years since last promotion:Attrition higher for those who are promoted recently