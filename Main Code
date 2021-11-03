# Aaron Chin z5164443 ACTL3142 Assignment #

##### EDA #####

# importing library and dataset
library(readr)
library(dplyr) # data wrangling
library(ggplot2) # making plots
library(tidyverse)
library(tidyr)
library(visdat) # for visual correlation and vis_miss
library(mice) # for imputation
library(caret) # for partitioning data
library(smotefamily) # for balancing data
library(stringr) # for splitting
library(precrec) # for ROCs
library(class) # for KNN
library(leaps) # for subset selection
library(glmnet) # for lasso regression
library(rpart) # decision tree
library(rpart.plot) # plotting decision tree
library(randomForest) # random forest

setwd("~/Documents/OneDrive - UNSW/ACTL3142/3142 assignment")
training_data <- read_csv("training_data.csv")
eval_data <- read_csv("eval_data.csv")
View(training_data)

dim(training_data) 
# there are 40000 rows and 50 variables
glimpse(training_data)

training_data[training_data==""]<-NA # change empty cells to NA

summary(training_data)

# CLEANING

# Change "Unknown" to NA in HandsetPrice and to numeric
training_data = training_data %>% 
  mutate(HandsetPrice = ifelse(HandsetPrice == "Unknown", NA, HandsetPrice))
training_data$HandsetPrice = as.numeric(training_data$HandsetPrice)

eval_data = eval_data %>% 
  mutate(HandsetPrice = ifelse(HandsetPrice == "Unknown", NA, HandsetPrice))
eval_data$HandsetPrice = as.numeric(eval_data$HandsetPrice)

# Split CreditRating column
CR = str_split_fixed(training_data$CreditRating, "-", 2)
training_data = training_data %>% 
mutate(CreditRatingLevel = CR[,1])
training_data = training_data %>% 
  mutate(CreditRatingDesc = CR[,2])
training_data$CreditRatingLevel = as.numeric(training_data$CreditRatingLevel)

CR2 = str_split_fixed(eval_data$CreditRating, "-", 2)
eval_data = eval_data %>% 
  mutate(CreditRatingLevel = CR2[,1])
eval_data = eval_data %>% 
  mutate(CreditRatingDesc = CR2[,2])
eval_data$CreditRatingLevel = as.numeric(eval_data$CreditRatingLevel)

ggplot(data = training_data) +
  geom_bar(mapping = aes(x = Churn))
summary(as.factor(training_data$Churn))
# shows that customers either have or haven't churned, no errors
# quite imbalanced

# visualising missing data using visdat
Subset1 <- dplyr::select(training_data, Churn:DroppedBlockedCalls)
Subset2 <- dplyr::select(training_data, CallForwardingCalls:RVOwner)
Subset3 <- dplyr::select(training_data, BuysViaMailOrder:CreditRatingDesc)

# visualise proportion of missing data
vis_miss(Subset1, sort_miss = TRUE)
vis_miss(Subset2, sort_miss = TRUE)
vis_miss(Subset3, sort_miss = TRUE)
# missing data in: MonthlyRevenue, MonthlyMinutes, TotalRecurringCharge, OverageMinutes, RoamingCalls, ServiceArea, AgeHH1, AgeHH2, HandsetPrice, MaritalStatus


# Analysing some categorical variables

summary(as.factor(training_data$ServiceArea))
summary(as.factor(training_data$PrizmCode)) # nearly half live in "Other"
summary(as.factor(training_data$MaritalStatus)) # large number of "Unknown"
summary(as.factor(training_data$Occupation)) # most classified as "Other"

# add a variable "Married" with 1=Yes, 0=No
training_data = training_data %>% 
  mutate(Married = ifelse(MaritalStatus == "Yes", 1, 0))
training_data$Married = as.numeric(training_data$Married)

eval_data = eval_data %>% 
  mutate(Married = ifelse(MaritalStatus == "Yes", 1, 0))
eval_data$Married = as.numeric(eval_data$Married)

# add a variable "Rural" with 1=Yes, 0=No
training_data = training_data %>% 
  mutate(Rural = ifelse(PrizmCode == "Rural", 1, 0))
training_data$Rural = as.numeric(training_data$Rural)

eval_data = eval_data %>% 
  mutate(Rural = ifelse(PrizmCode == "Rural", 1, 0))
eval_data$Rural = as.numeric(eval_data$Rural)

# add a variable "Suburban" with 1=Yes, 0=No
training_data = training_data %>% 
  mutate(Suburban = ifelse(PrizmCode == "Suburban", 1, 0))
training_data$Suburban = as.numeric(training_data$Suburban)

eval_data = eval_data %>% 
  mutate(Suburban = ifelse(PrizmCode == "Suburban", 1, 0))
eval_data$Suburban = as.numeric(eval_data$Suburban)

# add a variable "Town" with 1=Yes, 0=No
training_data = training_data %>% 
  mutate(Town = ifelse(PrizmCode == "Town", 1, 0))
training_data$Town = as.numeric(training_data$Town)

eval_data = eval_data %>% 
  mutate(Town = ifelse(PrizmCode == "Town", 1, 0))
eval_data$Town = as.numeric(eval_data$Town)

# add a variable "Clerical" with 1=Yes, 0=No
training_data = training_data %>% 
  mutate(Clerical = ifelse(Occupation == "Clerical", 1, 0))
training_data$Clerical = as.numeric(training_data$Clerical)

eval_data = eval_data %>% 
  mutate(Clerical = ifelse(Occupation == "Clerical", 1, 0))
eval_data$Clerical = as.numeric(eval_data$Clerical)

# add a variable "Crafts" with 1=Yes, 0=No
training_data = training_data %>% 
  mutate(Crafts = ifelse(Occupation == "Crafts", 1, 0))
training_data$Crafts = as.numeric(training_data$Crafts)

eval_data = eval_data %>% 
  mutate(Crafts = ifelse(Occupation == "Crafts", 1, 0))
eval_data$Crafts = as.numeric(eval_data$Crafts)

# add a variable "Homemaker" with 1=Yes, 0=No
training_data = training_data %>% 
  mutate(Homemaker = ifelse(Occupation == "Homemaker", 1, 0))
training_data$Homemaker = as.numeric(training_data$Homemaker)

eval_data = eval_data %>% 
  mutate(Homemaker = ifelse(Occupation == "Homemaker", 1, 0))
eval_data$Homemaker = as.numeric(eval_data$Homemaker)

# add a variable "Professional" with 1=Yes, 0=No
training_data = training_data %>% 
  mutate(Professional = ifelse(Occupation == "Professional", 1, 0))
training_data$Professional = as.numeric(training_data$Professional)

eval_data = eval_data %>% 
  mutate(Professional = ifelse(Occupation == "Professional", 1, 0))
eval_data$Professional = as.numeric(eval_data$Professional)

# add a variable "Retired" with 1=Yes, 0=No
training_data = training_data %>% 
  mutate(Retired = ifelse(Occupation == "Retired", 1, 0))
training_data$Retired = as.numeric(training_data$Retired)

eval_data = eval_data %>% 
  mutate(Retired = ifelse(Occupation == "Retired", 1, 0))
eval_data$Retired = as.numeric(eval_data$Retired)

# add a variable "Self-Employed" with 1=Yes, 0=No
training_data = training_data %>% 
  mutate(Self = ifelse(Occupation == "Self", 1, 0))
training_data$Self = as.numeric(training_data$Self)

eval_data = eval_data %>% 
  mutate(Self = ifelse(Occupation == "Self", 1, 0))
eval_data$Self = as.numeric(eval_data$Self)

# add a variable "Student" with 1=Yes, 0=No
training_data = training_data %>% 
  mutate(Student = ifelse(Occupation == "Student", 1, 0))
training_data$Student = as.numeric(training_data$Student)

eval_data = eval_data %>% 
  mutate(Student = ifelse(Occupation == "Student", 1, 0))
eval_data$Student = as.numeric(eval_data$Student)

# Many contradictions in ChildrenInHH

# OUTLIER CHECKS (Numerical)
# if median is at 0, drop it

summary(training_data$HandsetPrice) # majority NAs
hist(training_data$AgeHH1) # most have age 0, many NAs which could mean they don't have one
hist(training_data$AgeHH2) # most have age 0, many NAs which could mean they don't have one
hist(training_data$ActiveSubs) # 11 is an outlier
hist(training_data$ReferralsMadeBySubscriber, main = "Referrals Made By Subscriber", xlab = "Number of Referrals") # there seems to only be one outlier which impacts everything, drop col 42
hist(training_data$RetentionCalls, main = "Retention Calls", xlab = "Number of Calls Made") # most at 0, drop col 39
hist(training_data$RetentionOffersAccepted, main = "Retention Offers Accepted", xlab = "Number of Offers Accepted") # most at 0, drop col 40
hist(training_data$CurrentEquipmentDays) # not a big outlier but values below 0 - which isn't possible
hist(training_data$HandsetModels) # 15 outlier
hist(training_data$Handsets) # 24 outlier
hist(training_data$MonthsInService) # no big outliers here
hist(training_data$CallWaitingCalls) # BIG outlier at 212
hist(training_data$CallForwardingCalls, main = "Call Forwarding Calls", xlab = "Mean number of Call Forwarding Calls") # big outlier while everything else is at 0, drop 19
hist(training_data$DroppedBlockedCalls) # big outlier at 411
hist(training_data$OffPeakCallsInOut) # big outlier at 1474.7
hist(training_data$PeakCallsInOut) # big outlier at 1921.30
hist(training_data$InboundCalls) # big outlier at 519.3
hist(training_data$OutboundCalls) # big outlier at 644.3
hist(training_data$ReceivedCalls) # big outlier at 2692.4
hist(training_data$ThreewayCalls, main = "Threeway Calls", xlab = "Mean number of Threeway Calls") # big outlier at 65.3, most others are at 0. Drop col 12
hist(training_data$CustomerCareCalls, main = "Customer Care Calls", xlab = "Mean number of Customer Care Calls") # big outlier at 327.30, most othrs at 0, drop col 11
hist(training_data$UnansweredCalls) # big outlier at 848.70
hist(training_data$BlockedCalls) # big outlier at 384.3
hist(training_data$DroppedCalls) # big outlier at 221.7
hist(training_data$RoamingCalls, main = "Roaming Calls", xlab = "Mean number of Roaming Calls") # big outlier at 1112.400, drop col 7 because most at 0
hist(training_data$OverageMinutes) # big outlier at 2976.00
hist(training_data$TotalRecurringCharge) # big outlier at 400, some values below 0
hist(training_data$MonthlyMinutes) # big outlier at 7359.0
hist(training_data$MonthlyRevenue) # outlier at -6.17, 1223.38

#remove these, Original Credit Rating column, Original MaritalStatus, Original Occupation, CustID, ServiceArea, Original PrizmCode and Original ChildrenInHH

training_data_trim = training_data[-c(1, 7, 11, 12, 19, 23, 29, 39, 40, 42, 47, 48, 49, 50)]
dim(training_data_trim) # 50 variables left
summary(training_data_trim)

CustID = eval_data$CustomerID
eval_data_trim = eval_data[-c(1, 6, 10, 11, 18, 22, 28, 38, 39, 41, 46, 47, 48, 49)] # shift down 1 as no churn column
dim(eval_data_trim) # 49 variables left
summary(eval_data_trim)

# Replace NA values in AgeHH1, AgeHH2 with 0, assuming it means they don't have household members
# we also assume that a value of 0 also means that they don't have household members

training_data_trim$AgeHH1[is.na(training_data_trim$AgeHH1)] <- 0 
training_data_trim$AgeHH2[is.na(training_data_trim$AgeHH2)] <- 0 

eval_data_trim$AgeHH1[is.na(eval_data_trim$AgeHH1)] <- 0 
eval_data_trim$AgeHH2[is.na(eval_data_trim$AgeHH2)] <- 0

# add a variable "Household Members" 
training_data_trim = training_data_trim %>% 
  mutate(HH = ifelse(AgeHH1 == "0", ifelse(AgeHH2 == "0", 0, 1), ifelse(AgeHH2 == "0", 1, 2)))
training_data_trim$HH = as.numeric(training_data_trim$HH)

eval_data_trim = eval_data_trim %>% 
  mutate(HH = ifelse(AgeHH1 == "0", ifelse(AgeHH2 == "0", 0, 1), ifelse(AgeHH2 == "0", 1, 2)))
eval_data_trim$HH = as.numeric(eval_data_trim$HH)

# imputing the other NA values using MICE

imputed_Data <- mice(training_data_trim, m=5, maxit = 5, method = 'pmm', seed = 500) # note: takes a while
training_data2 <- complete(imputed_Data)

imputed_eval <- mice(eval_data_trim, m=5, maxit = 5, method = 'pmm', seed = 500)
eval_imputed = complete(imputed_eval)

# remove outlier values

training_data3 = training_data2

training_data3<-subset(training_data2, MonthlyRevenue < (quantile(training_data2$MonthlyRevenue, probs = 0.75)+1.5*IQR(training_data2$MonthlyRevenue, na.rm = TRUE)))

training_data3<-subset(training_data3, MonthlyMinutes < (quantile(training_data2$MonthlyMinutes, probs = 0.75)+1.5*IQR(training_data2$MonthlyMinutes, na.rm = TRUE)))

training_data3<-subset(training_data3, TotalRecurringCharge < (quantile(training_data2$TotalRecurringCharge, probs = 0.75)+1.5*IQR(training_data2$TotalRecurringCharge, na.rm = TRUE)))
training_data3<-subset(training_data3, TotalRecurringCharge > 0) # per histogram discovery

training_data3<-subset(training_data3, OverageMinutes < (quantile(training_data2$OverageMinutes, probs = 0.75)+1.5*IQR(training_data2$OverageMinutes, na.rm = TRUE)))

training_data3<-subset(training_data3, DroppedCalls < (quantile(training_data2$DroppedCalls, probs = 0.75)+1.5*IQR(training_data2$DroppedCalls, na.rm = TRUE)))

training_data3<-subset(training_data3, BlockedCalls < (quantile(training_data2$BlockedCalls, probs = 0.75)+1.5*IQR(training_data2$BlockedCalls, na.rm = TRUE)))

training_data3<-subset(training_data3, UnansweredCalls < (quantile(training_data2$UnansweredCalls, probs = 0.75)+1.5*IQR(training_data2$UnansweredCalls, na.rm = TRUE)))

training_data3<-subset(training_data3, ReceivedCalls < (quantile(training_data2$ReceivedCalls, probs = 0.75)+1.5*IQR(training_data2$ReceivedCalls, na.rm = TRUE)))

training_data3<-subset(training_data3, OutboundCalls < (quantile(training_data2$OutboundCalls, probs = 0.75)+1.5*IQR(training_data2$OutboundCalls, na.rm = TRUE)))

training_data3<-subset(training_data3, InboundCalls < (quantile(training_data2$InboundCalls, probs = 0.75)+1.5*IQR(training_data2$InboundCalls, na.rm = TRUE)))

training_data3<-subset(training_data3, PeakCallsInOut < (quantile(training_data2$PeakCallsInOut, probs = 0.75)+1.5*IQR(training_data2$PeakCallsInOut, na.rm = TRUE)))

training_data3<-subset(training_data3, OffPeakCallsInOut < (quantile(training_data2$OffPeakCallsInOut, probs = 0.75)+1.5*IQR(training_data2$OffPeakCallsInOut, na.rm = TRUE)))

training_data3<-subset(training_data3, DroppedBlockedCalls < (quantile(training_data2$DroppedBlockedCalls, probs = 0.75)+1.5*IQR(training_data2$DroppedBlockedCalls, na.rm = TRUE)))

training_data3<-subset(training_data3, CallWaitingCalls < (quantile(training_data2$CallWaitingCalls, probs = 0.75)+1.5*IQR(training_data2$CallWaitingCalls, na.rm = TRUE)))

training_data3<-subset(training_data3, Handsets < (quantile(training_data2$Handsets, probs = 0.75)+1.5*IQR(training_data2$Handsets, na.rm = TRUE)))

training_data3<-subset(training_data3, HandsetModels < (quantile(training_data2$HandsetModels, probs = 0.75)+1.5*IQR(training_data2$HandsetModels, na.rm = TRUE)))

training_data3<-subset(training_data3, ActiveSubs < (5+1.5*IQR(training_data2$ActiveSubs, na.rm = TRUE))) # from histogram discovery

training_data3<-subset(training_data3, CurrentEquipmentDays >= 0) # from histogram discovery

training_data3<-subset(training_data3, HandsetPrice < (quantile(training_data2$HandsetPrice, probs = 0.75)+1.5*IQR(training_data2$HandsetPrice, na.rm = TRUE)))

# Months in Service, AgeHH1, AgeHH2 reasonable

dim(training_data3) # 25028 observations left, 50 variables
summary(training_data3)

# Visual Correlation

Num3 = select(training_data3, MonthlyRevenue:AgeHH2)
dim(Num3)
vis_cor(Num3)

# categorical by categorical - comparing churn to the rest

# Status
ggplot(training_data3, aes(x = CreditRatingDesc, fill = Churn)) + geom_bar(position = "dodge") + ggtitle("Credit Rating Distribution")
ggplot(training_data3, aes(x = IncomeGroup, fill = Churn)) + geom_bar(position = "dodge") + ggtitle("Income Group Distribution")

# Vehicles and Assets
ggplot(training_data3, aes(x = OwnsMotorcycle, fill = TruckOwner)) + geom_bar(position = "dodge") # If you're a motorcyclist you're likely to be a truck owner
ggplot(training_data3, aes(x = RVOwner, fill = TruckOwner)) + geom_bar(position = "dodge") # all RV Owners are Truck owners, some truck owners aren't. Quite imbalanced too

ggplot(training_data3, aes(x = TruckOwner, fill = Churn)) + geom_bar(position = "dodge")
ggplot(training_data3, aes(x = RVOwner, fill = Churn)) + geom_bar(position = "dodge") 
ggplot(training_data3, aes(x = OwnsMotorcycle, fill = Churn)) + geom_bar(position = "dodge") 
ggplot(training_data3, aes(x = OwnsComputer, fill = Churn)) + geom_bar(position = "dodge")
ggplot(training_data3, aes(x = HasCreditCard, fill = Churn)) + geom_bar(position = "dodge") 

# Handset related
ggplot(training_data3, aes(x = NewCellphoneUser, fill = Churn)) + geom_bar(position = "dodge") 
ggplot(training_data3, aes(x = HandsetRefurbished, fill = Churn)) + geom_bar(position = "dodge") + ggtitle("Refurbished Handset Distribution")
ggplot(training_data3, aes(x = HandsetWebCapable, fill = Churn)) + geom_bar(position = "dodge") 


# Mail Offers
ggplot(training_data3, aes(x = RespondsToMailOffers, fill = BuysViaMailOrder)) + geom_bar(position = "dodge") # highly correlated

ggplot(training_data3, aes(x = BuysViaMailOrder, fill = Churn)) + geom_bar(position = "dodge")
ggplot(training_data3, aes(x = OptOutMailings, fill = Churn)) + geom_bar(position = "dodge") 
ggplot(training_data3, aes(x = RespondsToMailOffers, fill = Churn)) + geom_bar(position = "dodge") 

# Calls
ggplot(training_data3, aes(x = MadeCallToRetentionTeam, fill = Churn)) + geom_bar(position = "dodge")


##### BALANCING DATA ######

training_data4 = training_data3

# SMOTE only works with numerical - so convert the rest to numerical with 1 = Yes and 0 = No
training_data4$Churn <- ifelse(training_data4$Churn=="Yes", 1, 0) # 1 = Churned, 0 = Didn't churn
training_data4$Churn = as.numeric(training_data4$Churn)

training_data4$HandsetRefurbished <- ifelse(training_data4$HandsetRefurbished=="Yes", 1, 0)
training_data4$HandsetRefurbished = as.numeric(training_data4$HandsetRefurbished)

training_data4$HandsetWebCapable <- ifelse(training_data4$HandsetWebCapable=="Yes", 1, 0)
training_data4$HandsetWebCapable = as.numeric(training_data4$HandsetWebCapable)

training_data4$TruckOwner <- ifelse(training_data4$TruckOwner=="Yes", 1, 0)
training_data4$TruckOwner = as.numeric(training_data4$TruckOwner)

training_data4$RVOwner <- ifelse(training_data4$RVOwner=="Yes", 1, 0)
training_data4$RVOwner = as.numeric(training_data4$RVOwner)

training_data4$BuysViaMailOrder <- ifelse(training_data4$BuysViaMailOrder=="Yes", 1, 0)
training_data4$BuysViaMailOrder = as.numeric(training_data4$BuysViaMailOrder)

training_data4$RespondsToMailOffers <- ifelse(training_data4$RespondsToMailOffers=="Yes", 1, 0)
training_data4$RespondsToMailOffers = as.numeric(training_data4$RespondsToMailOffers)

training_data4$OptOutMailings <- ifelse(training_data4$OptOutMailings=="Yes", 1, 0)
training_data4$OptOutMailings = as.numeric(training_data4$OptOutMailings)

training_data4$OwnsComputer <- ifelse(training_data4$OwnsComputer=="Yes", 1, 0)
training_data4$OwnsComputer = as.numeric(training_data4$OwnsComputer)

training_data4$HasCreditCard <- ifelse(training_data4$HasCreditCard=="Yes", 1, 0)
training_data4$HasCreditCard = as.numeric(training_data4$HasCreditCard)

training_data4$NewCellphoneUser <- ifelse(training_data4$NewCellphoneUser=="Yes", 1, 0)
training_data4$NewCellphoneUser = as.numeric(training_data4$NewCellphoneUser)

training_data4$OwnsMotorcycle <- ifelse(training_data4$OwnsMotorcycle=="Yes", 1, 0)
training_data4$OwnsMotorcycle = as.numeric(training_data4$OwnsMotorcycle)

training_data4$MadeCallToRetentionTeam <- ifelse(training_data4$MadeCallToRetentionTeam=="Yes", 1, 0)
training_data4$MadeCallToRetentionTeam = as.numeric(training_data4$MadeCallToRetentionTeam)

training_data4 = select(training_data4, -c(CreditRatingDesc))
summary(training_data4)
dim(training_data4) # 25028 observations, 49 variables

# divide into training set and test set
set.seed(123)
train <- createDataPartition(training_data4$Churn, p = 0.8, times = 1, list = F) # 80-20 is a good starting point
train_orig <- training_data4[ train,]
test <- training_data4[-train,]

table(train_orig$Churn) # quite imbalanced with 14128 No and 5895 Yes
balance = SMOTE(train_orig, train_orig$Churn, K = 5) # using SMOTE to balance the data
table(balance$data$Churn) # 14128 No and 11790 Yes - much more balanced
# Only works with numeric dataset

train_balanced = balance$data
dim(train_balanced) # 25918 observations, 50 variables
colnames(train_balanced[50]) # = class
train_balanced = train_balanced[-50] # remove the "class" variable

# Converted Churn back to factor
train_balanced$Churn = as.factor(train_balanced$Churn)
test$Churn = as.factor(test$Churn)


##### MODELLING #####

# For each model
# a. Find confusion matrix
# b. Find ROC and the AUC

# 1 - Logistic Regression

fit1 <- glm(Churn ~ ., family = "binomial", data = train_balanced)
summary(fit1)

pred.fit1 <- predict(fit1, test, type = "response")
pred.val1 = rep(0, length(pred.fit1))
pred.val1[pred.fit1 > 0.5] = 1

CM1 = table(pred = pred.val1, true=test$Churn) # Confusion Matrix

# CM[1]=TN, CM[2]=FP, CM[3]=FN, CM[4]=TP

accuracy1 = (CM1[1]+CM1[4])/(CM1[1]+CM1[2]+CM1[3]+CM1[4])
accuracy1 # 0.6397602

precision1 = CM1[4]/(CM1[2]+CM1[4])
precision1 # 0.3809854

sensitivity1 = CM1[4]/(CM1[3]+CM1[4])
sensitivity1 # 0.3760274

specificity1 = CM1[1]/(CM1[1]+CM1[2])
specificity1 # 0.748378

F1.1 = 2*(precision1*sensitivity1)/(precision1+sensitivity1)
F1.1 # 0.3784902

ROC1 <- evalmod(scores = pred.val1, labels = test$Churn)
auc(ROC1) # 0.5622027 ROC
autoplot(ROC1)

# 2 - Backward Stepwise
fit2 = step(fit1, Churn~., direction = "backward") 

pred.fit2 <- predict(fit2, test, type = "response")
pred.val2 = rep(0, length(pred.fit2))
pred.val2[pred.fit2 > 0.5] = 1

CM2 = table(pred = pred.val2, true=test$Churn)

accuracy2 = (CM2[1]+CM2[4])/(CM2[1]+CM2[2]+CM2[3]+CM2[4])
accuracy2 # 0.638961

precision2 = CM2[4]/(CM2[2]+CM2[4])
precision2 # 0.3795975

sensitivity2 = CM2[4]/(CM2[3]+CM2[4])
sensitivity2 # 0.3746575

specificity2 = CM2[1]/(CM2[1]+CM2[2])
specificity2 # 0.7478138

F1.2 = 2*(precision2*sensitivity2)/(precision2+sensitivity2)
F1.2 # 0.3771113

ROC2 <- evalmod(scores = pred.val2, labels = test$Churn)
auc(ROC2) # 0.5612357 ROC
autoplot(ROC2)

# 3 - KNN

error_rates3 = rep(0,10)

for (i in 1:10){
  fit3 = knn(train_balanced, test, train_balanced$Churn, k=i)
  table3 = table(pred=fit3, true=test$Churn)
  error_rates3[i] = (table3[2] + table3[3])/(table3[1]+table3[2]+table3[3]+table3[4])*100
}

plot(1:10, error_rates3) # from the plot it can be seen K=1 is best as it gives the lowest error rate

fit3.1 = knn(train_balanced, test, train_balanced$Churn, k=1)
CM3 = table(pred=fit3.1, true=test$Churn)

accuracy3 = (CM3[1]+CM3[4])/(CM3[1]+CM3[2]+CM3[3]+CM3[4])
accuracy3 # 0.586014

precision3 = CM3[4]/(CM3[2]+CM3[4])
precision3 # 0.33

sensitivity3 = CM3[4]/(CM3[3]+CM3[4])
sensitivity3 # 0.4068493

specificity3 = CM3[1]/(CM3[1]+CM3[2])
specificity3 # 0.6598025

F1.3 = 2*(precision3*sensitivity3)/(precision3+sensitivity3)
F1.3 # 0.3644172

ROC3 <- evalmod(scores = as.numeric(fit3.1), labels = test$Churn)
auc(ROC3) # 0.5333259 ROC
autoplot(ROC3)

# 4-5: Shrinkage
lambda.grid = 10^seq(10,-2,length.out=100)
train.modelmatrix = model.matrix(Churn~., train_balanced)[,-1]
test.modelmatrix = model.matrix(Churn~., test)[,-1]
y_train = train_balanced$Churn

# 4 - Logistic with Ridge (alpha=0)
fit4= glmnet(train.modelmatrix, y_train, alpha=0, family = "binomial", lambda = lambda.grid)
cv.fit4= cv.glmnet(train.modelmatrix, y_train, alpha=0, family = "binomial", lambda = lambda.grid)
plot(fit4)
plot(cv.fit4)
plot(cv.fit4$lambda)

pred.fit4 <- predict(fit4, test.modelmatrix, type="response", s=cv.fit4$lambda.min)
pred.val4 = rep(0, length(pred.fit4))
pred.val4[pred.fit4 > 0.5] = 1
CM4 = table(pred = pred.val4, true=test$Churn)

accuracy4 = (CM4[1]+CM4[4])/(CM4[1]+CM4[2]+CM4[3]+CM4[4])
accuracy4 # 0.6451548

precision4 = CM4[4]/(CM4[2]+CM4[4])
precision4 # 0.3826152

sensitivity4 = CM4[4]/(CM4[3]+CM4[4])
sensitivity4 # 0.3527397

specificity4 = CM4[1]/(CM4[1]+CM4[2])
specificity4 # 0.7655853

F1.4 = 2*(precision4*sensitivity4)/(precision4+sensitivity4)
F1.4 # 0.3670706

ROC4 <- evalmod(scores = pred.val4, labels = test$Churn)
auc(ROC4) # 0.5591625 ROC
autoplot(ROC4)
  
# 5 - Logistic with Lasso (alpha=1)
fit5= glmnet(train.modelmatrix, y_train, alpha=1, family = "binomial", lambda = lambda.grid)
cv.fit5= cv.glmnet(train.modelmatrix, y_train, alpha=1, family = "binomial", lambda = lambda.grid)
plot(fit5)
plot(cv.fit5)
plot(cv.fit5$lambda)

pred.fit5 <- predict(fit5, test.modelmatrix, type="response", s=cv.fit5$lambda.min)
pred.val5 = rep(0, length(pred.fit5))
pred.val5[pred.fit5 > 0.5] = 1
CM5 = table(pred=pred.val5, true=test$Churn)

accuracy5 = (CM5[1]+CM5[4])/(CM5[1]+CM5[2]+CM5[3]+CM5[4])
accuracy5 # 0.6577423

precision5 = CM5[4]/(CM5[2]+CM5[4])
precision5 # 0.3726083

sensitivity5 = CM5[4]/(CM5[3]+CM5[4])
sensitivity5 # 0.2534247

specificity5 = CM5[1]/(CM5[1]+CM5[2])
specificity5 # 0.8242595

F1.5 = 2*(precision5*sensitivity5)/(precision5+sensitivity5)
F1.5 # 0.3016714

ROC5 <- evalmod(scores = pred.val5, labels = test$Churn)
auc(ROC5) # 0.5388421 ROC
autoplot(ROC5)

# 6 - Decision Tree
fit6 = rpart(Churn~., data = train_balanced, method = 'class')
rpart.plot(fit6, extra = 106) # plots the decision tree

pred.fit6 = predict(fit6, test, type = 'class')
CM6 = table(pred = pred.fit6, true=test$Churn)

accuracy6 = (CM6[1]+CM6[4])/(CM6[1]+CM6[2]+CM6[3]+CM6[4])
accuracy6 # 0.6605395

precision6 = CM6[4]/(CM6[2]+CM6[4])
precision6 # 0.3369714

sensitivity6 = CM6[4]/(CM6[3]+CM6[4])
sensitivity6 # 0.1691781

specificity6 = CM6[1]/(CM6[1]+CM6[2])
specificity6 # 0.8629055

F1.6 = 2*(precision6*sensitivity6)/(precision6+sensitivity6)
F1.6 # 0.2252622

ROC6 <- evalmod(scores = as.numeric(pred.fit6), labels = test$Churn)
auc(ROC6) # 0.5160418 ROC
autoplot(ROC6)

# 7 - Random Forest
set.seed(234)
fit7 = randomForest(Churn ~ ., data=train_balanced) # computationally expensive
pred.fit7 = predict(fit7, newdata=test, type="response")
CM7 = table(pred = pred.fit7, true=test$Churn)

accuracy7 = (CM7[1]+CM7[4])/(CM7[1]+CM7[2]+CM7[3]+CM7[4])
accuracy7 # 0.7088911

precision7 = CM7[4]/(CM7[2]+CM7[4])
precision7 # 0.5055762

sensitivity7 = CM7[4]/(CM7[3]+CM7[4])
sensitivity7 # 0.09315068

specificity7 = CM7[1]/(CM7[1]+CM7[2])
specificity7 # 0.9624824

F1.7 = 2*(precision7*sensitivity7)/(precision7+sensitivity7)
F1.7 # 0.1573164

ROC7 <- evalmod(scores = as.numeric(pred.fit7), labels = test$Churn)
auc(ROC7) # 0.5278165 ROC
autoplot(ROC7)

##### COMPARING CHARACTERISTICS #####

test1 = select(test, c(2,3,4,5,9,10,11,12,16,18,20,21,22,23,24,35,36,37,39,49))
test1$PredChurn = pred.val1
summary(test1)

test2 = subset(test1, PredChurn == "1")
summary(test2)

vis_cor(test1)

##### FIT ONTO EVALUATION DATASET #####

# Change to numeric variables
eval_data2 = eval_imputed #backup

eval_data2$HandsetRefurbished <- ifelse(eval_data2$HandsetRefurbished=="Yes", 1, 0)
eval_data2$HandsetRefurbished = as.numeric(eval_data2$HandsetRefurbished)

eval_data2$HandsetWebCapable <- ifelse(eval_data2$HandsetWebCapable=="Yes", 1, 0)
eval_data2$HandsetWebCapable = as.numeric(eval_data2$HandsetWebCapable)

eval_data2$TruckOwner <- ifelse(eval_data2$TruckOwner=="Yes", 1, 0)
eval_data2$TruckOwner = as.numeric(eval_data2$TruckOwner)

eval_data2$RVOwner <- ifelse(eval_data2$RVOwner=="Yes", 1, 0)
eval_data2$RVOwner = as.numeric(eval_data2$RVOwner)

eval_data2$BuysViaMailOrder <- ifelse(eval_data2$BuysViaMailOrder=="Yes", 1, 0)
eval_data2$BuysViaMailOrder = as.numeric(eval_data2$BuysViaMailOrder)

eval_data2$RespondsToMailOffers <- ifelse(eval_data2$RespondsToMailOffers=="Yes", 1, 0)
eval_data2$RespondsToMailOffers = as.numeric(eval_data2$RespondsToMailOffers)

eval_data2$OptOutMailings <- ifelse(eval_data2$OptOutMailings=="Yes", 1, 0)
eval_data2$OptOutMailings = as.numeric(eval_data2$OptOutMailings)

eval_data2$OwnsComputer <- ifelse(eval_data2$OwnsComputer=="Yes", 1, 0)
eval_data2$OwnsComputer = as.numeric(eval_data2$OwnsComputer)

eval_data2$HasCreditCard <- ifelse(eval_data2$HasCreditCard=="Yes", 1, 0)
eval_data2$HasCreditCard = as.numeric(eval_data2$HasCreditCard)

eval_data2$NewCellphoneUser <- ifelse(eval_data2$NewCellphoneUser=="Yes", 1, 0)
eval_data2$NewCellphoneUser = as.numeric(eval_data2$NewCellphoneUser)

eval_data2$OwnsMotorcycle <- ifelse(eval_data2$OwnsMotorcycle=="Yes", 1, 0)
eval_data2$OwnsMotorcycle = as.numeric(eval_data2$OwnsMotorcycle)

eval_data2$MadeCallToRetentionTeam <- ifelse(eval_data2$MadeCallToRetentionTeam=="Yes", 1, 0)
eval_data2$MadeCallToRetentionTeam = as.numeric(eval_data2$MadeCallToRetentionTeam)

eval_data2 = select(eval_data2, -c("CreditRatingDesc"))
summary(eval_data2)

# balance the final training dataset
summary(as.factor(training_data4$Churn)) # 17673 No, 7355 Yes
training_data4.balanced = SMOTE(training_data4, training_data4$Churn, K = 4) # using SMOTE to balance the data
table(training_data4.balanced$data$Churn) # 17673 No and 14710 Yes - much more balanced
training_final = training_data4.balanced$data
training_final = select(training_final, -c("class"))

# fitting the logistic model
set.seed(12345)
eval_data3 = eval_data2
final.fit = glm(Churn ~ ., family = "binomial", data = training_final)
pred.churn <- predict(final.fit, eval_data3, type = "response")
predval.churn = rep(0, length(pred.churn))
predval.churn[pred.churn > 0.5] = 1
summary(as.factor(predval.churn)) # 3009 who churned

eval_data3$Churn = as.factor(predval.churn) # add churn column
eval_data3$CustomerID = CustID # add Customer ID column back

# finding the final 3000
eval_data3 <- eval_data3 %>%
  select(CustomerID, Churn, everything()) # put CustomerID and Churn at front

eval_final = eval_data3$CustomerID[eval_data3$Churn == "1"] # extracting customers churned
eval_final = data.frame(eval_final)
CustomerID = eval_final[c(1:3000),] # taking the first 3000 observations
CustomerID <- data.frame(CustomerID)
View(CustomerID)
dim(CustomerID) # cut to 3000

write.csv(CustomerID, "z5164443 churned customers.csv", row.names = FALSE) # writing final CSV

