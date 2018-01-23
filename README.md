# Travelers
#Update the location of Train and Test data sets

#Installing packages and including libraries
install.packages('zipcode')
library(zipcode)
install.packages("dummies")
library(dummies)
install.packages("caTools")
library(caTools)
install.packages("ROCR")
library(ROCR)
install.packages("AUC")
library(AUC)

#Reading train data set
Train <- read.csv("E:/Travellers Case Competetion/Train.csv")     #Import Train dataset --> Give the path for train data set
Test <- read.csv("E:/Travellers Case Competetion/Test.csv")       #Import Test dataset ---> Give the path for test data set

Full_Dataset=Train
colnames(Full_Dataset)

#Functions to clean data, develop features, recode column types

#Impute missing values --> Create function

Impute_Values <- function(dataset){
  
  ##Age --> Replace with mean
  dataset$ni.age[!is.na(dataset$ni.age)&dataset$ni.age>100] = NA 
  dataset$ni.age[is.na(dataset$ni.age)] = mean(dataset$ni.age,na.rm = TRUE)
  
  #Tenure with mean
  dataset$tenure[is.na(dataset$tenure)] = round(mean(dataset$tenure,na.rm = TRUE),0)
  
  #Len at Res --> Replace with mean
  dataset$len.at.res[is.na(dataset$len.at.res)] = round(mean(dataset$len.at.res,na.rm = TRUE),0)
  
  #if Tenure/len.at.res > age replace with age
  dataset$len.at.res[dataset$len.at.res>dataset$ni.age] = dataset$ni.age[dataset$len.at.res>dataset$ni.age]
  dataset$tenure[dataset$tenure>dataset$ni.age] = dataset$ni.age[dataset$tenure>dataset$ni.age]
  
  #n.adults --> replace with median value
  dataset$n.adults[is.na(dataset$n.adults)] = round(median(dataset$n.adults,na.rm = TRUE),0)
  
  #n.children --> median
  dataset$n.children[is.na(dataset$n.children)] = round(median(dataset$n.children,na.rm = TRUE),0)
  
  #Premium --- > replace with mean
  dataset$premium[is.na(dataset$premium)] = (mean(dataset$premium,na.rm = TRUE))
  
  #Replace all categorical values with mode
  
  dataset$sales.channel[is.na(dataset$sales.channel)] = 'Broker'
  
  dataset$ni.marital.status[is.na(dataset$ni.marital.status)] = 1
  
  dataset$coverage.type[is.na(dataset$coverage.type)] = 'C'
  
  dataset$dwelling.type[is.na(dataset$dwelling.type)] = 'House'
  
  dataset$ni.gender[is.na(dataset$ni.gender)] = 'M'
  
  dataset$credit[is.na(dataset$credit)] = 'high'
  
  dataset$claim.ind[is.na(dataset$claim.ind)] = 0
  
  dataset$house.color[is.na(dataset$house.color)] = 'white'
  
  #Zipcode replace missing values with mode
  zip_freq_table = as.data.frame(table(dataset$zip.code))
  dataset$zip.code[is.na(dataset$zip.code)] = zip_freq_table$Var1[which.max(zip_freq_table$Freq)]
  
  dataset$Depandants = dataset$n.adults+dataset$n.children  #Calculate depandants on policy holder
  
  #Financial Burden is premium * depandants ---> Measuring financial burden on the policy holder based on policy
  #premium and the number of people residing in the property
  dataset$Financial_Burden = dataset$premium*dataset$Depandants 
  dataset$Cust_since = round(dataset$year - dataset$tenure,0) #Cust_Since is year - tenure
  dataset$Cust_since = as.factor(dataset$Cust_since)  #Convert customer since to factors
  
  return(dataset)
}

####################################################################
####################################################################


#Function to append Z-scores for premium, len.at.res, tenure to the dataset

Features <- function(dataset){
  dataset$premium_z_score = (dataset$premium - mean(dataset$premium))/sd(dataset$premium)
  dataset$len.at.res_z_score = (dataset$len.at.res - mean(dataset$len.at.res))/sd(dataset$len.at.res)
  dataset$tenure_z_score = (dataset$tenure - mean(dataset$tenure))/sd(dataset$tenure)
  return(dataset)
}

##################################################################
##################################################################

#Recode column types

Recode_type <- function(dataset){
  dataset$claim.ind = as.factor(dataset$claim.ind)
  dataset$ni.marital.status = as.factor(dataset$ni.marital.status)
  dataset$zip.code = as.factor(dataset$zip.code)
  return(dataset)
}

#################################################################
####################################################################

#Remove Cancel = -1
Full_Dataset = Full_Dataset[Full_Dataset$cancel!=-1,]
Full_Dataset$cancel = as.factor(Full_Dataset$cancel)

#Preprocess the data by calling functions created above
Full_Dataset = Impute_Values(Full_Dataset)
colnames(Full_Dataset)
Full_Dataset = Recode_type(Full_Dataset)
summary(Full_Dataset)

#Use zipcode package to add states and cities

data("zipcode")
zipcode_req = data.frame(zipcode$zip,zipcode$city,zipcode$state)
colnames(zipcode_req) = c('zip.code','city','state')
Full_Dataset = merge(Full_Dataset,zipcode_req)
Full_Dataset = Full_Dataset[-c(1)]  #Remove Zipcode

#Covert categorical variables to dummies

colnames(Full_Dataset)
df_dum = dummy.data.frame(Full_Dataset[c(3,6,7,9,10,11,13,14,21,22)])
Full_Dataset_Dummies = data.frame(Full_Dataset[-c(3,6,7,9,10,11,13,14,22,21,1,16,20)],df_dum)
colnames(Full_Dataset_Dummies)

####################################################################
####################################################################

#Split the dataset as train and validation


set.seed(564)

split = sample.split(Full_Dataset_Dummies$cancel, SplitRatio = 0.7)
training_set_dummies = subset(Full_Dataset_Dummies, split == TRUE)
validation_set_dummies = subset(Full_Dataset_Dummies, split == FALSE)

#Add features to training and validation

training_set_dummies = Features(training_set_dummies)
validation_set_dummies = Features(validation_set_dummies)

colnames(training_set_dummies)
#training_set_dummies_x = training_set_dummies 

#Pick few significant variables
training_set_dummies_x = training_set_dummies[c(1,2,3,4,5,6,7,9,10:31,156:164)]

####################################################################
####################################################################

#Run a backward selection logistic model

#============================ Backward selection ===============================
attach(training_set_dummies_x)
logistic_model_tuning = glm(cancel ~ ., data = training_set_dummies_x, family = binomial,maxit=200)
logistic_tuned_summ = (summary(logistic_model_tuning))
coeff_df = data.frame(logistic_tuned_summ$coefficients)
dum = row.names.data.frame(coeff_df[!is.na(coeff_df$Pr...z..),])

for(i in 1:1000 ) {
  logistic_reg_tuned = glm(cancel~., data = training_set_dummies_x[c(dum[-1],"cancel")],family = binomial,maxit = 200)
  logistic_tuned_summ = (summary(logistic_reg_tuned))
  logistic_tuned_summ
  
  coeff_df = data.frame(logistic_tuned_summ$coefficients)
  if (max(coeff_df$Pr...z..) <= 0.05) 
    {
    break
  }
  dum = row.names.data.frame(coeff_df[coeff_df$Pr...z.. < max(coeff_df$Pr...z..),])
}
# ============================ End Backward Selection 

####################################################################
####################################################################

# predict training and validation
predict_train_glm = predict(logistic_reg_tuned, newdata = training_set_dummies_x,type = 'response')

predict_validation_glm = predict(logistic_reg_tuned, newdata = validation_set_dummies,type = 'response')

####################################################################
####################################################################

#Check AUC
#training
pred_input_training = prediction(predict_train_glm,training_set_dummies_x$cancel)
perf_training = performance(pred_input_training,"auc")
print(perf_training@y.values)         #0.7162924

#test
pred_input_validation = prediction(predict_validation_glm,validation_set_dummies$cancel)
perf_validation = performance(pred_input_validation,"auc")
print(perf_validation@y.values)       #0.7272382

#######################################################################
#######################################################################

travellers_test=Test


#Run Impute_Values, Recode_Type functions to clean the test dataset
travellers_test = Impute_Values(travellers_test)
colnames(travellers_test)
travellers_test = Recode_type(travellers_test)
summary(travellers_test)

#Use zipcode package to add states and cities
install.packages('zipcode')
library(zipcode)
data("zipcode")
zipcode_req = data.frame(zipcode$zip,zipcode$city,zipcode$state)
colnames(zipcode_req) = c('zip.code','city','state')
travellers_test = merge(travellers_test,zipcode_req)
travellers_test = travellers_test[-c(1)]  #Remove Zipcode

#Covert categorical variables to dummies
install.packages("dummies")
library(dummies)
colnames(travellers_test)
df_dum = dummy.data.frame(travellers_test[c(3,6,7,9,10,11,13,14,21)])
travellers_test_Dummies = data.frame(travellers_test[-c(3,6,7,9,10,11,13,14,21)],df_dum)
colnames(travellers_test_Dummies)

#Predict probabilities on test data set
predictions_testSet = predict(logistic_reg_tuned,newdata = travellers_test_Dummies,type = 'response')

#Write predicted values to data frame
travellers_test_final_predictions = data.frame(travellers_test_Dummies,predictions_testSet)

#Write data frame to a file
write.csv(travellers_test_final_predictions,'E:/Travellers Case Competetion/Test_Predictions_FinalV1.0.csv')


####################################################################
####################################################################

#####################################################################################################################
#Below C5 model is also robust and giving Training AUC 0.71 and Validation AUC 0.72                                 #  
#Logistic is also giving similar results                                                                            #
#Ensemble of Logistic and C5 is also giving same AUC for multiple runs                                              #
#As Logistic is easier to implement we choose logistic as our final model and predicted values using Logistic Model #
#####################################################################################################################
# install.packages("C50")
# library(C50)
# c_model = C5.0(cancel~., data = training_set_dummies_x[c("cancel",dum[-1])], trials = 40)
# # print(c_model)
# summary(c_model)
# 
# C5imp(c_model)
# C5imp(c_model,metric = 'splits')
# imp_c5 = row.names(C5imp(c_model))
# 
# # predictions on training set
# pred_c = predict(c_model, newdata = training_set_dummies_x )
# pred_c_prob = predict(c_model, newdata = training_set_dummies_x, type = "prob" )
# act_vs_pred_c = data.frame(act = training_set_dummies_x$cancel, pred = pred_c)
# 
# # training
# library(ROCR)
# pred_input <- prediction(pred_c_prob[,2],training_set_dummies_x$cancel)
# AUC <- performance(pred_input,"auc")
# print(AUC@y.values)
# 
# # predictions on validation set
# pred_c = predict(c_model, newdata = validation_set_dummies )
# pred_c_prob = predict(c_model, newdata = validation_set_dummies, type = "prob" )
# act_vs_pred_c = data.frame(act = validation_set_dummies$cancel, pred = pred_c)
# 
# #predictions
# library(ROCR)
# pred_input <- prediction(pred_c_prob[,2],validation_set_dummies$cancel)
# AUC <- performance(pred_input,"auc")
# print(AUC@y.values)
# 
# write.csv(data.frame(validation_set_dummies,predict_validation_glm),'E:/Travellers Case Competetion/val_logistic.csv')
# write.csv(data.frame(validation_set_dummies,pred_c_prob[,2]),'E:/Travellers Case Competetion/Val_C5.csv')
# 
# Average = (predict_validation_glm+pred_c_prob[,2])/2
# 
# pred_input <- prediction(Average,validation_set_dummies$cancel)
# AUC <- performance(pred_input,"auc")
# print(AUC@y.values)
