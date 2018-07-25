setwd("C:/Users/Rishabh/Desktop/Edwisor-new/Project-1")
library(readxl)
df = read_excel("Absenteeism_at_work_Project.xls")

df = as.data.frame(df)
df = df[order(df$ID,df$`Reason for absence`),]

#Load Libraries
x = c("ggplot2", "corrgram", "DMwR", "caret", "randomForest", "unbalanced", "C50", "dummies", "e1071", "Information",
      "MASS", "rpart", "gbm", "ROSE", 'sampling', 'DataCombine', 'inTrees')

lapply(x,install.packages(x))
#install.packages(x)
lapply(x, require, character.only = TRUE)
rm(x)

###########################EXPLORATORY DATA ANALYSIS#####################################
for (i in c(2:5,12:17)){
  df[,i] = as.factor(df[,i])
} 

##################################Missing Values Analysis###############################################
missing_val = data.frame(apply(df,2,function(x){sum(is.na(x))}))
missing_val$Columns = row.names(missing_val)
names(missing_val)[1] = "Missing_Percentage"
missing_val$Missing_Percentage = (missing_val$Missing_Percentage/nrow(df)) * 100
missing_val = missing_val[order(-missing_val$Missing_Percentage),]
row.names(missing_val) = NULL
missing_val = missing_val[,c(2,1)]
write.csv(missing_val, "Missing_perc.csv", row.names = F)


#df[4,6] 
#Actual value = 235
#Mean Method = 221.0164
#Median Method = 225
#KNN Method = 235

#Mean Method
#df$`Transportation expense`[is.na(df$`Transportation expense`)] = mean(df$`Transportation expense`, na.rm =T)
#Median Method
#df$`Transportation expense`[is.na(df$`Transportation expense`)] = median(df$`Transportation expense`, na.rm =T)

#kNN Imputation
df = knnImputation(df, k=3)
sum(is.na(df))

############################################Outlier Analysis#############################################
# ## BoxPlots - Distribution and Outlier Check
numeric_index = sapply(df,is.numeric) #selecting only numeric
numeric_data = df[,numeric_index]

specific_data = df[,c(10,11,21)]
cnames = colnames(specific_data)  

#There are 3 numeric variables.
#So, doing univariate analysis for outlier detection and then imputation
outlier_values = boxplot.stats(df$`Hit target`)$out  
boxplot(df$`Hit target`, main="Hit Target", boxwex=0.1)
mtext(paste("Outliers: ", paste(outlier_values, collapse=", ")), cex=0.6)


outlier_values1 = boxplot.stats(df$`Work load Average/day`)$out  
boxplot(df$`Work load Average/day`, main="Work load Average/day", boxwex=0.1)
mtext(paste("Outliers: ", paste(outlier_values1, collapse=", ")), cex=0.6)

outlier_values2 = boxplot.stats(df$`Absenteeism time in hours`)$out  
boxplot(df$`Absenteeism time in hours`, main="Absenteeism time in hours", boxwex=0.1)
mtext(paste("Outliers: ", paste(outlier_values2, collapse=", ")), cex=0.6)

val = df$`Hit target`[df$`Hit target` %in% boxplot.stats(df$`Hit target`)$out]

for(i in cnames)
{
  val = df[,i][df[,i] %in% boxplot.stats(df[,i])$out]
  #print((val))
  df[,i][df[,i] %in% val] = NA
}

df= knnImputation(df, k = 3)

for(i in cnames){
  df[,i] = round(df[,i])
}

##################################Feature Selection################################################
## Correlation Plot 
corrgram(df[,numeric_index], order = F,
         upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")

## Chi-squared Test of Independence
factor_index = sapply(df,is.factor)
factor_data = df[,factor_index] 

for (i in 1:10)
{
 print(names(factor_data)[i])
print(chisq.test(table(factor_data$`Reason for absence`,factor_data[,i])))
}

df1 = df[,-20]
numeric_index1 = sapply(df1,(is.numeric))
numeric_data1 = df1[,numeric_index1]
cnames1 = colnames(numeric_data1)
##################################Feature Scaling################################################
#Normality check
qqnorm(df1$`Work load Average/day`)
hist(df1$`Work load Average/day`)

numeric_data2 = numeric_data1[,-1]
cnames2 = colnames(numeric_data2)
for(i in cnames2){
  #print(i)
  df1[,i] = (df1[,i] - min(df1[,i]))/
    (max(df1[,i] - min(df1[,i])))
}

write.csv(df1,"polished_data.csv",row.names =T)
# #Standardisation
# for(i in cnames){
#   print(i)
#   marketing_train[,i] = (marketing_train[,i] - mean(marketing_train[,i]))/
#                                  sd(marketing_train[,i])
# } 

#############################################Sampling#############################################
##Stratified Sampling
#ID_0.8length = c(18,4,90,1,15,6,4,1,6,19,32,5,12,23,29,1,16,12,2,33,2,36,6,24,8,4,5,60,4,5,2,4,19,44,0,27)
# stratas = strata(df,"ID", size = ID_0.8length, method = "srswor")
#stratified_data = getdata(df, stratas)

###################################Model Development#######################################
#########DECISION TREE MODEL######################################################
#Clean the environment
rmExcept("df1")

library(rpart)
library(MASS)

#Divide data into train and test using stratified sampling method
train.index = createDataPartition(df1$ID, p = 0.8, list = FALSE) 
train = df1[ train.index,]
test  = df1[-train.index,]

##rpart for regression
fit = rpart(`Absenteeism time in hours` ~ . , data = train , method = "anova")

#Predict for new test cases
predictions_DT = predict(fit, test[,-20])

summary(predictions_DT)

#Calculate MAPE
MAPE = function(y, yhat){
  mean(abs((y - yhat)/y))
}

#MAPE(test[,20],predictions_DT)

#Alternate Method
regr.eval(test[,20], predictions_DT, stats = c("mae","rmse","mape"))
#Error Rate = 12.9%
#Accuracy = 87.1%

####################################LINEAR REGRESSION####################################
library(usdm)
vif(df1[,-20])
vifcor(df1[,-20], th = 0.9)

df2 = df1[,-c(8,9)] #Removing multicollinearity by removing variables with VIF > 9 
train.index1 = createDataPartition(df2$ID, p = 0.8, list = FALSE) 
train1 = df2[ train.index,]
test1  = df2[-train.index,]

#run regression model
lm_model = lm(`Absenteeism time in hours` ~., data = train1)

#Summary of the model
summary(lm_model)

#Predict
predictions_LR = predict(lm_model, test1[,1:18])

#Calculate MAPE
MAPE(test[,10], predictions_LR)

regr.eval(test[,20], predictions_LR, stats = c("mae","rmse","mape"))
#Error Rate = 12.06%
#Accuracy = 88.94%

###############################Predicting for future using decision tree model##########
fit_future = rpart(`Absenteeism time in hours` ~ . , data = df1 , method = "anova")
predictions_DT_future = predict(fit_future,df1[,-20])
df3 = cbind(df1[,-20],predictions_DT_future)

####Sorrting according to month for second question#######3
View(df3)
df3 = df3[order(df3$`Month of absence`),]
write.csv(df3, "future.csv", row.names =T)
