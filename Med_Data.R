#****************************************************************
#* Q1.  1. Perform detailed descriptive statistics on columns V3, V4 and V6
#*      2. Write at least 3 important insights from the descriptive statistics of the three columns
#*

#****************************************************************
#* Q2. 1.  Replace the missing values of columns V3, V4, V6 with their mean values.
#*     2.  Delete the entire record if there exists even a single missing value in any other column
#*     3.  Compute the frequencies of gender and the treatment columns after this exercise
#*
#****************************************************************
#* Q3. Split the data into training set (80% of the observations). Perform
#*     1. Box-Cox transformation
#*     2. Centering pre-processing operations on Columns V1 to V8 on training set 
#*        in the same order and apply same preprocessing to the testing set
#*     3.  Produce Descriptive statistics of the testing dataset
#*
#*
#****************************************************************
#* Q4. Build 
#*     1. a CART algorithm based Decision Tree
#*     2. Gradient Boosting models to the training data by taking TREATMENT as
#*        target variable and all other variable as predictior variables
#*     3. Use 5-fold cross validation to tune the parameters
#*     4. Produce a summary of both the fitted models
#*
#*
#****************************************************************
# Q5. Test the performance of the model oon testing data by computing:
#     1. Confusion Matrix incl. accuracy, Sensitivity, Kappa Score and Balance accuracy
#     2. Area under ROC Curve and detailed interpretation of each of the output and overall predicatabily 
#        of the models
#     3. Provide 3 important suggestions on how to further improve predictability of this model

#****************************************************************
#*
#*Loading the dataset:


data<-read.csv("C:/R/PA/Med_Data.csv",stringsAsFactors = TRUE)

#****************************************************************
#* Q1.  1. Perform detailed descriptive statistics on columns V3, V4 and V6

#Loading the necessary Libraries
library(DescTools)
library(Amelia)
library(corrplot)
library(caret)
library(ISLR)
library(SmartEDA)

names(data)   # Returns Column names in the database
head(data)    # Returns First six rows in the table
tail(data)    # Returns Last six rows in the table

# ExpData :This function provides overall and variable level data summary like percentage of 
# missings, variable types etc. # Type 1 is overall data summary; Type 2 is variable level summary
# Overall data summary
ExpData(data=data,type=1)
# Variable level data summary
ExpData(data=data,type=2)

#*******************************************************************
# Some descriptive Statistics
summary(data)
Desc(data)


#***********************************************************************
#* Skewness measures the amount of skew or asymetry of a distribution
#* Kurtosis measures the peakedness of a distribution
#* Skewness and kurtosis take the idea of variance a step further:
#* While variance involves squaring the deviations from the mean, 
#* Skewness involves cubing deviations from the mean and 
#* Kurtosis involves raising deviations from the mean to the 4th power
#* 
#* Interpretation: Skewness is a statistical numerical method to measure the asymmetry of 
#* the distribution or data set. It tells about the position of the majority of data values
#* in the distribution around the mean value.  There exist 3 types of skewness values on 
#* the basis of which asymmetry of the graph is decided. These are as follows:

#* Positive Skew
#* If the coefficient of skewness is greater than 0, then the graph is said to be positively skewed
#* with the majority of data values less than mean. Most of the values are concentrated on the 
#* left side of the graph.

#* Zero Skewness or Symmetric
#* If the coefficient of skewness is equal to 0 or approximately close to 0, then the graph 
#* is said to be symmetric and data is normally distributed.

#*  Negatively skewed
#*  If the coefficient of skewness is less than 0 then the graph is said to be negatively skewed
#*  with the majority of data values greater than mean. Most of the values are concentrated 
#*  on the right side of the graph.

#***********************************************************************
#* INterpretation: Kurtosis is a numerical method in statistics that measures the sharpness 
#* of the peak in the data distribution.  There exist 3 types of Kurtosis values on the 
#* basis of which sharpness of the peak is measured. These are as follows:

#* Platykurtic
#* If the coefficient of kurtosis is less than 3, then the data distribution is platykurtic. 
#* Being platykurtic doesn’t mean that the graph is flat-topped.
 
#* Mesorkurtic
#* If the coefficient of kurtosis is equal to 3 or approximately close to 3, then the data 
#* distribution is mesokurtic. For normal distribution, kurtosis value is approximately equal to 3.

#* Leptokurtic
#* If the coefficient of kurtosis is greater than 3, then the data distribution is
#* leptokurtic and shows a sharp peak on the graph.
#* 
#* *************************************************************************

dim(data)
#* returns the number of observations (rows) and no. of variables (columns)
#* 
#************************************************************************************
#*
#****************************************************************
#* Q2. 1.  Replace the missing values of columns V3, V4, V6 with their mean values.
#*     2.  Delete the entire record if there exists even a single missing value in any other column
#*     3.  Compute the frequencies of gender and the treatment columns after this exercise


#****************************************************************
#* Q2. 1.  Replace the missing values of columns V3, V4, V6 with their mean values.
#identify missing values
any(is.na(data))

# Using complete.cases() function to get percentage of missing value
nrow(data[!complete.cases(data), ])/nrow(data)*100

#identify which variables and what percentage of observations from each variable are missing
# Looking at the missing data pattern 

## install.packages("mice")

library(mice)
md.pattern(data)

library(Hmisc)

#* Replace the missing values of columns V3, V4, V6 with their mean values
data$V3<-impute(data$V3, mean)  # replace NA in V3 with mean
data$V4<-impute(data$V4, mean)  # replace NA in V4 with mean
data$V6<-impute(data$V6, mean)  # replace NA in V6 with mean

#* Display data after replacing NA values in V3,V4 and V6 with mean
md.pattern(data)

summary(data)


#****************************************************************
#* Q2. 
#*     2.  Delete the entire record if there exists even a single missing value in any other column
#*     


data$SEX<-as.numeric(data$SEX)
summary(data)

ExpData(data=data,type=1)
ExpData(data=data,type=2)

ExpNumStat(data,by="A",gp=NULL,Qnt=seq(0,1,0.1),MesofShape=2,Outlier=TRUE,round=2,Nlim=10)
ExpCTable(data,Target=NULL,margin=1,clim=10,nlim=3,round=2,bin=NULL,per=T)
         
#HOW TO DELETE MISSING OBSERVATIONS
data1 <- data[complete.cases(data), ]
# or we can use na.omit() function
data1 <- na.omit(data)

Glimpse(data)
#* Display data after deleting rows with NA values in the table
md.pattern(data1)

summary(data1)

#****************************************************************
#* Q2. 
#*     3.  Compute the frequencies of gender and the treatment columns after this exercise
#*     

library(plyr)
count(data1, 'SEX')
count(data1, 'Treatment')



#****************************************************************
#*
#* Q3. Split the data into training set (80% of the observations). Perform
#*     1. Box-Cox transformation
#*     2. Centering pre-processing operations on Columns V1 to V8 on training set 
#*        in the same order and apply same preprocessing to the testing set
#*     3.  Produce Descriptive statistics of the testing dataset
#*     
#*     
#*     Split the data into training set (80% of the observations).


index = createDataPartition(y=data1$Treatment, p=0.8, list=FALSE)

train.set = data1[index,]
test.set = data1[-index,]

summary(train.set)
summary(test.set)


#* Q3. Split the data into training set (80% of the observations). 
#* Perform Box-Cox transformation


library(caret)
# summarize data
summary(train.set)
# calculate the pre-process parameters from the dataset
ExpData(data=train.set,type=2)
train_preprocessParams <- preProcess(train.set[,c(1,2,3,4,5,6,7,8)], method=c("BoxCox","center"))

# summarize transform parameters
print(train_preprocessParams)

#*  apply same preprocessing to the testing set
#*  

summary(test.set)
# calculate the pre-process parameters from the dataset
ExpData(data=test.set,type=2)
test_preprocessParams <- preProcess(test.set[,c(1,2,3,4,5,6,7,8)], method=c("BoxCox","center"))

# summarize transform parameters
print(test_preprocessParams)


#****************************************************************
  
  
#* Q3. 
#*     3.  Produce Descriptive statistics of the testing dataset
#*     For Training data
ExpData(data=train.set,type=1)
# Variable level data summary
ExpData(data=train.set,type=2)

#*******************************************************************
# Some descriptive Statistics
summary(train.set)
Desc(train.set)

#*     For Test data
ExpData(data=test.set,type=1)
# Variable level data summary
ExpData(data=test.set,type=2)

#*******************************************************************
# Some descriptive Statistics
summary(test.set)
Desc(test.set)

summary(train.set)
summary(test.set)