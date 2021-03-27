library(DescTools)
library(corrplot)
library(caret)
library(ISLR)
library(SmartEDA)

#Loading the file

data<-read.csv("C:/R/PA/BAZG512_EC2M_Data.csv",stringsAsFactors = TRUE)


#************************************************************************
#* Q1  Descriptive Statistics on MetaScore, no of votes and Gross columns
#* 
names(data)  # Returns Column names in the database

ExpData(data=data,type=1)  # Overall data summary
ExpData(data=data,type=2)  # Variable level data summary

# Some descriptive Statistics
summary(data)
Desc(data)
dim(data)

ExpNumStat(data,by="A",gp=NULL,Qnt=seq(0,1,0.1),MesofShape=2,Outlier=TRUE,round=2,Nlim=10)
ExpCTable(data,Target=NULL,margin=1,clim=10,nlim=3,round=2,bin=NULL,per=T)

# distribution of class variable
y <- data$IMDB_Above8
cbind(freq=table(y), percentage=prop.table(table(y))*100)

#Single variable Visualizations
# create histograms for each attribute
cat_var<-sapply(data,FUN=is.factor)
cat_var1<-which(cat_var)
cat_var2<-which(!cat_var)
par(mfrow=c(1,3))

for(i in which(cat_var)) {
  counts<-table(data[,i])
  name<-names(data)[i]
  barplot(counts, main=name)
}

# Multivariate visualization

correlations <- round(cor(data[,!cat_var]),3)

# Create Correlation plot
corrplot(correlations, method="circle")
pairs(data[,!cat_var])
pairs(IMDB_Above8~.,data = data[,c(24,cat_var2)],col= data$IMDB_Above8)

#* Four major observations from Descriptive statistics of three columns MetaScore, no of votes and Gross columns
#* 
#* Overall, there are 1000 observations in the data set and there are 24 variables
#* 1.  Meta_score, No_of_Votes and Gross columns are of integer type with 64, 999 and 2 unique values respectively
#* 2.  Meta_Score has an Average mean of 77.97, while Average no. of votes is 27369 
#*     and Average gross collection being 68034751 
#* 3.  There are 157 missing values in Meta_score column and 169 missing values in Gross column. 
#*     There are no missing values in No_of_Votes column
#* 4.  Skewness and Kurtosis for Meta_score is -.6 and .4 indicating that distribution of data is 
#*     negatively skewed with the some of data values greater than mean with most of the values concentrated 
#*     slightly on the right side of the graph. Kurtosis which is a  measure the sharpness 
#*     of the peak in the data distribution being less than 3 indicated that  data distribution 
#*     is platykurtic. Being platykurtic doesn’t mean that the graph is flat-topped     
#*     
#*     Similarly for no_of votes, skewness of 2.29 indicates that data is positively skewed and 
#*     kurtosis value of 6.83 indicates that data distribution is leptokurtic and shows a sharp peak 
#*     on the graph.
#*  
#*     For Gross column, skewness is 3.12 and Kutosis is very high at 13.78 indicating that data is 
#*     very skewed on rightside and there is a very sharp peak
#*     
#*5.   32.2% (322) movies gave been IMDB rated above 8 and remaining 67.8%  (678) movies have been rated below 8
#*     
#*     ***************************************************************************




#* **********************************************************************************
#* Q2. Impute the missing values of Meta Score and Gross columns with the first quartile value
#*     Compute the mean meta score and mean Gross for movies with IMDB Rating of below 8
#*     
#*     
#*     
library(mice)
md.pattern(data)

library(Hmisc)
# IMputing values in Meta_score and Gross columns with 1st quartile

data$Meta_score<-impute(data$Meta_score, summary(data$Meta_score)[2] )  # replace with quartile 1
data$Gross<-impute(data$Gross, summary(data$Gross)[2] )  # replace with quartile 1
md.pattern(data)
summary(data)

#*     Compute the mean meta score and mean Gross for movies with IMDB Rating of below 8

data2 <- data[which(data$IMDB_Above8 == "Yes"),]
data3 <- data[which(data$IMDB_Above8 == "No"),]
mean(data2$Meta_score)
# [1] 78.8913
mean(data2$Gross)
# [1] 59413876
mean(data3$Meta_score)
# [1] 75.68879
mean(data3$Gross)
# [1] 55981507

#* **********************************************************************************
#* Q3. Remove movies data for whcih no certificate is available 
#*     Create new dataset with movies belonging to any one or more of Crime or action or Biographic categotries
#*     If a movie belongs to one or more categories along with any of the other categories then also, the movie needs to 
#*     be included in the subset
#*     Use this subset and seed to 100 , split the data in training and testing sets(70:30)
#*     Compute the proportion of movies with an IMDB rating of above 8 in testing set
#*     
#*     
####### Q3 #########


library(Amelia)
library(imputeTS)
library(fBasics)


data<-read.csv("C:/R/PA/BAZG512_EC2M_Data.csv",stringsAsFactors = TRUE)

#*  Remove movies data for whcih no certificate is available
data2 <-  data[!(is.na(data$Certificate) | data$Certificate==""), ]

#* Create new dataset with movies belonging to any one or more of Crime or action or Biographic categories
#* If a movie belongs to one or more categories along with any of the other categories then also, the movie needs to 
#* be included in the subset
data3 <- data[data$Comedy == "1"| data$Mystery == "1",]

set.seed(100)  # For reproducibility
# Create index for testing and training data
Train <- createDataPartition(y = data3$IMDB_Above8, p = 0.7, list = FALSE)
# subset titanic data to training
training <- data3[Train,]
# subset the rest to test
testing <- data3[-Train,]

#proportions of each movie

finalset <- testing[which(testing$IMDB_Above8 == "Yes"),]

prop.table(finalset[,5:10])




#************************************************************************
#*Q4  Build 
#*      1. Conditional inference decision tree
#*      2. logistic regression model
#*      3.Random forest
#*      4. Gradient Boosting models to training data by taking IMDB of above 8
#*         as target variable and all other variables as predictor variables
#*         Use at least two different preprocessing tasks
#*         Test performance of modelson testing data by computing 
#*         1. comfusion matrix and area under the curve 
#*
#*
#*#Loading the file

data<-read.csv("C:/R/PA/BAZG512_EC2M_Data.csv",stringsAsFactors = TRUE)
summary(data)

# IMputing values in Meta_score and Gross columns with 1st quartile

data$Meta_score<-impute(data$Meta_score, summary(data$Meta_score)[2] )  # replace with quartile 1
data$Gross<-impute(data$Gross, summary(data$Gross)[2] )  # replace with quartile 1
md.pattern(data)
summary(data)


# calculate the pre-process parameters from the dataset on numeric columns
preprocess <- preProcess(data[,c(1,2,3,4,5,7,8,9,10,11,12,13,14,15,16,17,17,18,19,20,21,22,23,24)], method=c("scale"))
# summarize transform parameters
print(preprocess)

#Pre-processing:  - ignored (5) and - scaled (19)

# transform the dataset using the parameters
transformed <- predict(preprocess, data[,c(1,2,3,4,5,7,8,9,10,11,12,13,14,15,16,17,17,18,19,20,21,22,23,24)])
# summarize the transformed dataset
summary(transformed)  # scale expreses each observation in terms of no. of Standard deviations

