#***************************************************************
# Lecture Class 2 : Data Descriptive Statistics/Understanding | 16th Jan 2021
#***************************************************************

library(DescTools)
library(Amelia)
library(corrplot)
library(caret)
library(ISLR)
library(SmartEDA)

#*******************************************************************
#heart<-read.csv("C:/R/PA/heart_failure_clinical_records_dataset.csv",stringsAsFactors = TRUE)

data<-read.csv("C:/R/PA/heart_failure_clinical_records_dataset.csv",stringsAsFactors = TRUE)

head(data)    # Returns First six rows in the table
tail(data)    # Returns Last six rows in the table
names(data)   # Returns Column names in the database

#*******************************************************************
# ASsign Psuedoname to a column

names(data)[3]<-"CP"

#*******************************************************************
#*
#?ExpData :This function provides overall and variable level data summary like percentage of 
# missings, variable types etc.
# Type 1 is overall data summary; Type 2 is variable level summary

# Overall data summary

ExpData(data=data,type=1)

# Variable level data summary

ExpData(data=data,type=2)

#*******************************************************************
#Correct the data types of the variables

data$sex<-as.factor(data$sex)
data$smoking<-as.factor(data$smoking)
data$DEATH_EVENT<-as.factor(data$DEATH_EVENT)
data$anaemia<-as.factor(data$anaemia)
data$diabetes<-as.factor(data$diabetes)
data$high_blood_pressure<-as.factor(data$high_blood_pressure)

# Rerun ExpData command to note the change in data types of the variables
# Overall data summary
ExpData(data=data,type=1)
# Variable level data summary
ExpData(data=data,type=2)


#*******************************************************************


# Some descriptive Statistics
summary(data)
Desc(data)
dim(data)

#*******************************************
#Function provides summary statistics for all numerical variable. 
#This function automatically scans through each variable and select only numeric/integer variables. 
#Also if we know the target variable, function will generate relationship between target variable 
#and each independent variable.

ExpNumStat(data,by="A",gp=NULL,Qnt=seq(0,1,0.1),MesofShape=2,Outlier=TRUE,round=2,Nlim=10)

#Function To Create Frequency And Custom Tables
#this function will automatically select categorical variables and generate frequency or 
#cross tables based on the user inputs. Output includes counts, percentages, row total and column total

ExpCTable(data,Target=NULL,margin=1,clim=10,nlim=3,round=2,bin=NULL,per=T)

#*******************************************
# distribution of class variable
y <- data$DEATH_EVENT
cbind(freq=table(y), percentage=prop.table(table(y))*100)

#*******************************************
#Single variable Visualizations
#create histograms for each attribute

cat_var<-sapply(data,FUN=is.factor)
cat_var1<-which(cat_var)
cat_var2<-which(!cat_var)
par(mfrow=c(1,3))
for(i in which(!cat_var)) {
	hist(data[,i],breaks =30, main=names(data)[i])
	boxplot(data[,i],main=names(data)[i])
	plot(density(data[,i]))
}

for(i in which(cat_var)) {
	counts<-table(data[,i])
	name<-names(data)[i]
	barplot(counts, main=name)
}

#*******************************************
# create a missing map
par(mfrow = c(1,1))
missmap(data, col=c("black", "grey"), legend=FALSE)


#*******************************************
# Multivariate visualization

correlations <- round(cor(data[,!cat_var]),3)
# create correlation plot
corrplot(correlations, method="circle")
pairs(data[,!cat_var])
pairs(DEATH_EVENT~.,data = data[,c(12,cat_var2)],col= data$DEATH_EVENT)

prop.table(table(data$DEATH_EVENT,data$smoking),margin=1)
prop.table(table(data$DEATH_EVENT,data$smoking),margin=2)


chisq.test(data$DEATH_EVENT,data$smoking)
chisq.test(data$DEATH_EVENT,data$diabetes)
chisq.test(data$DEATH_EVENT,data$high_blood_pressure)

#*******************************************
# density plots for each attribute by class value
x <- data[,cat_var2]
y <- data[,12]
scales <- list(x=list(relation="free"), y=list(relation="free"))
featurePlot(x=x, y=y, plot="density", scales=scales)
featurePlot(x=x, y=y, plot="box",scales=scales)


#*****************************************************************
Lecture 3: Data Preparation | 23rd Jan 2021
#*****************************************************************

#IMputation
set.seed(100)
v1<-rnorm(100,5,2)
v2<-rgamma(100, shape = 3, rate = 0.2)
random_blanks<-sample(1:100,5)
v2[random_blanks]<-NA

v1
order(v1)

v2[21]<-qgamma(0.33,shape = 3, rate = .2)
v2
v2[21]<-qgamma(runif(1),shape = 3, rate = .2)
v2[21]

#*****************************************************************

#Most Common Transformations

#1.  Scaled Transformation

library(caret)

# summarize data
summary(data)

# calculate the pre-process parameters from the dataset
preprocessParams <- preProcess(data[,c(1,3,5,7,8,9)], method=c("scale"))

# summarize transform parameters
print(preprocessParams)

# transform the dataset using the parameters
transformed <- predict(preprocessParams, data[,c(1,3,5,7,8,9)])

# summarize the transformed dataset
summary(transformed)

#*******************************************

#2.  Center Transformation

preprocessParams <- preProcess(data[,c(1,3,5,7,8,9)], method=c("center"))

# summarize transform parameters
print(preprocessParams)

# transform the dataset using the parameters
transformed <- predict(preprocessParams, data[,c(1,3,5,7,8,9)])

# summarize the transformed dataset
summary(transformed)

#*******************************************

#3. Combination of Center and Scale Transformation

preprocessParams <- preProcess(data[,c(1,3,5,7,8,9)], method=c("center", "scale"))

# summarize transform parameters
print(preprocessParams)

# transform the dataset using the parameters
transformed <- predict(preprocessParams, data[,c(1,3,5,7,8,9)])

# summarize the transformed dataset
summary(transformed)

#*******************************************

#4.  Range Transformation

preprocessParams <- preProcess(data[,c(1,3,5,7,8,9)], method=c("range"))

# summarize transform parameters
print(preprocessParams)

# transform the dataset using the parameters
transformed <- predict(preprocessParams, data[,c(1,3,5,7,8,9)])

# summarize the transformed dataset
summary(transformed)

#*******************************************

#5.  Box-Cox Transformation

#install.packages("fBasics")
library(fBasics)

basicStats(data[,c(1,3,5,7,8,9)])
preprocessParams <- preProcess(data[,c(1,3,5,7,8,9)], method=c("BoxCox"))

# summarize transform parameters
print(preprocessParams)

# transform the dataset using the parameters
transformed <- predict(preprocessParams, data[,c(1,3,5,7,8,9)])

# summarize the transformed dataset
summary(transformed)
basicStats(transformed)

#*******************************************

#6.  YeoJohnson Transformation
preprocessParams <- preProcess(data[,c(1,3,5,7,8,9)], method=c("YeoJohnson"))

# summarize transform parameters
print(preprocessParams)

# transform the dataset using the parameters
transformed <- predict(preprocessParams, data[,c(1,3,5,7,8,9)])

# summarize the transformed dataset
summary(transformed)
basicStats(transformed)




#*****************************************************************
Lecture 4: Data Transformation and Outliers | 24th Jan 2021
#*****************************************************************


#7.  Principal Component Analysis (PCA) Transformation

preprocessParams <- preProcess(data[,c(1,3,5,7,8,9)], method=c("center", "scale", "pca"))

# summarize transform parameters
print(preprocessParams)

# transform the dataset using the parameters
transformed <- predict(preprocessParams, data[,c(1,3,5,7,8,9)])

# summarize the transformed dataset
summary(transformed)
prcomp(data[,c(1,3,5,7,8,9)])


options(scipen=7)
prcomp(data[,c(1,3,5,7,8,9)],scale.=TRUE)

cor(transformed)


#*******************************************

#8.  Independent Component Analysis (ICA) Transformation

#install.packages("fastICA")

preprocessParams <- preProcess(data[,c(1,3,5,7,8,9)], method=c("center", "scale", "ica","YeoJohnson"), n.comp = 4)

# summarize transform parameters
print(preprocessParams)

# transform the dataset using the parameters
transformed <- predict(preprocessParams, data[,c(1,3,5,7,8,9)])

# summarize the transformed dataset
summary(transformed)

cor(transformed)

Desc(transformed)

#************************************************************

# Injecting missing value
set.seed(100)
data[sample(1:nrow(data), 5), "age"] <- NA
data[sample(1:nrow(data), 10), "CP"] <- NA
data[sample(1:nrow(data), 8), "serum_creatinine"] <- NA
summary(data)

#*****************************************************
#identify missing values
any(is.na(data))

# Using complete.cases() function to get percentage of missing value
nrow(data[!complete.cases(data), ])/nrow(data)*100

#************************************************************
#identify which variables and what percentage of observations from each variable are missing
# Looking at the missing data pattern 

## install.packages("mice")

library(mice)
md.pattern(data)

#HOW TO DELETE MISSING OBSERVATIONS

data1 <- data[complete.cases(data), ]

# or we can use na.omit() function
data1 <- na.omit(data)

#DELETE VARIABLES WITH MISSING VALUES
## Removing columns with more than 30% NA
data[, -which(colMeans(is.na(data)) > 0.3)]
colMeans(is.na(data))
md.pattern(data)

#************************************************************

#IMPUTING MISSING VALUES
#USING MEAN/MEDIAN/MODE


data$CP<-impute(data$CP, median)  # median

#************************************************************
#MULTIVARIATE IMPUTATION BY CHAINED EQUATIONS
#missing values are  replaced by Predictive Mean Matching (PMM) or Logistic Regression or Proportional Odds Model 

install.packages('bnstruct')
library(bnstruct)
imputed_data <- mice(data, m=5, method = 'pmm', seed = 100)

# checking the summary
summary(imputed_data)

# Checking imputed values 
imputed_data$imp

#************************************************************

#USING MACHINE LEARNING ALGORITHMS
#USING KNN TO FILL THE MISSING VALUES
library(bnstruct)
data2<-knn.impute(as.matrix(data), k = 10, cat.var = c(2,4,6,10,11,12), to.impute = 1:nrow(data),using = 1:nrow(data))
summary(data2)
# Imputing values using Random forest
library(randomForest)
set.seed(100)
data3 <- rfImpute(DEATH_EVENT ~ ., data)
summary(data3)


#************************************************************

#ANALYSING IMPUTED VALUES USING GRAPHICAL METHODS
library(VIM)

# imputting values in iris dataset using knn
data_imputed <- kNN(data[, c("age", "CP", "serum_creatinine")])

# Using pbox function to draw boxplot
pbox(data_imputed, delimiter="imp", selection="any")

par(mfrow = c(1,1))


#Working with Outliers
# Building a histogram and Box Plot are shown earlier
# Detecting Outliers
out<-boxplot.stats(data3$serum_creatinine)$out
out_ind <- which(data3$serum_creatinine %in% c(out))
out_ind
data3[out_ind, ]

#Building a glm model around death event
mod <- glm(DEATH_EVENT ~ ., data = data3, family=binomial(link="logit"))

#calculating cook's distance

data3$cooksd <- cooks.distance(mod)

plot(data3$cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")
abline(h = 4*mean(data3$cooksd, na.rm=T), col="red")
influential <- data3$cooksd[(data3$cooksd > 4*mean(data3$cooksd, na.rm=T))] 
influential_rows<-which(data3$cooksd %in% influential)
head(data3[influential_rows, ])


# Defining outliers based on 4/n criteria
data3$outlier <- ifelse(data3$cooksd < 4/nrow(data3), "keep","delete")
data3$outlier<-as.factor(data3$outlier)


#Hampel Filter to identify outliers
lower_bound <- median(data3$serum_creatinine) - 3 * mad(data3$serum_creatinine)
upper_bound <- median(data3$serum_creatinine) + 3 * mad(data3$serum_creatinine)
outlier_ind <- which(data3$serum_creatinine < lower_bound | data3$serum_creatinine > upper_bound)
outlier_ind


#Statistical Tests
#Grubb's test for identification of outliers

library(outliers)
test <- grubbs.test(data3$serum_creatinine)
test
test <- grubbs.test(data3$serum_creatinine, opposite=TRUE)
test

#Rosner's test to identify the outliers
library(EnvStats)
test <- rosnerTest(data3$serum_creatinine, k = 10)
test
test$all.stats


#*****************************************************************************
Lecture 5: Data Transformation - Information based Learning | 30th Jan 2021
#*****************************************************************************


# Common Steps for any Predict modelling

# 1. Prepare Problem
#    a) Load packages
#    b) Load dataset
# 2. Summarize Data
#    a) Descriptive statistics
#    b) Data visualizations
# 3. Prepare Data
#    a) Data Cleaning
#    b) Feature Selection
#    c) Data Transforms
# 4. Evaluate Algorithms
#    a) Test options and evaluation metric
#    b) Spot-Check Algorithms
#    c) Compare Algorithms
# 5. Improve Accuracy
#    a) Algorithm Tuning
#    b) Ensembles
# 6. Finalize Model
#    a) Predictions on validation dataset
#    b) Create standalone model on entire training dataset
#    c) Save model for later use

#************************************************
# Information based learning


library(dplyr)
titanic<-read.csv("C:\\R\\PA\\titanic.csv")
head(titanic)
tail(titanic)
glimpse(titanic)

#************************************************
#Making changes to data type wherever needed

titanic$Passengerid<-NULL
titanic$Sex<-as.factor(titanic$Sex)
titanic$Pclass<-as.factor(titanic$Pclass)
titanic$Embarked<-as.factor(titanic$Embarked)
titanic$Survived<-as.factor(titanic$Survived)
summary(titanic)

glimpse(titanic)

#************************************************
#Creating Data Partition to track survivors


index = createDataPartition(y=titanic$Survived, p=0.7, list=FALSE)

train.set = titanic[index,]
test.set = titanic[-index,]


#************************************************

dim(train.set)
with(titanic, qplot(Age, Fare, colour=Survived, cex=2))

#*****************************************************************************
Lecture 6: Decision Tree | 6th Feb 2021
#*****************************************************************************
#*# fit the model


titanic.tree = train(Survived ~ ., 
                     data=train.set, 
                     method="rpart", 
                     trControl = trainControl(method = "cv"),
                     metric = "Accuracy",
                     tuneLength = 10)

titanic.tree
plot(titanic.tree)

#*#************************************************
# fit the model

xgrid<-expand.grid(cp = seq(0,0.2,0.01))
titanic.tree = train(Survived ~ ., 
                     data=train.set, 
                     method="rpart", 
                     trControl = trainControl(method = "cv"),
                     metric = "Accuracy",
                     tuneGrid = xgrid)


#tuneLength = 10)

titanic.tree
plot(titanic.tree)

titanic.tree$bestTune
titanic.tree$finalModel
summary(titanic.tree$finalModel)


#***********************************************
# plot the model
plot(titanic.tree$finalModel, uniform=TRUE,
     main="Classification Tree")
text(titanic.tree$finalModel, use.n.=TRUE, all=TRUE, cex=.8)

#***********************************************
library(rattle)
fancyRpartPlot(titanic.tree$finalModel)

titanic.pred = predict(titanic.tree, newdata = test.set)
table(titanic.pred, test.set$Survived)

#***********************************************
#Compute the Error parameter

error.rate = round(mean(titanic.pred != test.set$Survived),2)
error.rate

#***********************************************
#confusion Matrix

confusionMatrix(titanic.pred, as.factor(test.set$Survived),positive="Yes")


library(ROCit)
titanic.pred1 = predict(titanic.tree, newdata = test.set, type="prob")
df <- data.frame(pred =titanic.pred1[,2], actual = as.factor(test.set$Survived))
ROCit_obj <- rocit(score=df$pred,class=df$actual)
ROCit_obj
plot(ROCit_obj)
ksplot(ROCit_obj)

#***********************************************
#Various within Decision tree

# Conditionnal inference tree
library(party)
set.seed(123)
model <- train(
  Survived ~., data = train.set, method = "ctree2",
  trControl = trainControl("cv", number = 10),
  tuneLength = 10 )

plot(model$finalModel)
titanic.pred <-predict(model,test.set)
table(titanic.pred, test.set$Survived)

error.rate = round(mean(titanic.pred != test.set$Survived),2)
error.rate

confusionMatrix(titanic.pred, as.factor(test.set$Survived),positive="Yes")


#c50 Algorithm
library(C50)
model2 <- train(
  Survived ~., data = train.set, method = "C5.0",
  trControl = trainControl("cv", number = 10),
  tuneLength = 10)

model2
plot(model2)
model2$bestTune
model2$finalModel
summary(model2$finalModel)
model2.pred = predict(model2, newdata = test.set)
table(model2.pred, test.set$Survived)

error.rate = round(mean(model2.pred != test.set$Survived),2)
error.rate

confusionMatrix(model2.pred, as.factor(test.set$Survived),positive="Yes")
model2.pred1 = predict(model2, newdata = test.set, type="prob")
df <- data.frame(pred =model2.pred1[,2], actual = as.factor(test.set$Survived))
ROCit_obj <- rocit(score=df$pred,class=df$actual)
ROCit_obj
plot(ROCit_obj)
ksplot(ROCit_obj)

# grow tree
library(rpart)
fit <- rpart(Survived ~ .,method="class", data=train.set)
printcp(fit) # display the results
plotcp(fit) # visualize cross-validation results
summary(fit) # detailed summary of splits


# plot tree
plot(fit, uniform=TRUE,
     main="Classification Tree for Survived")
text(fit, use.n=TRUE, all=TRUE, cex=.8)

# prune the tree
pfit<- prune(fit, cp=   fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])

# plot the pruned tree
plot(pfit, uniform=TRUE,
     main="Pruned Classification Tree for Survived")
text(pfit, use.n=TRUE, all=TRUE, cex=.8)

#Random Forest Algorithm
library(randomForest)

model2 <- train(
  Survived ~., data = train.set, method = "ranger",
  trControl = trainControl("cv", number = 10),
  tuneLength = 10)

model2
plot(model2)
model2$bestTune
model2$finalModel
summary(model2$finalModel)
model2.pred = predict(model2, newdata = test.set)
table(model2.pred, test.set$Survived)

error.rate = round(mean(model2.pred != test.set$Survived),2)
error.rate

confusionMatrix(model2.pred, as.factor(test.set$Survived),positive="Yes")
model2.pred1 = predict(model2, newdata = test.set, type="prob")
df <- data.frame(pred =model2.pred1[,2], actual = as.factor(test.set$Survived))
ROCit_obj <- rocit(score=df$pred,class=df$actual)
ROCit_obj
plot(ROCit_obj)
ksplot(ROCit_obj)

# Other Alternative methods
library(rsample)      # data splitting 
library(randomForest) # basic implementation
library(ranger)       # a faster implementation of randomForest
library(caret)        # an aggregator package for performing many machine learning models
library(h2o)          # an extremely fast java-based platform

set.seed(123)
m1 <- randomForest(formula = Survived ~ .,data    = train.set)
m1$err.rate
plot(m1)
which.min(m1$err.rate[,1])
a=c()
for (i in 2:6) {
  model3 <- randomForest(Survived ~ ., data = train.set, ntree = 122, mtry = i, importance = TRUE)
  predValid <- predict(model3, test.set, type = "class")
  a[i-1] = mean(predValid == test.set$Survived)
}

# create training and validation data 
set.seed(123)
valid_split <- initial_split(train.set, .8)

# training data
survived_train_v2 <- analysis(valid_split)

# validation data
survived_valid <- assessment(valid_split)
x_test <- survived_valid[setdiff(names(survived_valid), "Survived")]
y_test <- survived_valid$Survived

rf_oob_comp <- randomForest(
  formula = Survived ~ .,
  data    = survived_train_v2,
  xtest   = x_test,
  ytest   = y_test
)

# names of features
features <- setdiff(names(train.set), "Survived")

set.seed(123)

m2 <- tuneRF(
  x          = train.set[features],
  y          = train.set$Survived,
  ntreeTry   = 500,
  mtryStart  = floor(length(features) / 3),
  stepFactor = 1.5,
  improve    = 0.001,
  trace      = FALSE      # to not show real-time progress 
)

# hyperparameter grid search
hyper_grid <- expand.grid(
  mtry       = seq(2, 7, by = 1),
  node_size  = seq(3, 9, by = 2),
  sampe_size = seq(0.5,0.8,by = 0.1),
  ntree = seq(100,1000,200)
)

# total number of combinations
nrow(hyper_grid)

for(i in 1:nrow(hyper_grid)) {
  
  # train model
  model <- ranger(
    formula         = Survived ~ ., 
    data            = train.set, 
    num.trees       = hyper_grid$ntree[i],
    mtry            = hyper_grid$mtry[i],
    min.node.size   = hyper_grid$node_size[i],
    sample.fraction = hyper_grid$sampe_size[i],
    seed            = 123
  )
  
  # add OOB error to grid
  hyper_grid$OOB_[i] <- model$prediction.error
}

xyz<-hyper_grid %>% 
  dplyr::arrange(OOB_) %>%
  head(10)

model2<-ranger(
  formula         = Survived ~ ., 
  data            = train.set, 
  num.trees       = xyz[1,4],
  mtry            = xyz[1,1],
  min.node.size   = xyz[1,2],
  sample.fraction = xyz[1,3],
  seed            = 123
)

model2.pred = predict(model2, test.set)
table(model2.pred$predictions, test.set$Survived)

error.rate = round(mean(model2.pred$predictions != test.set$Survived),2)
error.rate

confusionMatrix(model2.pred$predictions, as.factor(test.set$Survived),positive="Yes")
model2.pred1 = predict(model2, test.set, type="response")
df <- data.frame(pred =model2.pred1$predictions, actual = as.factor(test.set$Survived))
#ROCit_obj <- rocit(score=df$pred,class=df$actual)
ROCit_obj
plot(ROCit_obj)
ksplot(ROCit_obj)

varImp(model2)

# start up h2o
h2o.no_progress()
h2o.init(max_mem_size = "5g")
# create feature names
y <- "Survived"
x <- setdiff(names(train.set), y)

# turn training set into h2o object
train.h2o <- as.h2o(train.set)

# hyperparameter grid
hyper_grid.h2o <- list(
  ntrees      = seq(200, 500, by = 100),
  mtries      = seq(2, 7, by = 1),
  max_depth   = seq(3, 10, by = 1),
  min_rows    = seq(1, 5, by = 2),
  nbins       = seq(10, 30, by = 5),
  sample_rate = c(.55, .632, .75)
)

# random grid search criteria
search_criteria <- list(
  strategy = "RandomDiscrete",
  stopping_metric = "mse",
  stopping_tolerance = 0.005,
  stopping_rounds = 10,
  max_runtime_secs = 5*60
)

# build grid search 
random_grid <- h2o.grid(
  algorithm = "randomForest",
  grid_id = "rf_grid2",
  x = x, 
  y = y, 
  training_frame = train.h2o,
  hyper_params = hyper_grid.h2o,
  search_criteria = search_criteria
)

# collect the results and sort by our model performance metric of choice
grid_perf2 <- h2o.getGrid(
  grid_id = "rf_grid2", 
  sort_by = "mse", 
  decreasing = FALSE
)
print(grid_perf2)

#Gradient Boosting Machines
library(readxl)
library(tidyverse)
library(xgboost)
library(caret)
library(gbm)
set.seed(100)  # For reproducibility
# Create index for testing and training data
inTrain <- createDataPartition(y = titanic$Survived, p = 0.8, list = FALSE)
# subset titanic data to training
training <- titanic[inTrain,]
# subset the rest to test
testing <- titanic[-inTrain,]
# Train model with preprocessing & repeated cv
model_gbm <- caret::train(Survived ~ .,
                          data = training,
                          method = "gbm",
                          trControl = trainControl(method = "repeatedcv", 
                                                   number = 10, 
                                                   repeats = 3, 
                                                   verboseIter = TRUE,classProbs = T,summaryFunction = twoClassSummary),
                          verbose = 0,metric="ROC")
varImp(model_gbm)
summary(model_gbm)
print(model_gbm)

caret::confusionMatrix(
  data = predict(model_gbm, testing),
  reference = testing$Survived, positive = "Yes",
)

varImp(model_gbm)
library(xgboost)
dummies <- dummyVars(Survived ~ ., data = titanic, levelsOnly=TRUE,fullRank=TRUE)
titanic1<-as.data.frame(predict(dummies, newdata = titanic))
titanic1$Survived<-titanic$Survived

set.seed(100)  # For reproducibility
# Create index for testing and training data
inTrain <- createDataPartition(y = titanic1$Survived, p = 0.8, list = FALSE)
# subset titanic data to training
training <- titanic1[inTrain,]
# subset the rest to test
testing <- titanic1[-inTrain,]

X_train = xgb.DMatrix(as.matrix(training[,-11]))
y_train = training$Survived
X_test = xgb.DMatrix(as.matrix(testing[,-11]))
y_test = testing$Survived

xgb_trcontrol = trainControl(
  method = "repeatedcv",
  number = 10,  
  repeats=3,
  allowParallel = TRUE,
  verboseIter = FALSE,
  returnData = FALSE
)

xgbGrid <- expand.grid(nrounds = c(100,200),  
                       max_depth = c(10, 15, 20, 25),
                       colsample_bytree = seq(0.5, 0.9, length.out = 5),
                       eta = 0.1,
                       gamma=0,
                       min_child_weight = 1,
                       subsample = 1
)

set.seed(0) 
xgb_model = train(
  X_train, y_train,  
  trControl = xgb_trcontrol,
  tuneGrid = xgbGrid,
  method = "xgbTree"
)

xgb_model$bestTune

predicted = predict(xgb_model, X_test)
confusionMatrix(predicted,y_test)


library(h2o)
h2o.init(nthreads = -1)

h2o.no_progress()

data_hf <- as.h2o(titanic1)

splits <- h2o.splitFrame(data_hf, 
                         ratios = 0.8, 
                         seed = 1)

train <- splits[[1]]
test <- splits[[2]]

response <- "Survived"
features <- setdiff(colnames(train), response)

h2o_gbm<-h2o.gbm(x = features, 
                 y = response, 
                 training_frame = train,
                 nfolds = 3) # cross-validation
h2o_gbm
h2o.performance(h2o_gbm, test)