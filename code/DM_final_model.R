#####Loading package
library(tidyverse)
library(caret) #modeling package
library(xgboost)#model:XGboost
library(randomForest) #model:RandomForest
library(kernlab) #model:SVM
library(doParallel) #Parallel computing
#####Loading data
file_path <- "C:/Users/陳昱廷/Desktop/Data_mining_final/"
file_name <- "HRV_data_update.csv"
file <- paste(file_path, file_name, sep =  "")
HRV_data <- read.csv(file)
HRV_data <- HRV_data[,-1]
set.seed(5566)
#####Spliting data
trainIndex <- createDataPartition(HRV_data$Label, p = .9, 
                                  list = FALSE, 
                                  times = 1)
train_data <- HRV_data[trainIndex, ]
test_data_X <- HRV_data[-trainIndex, -1]
test_data_y <-  HRV_data[-trainIndex, 1]
#####Data Preprocessing
#Log transform (SDNN, RMSSD)
train_data_log <- train_data %>% select(SDNN,RMSSD)  %>% log()
train_data_fix <- train_data %>% select(-c("SDNN","RMSSD"))
train_data_all_log <- cbind(train_data_fix,train_data_log)
train_data_all_log$Label <- factor(train_data_all_log$Label)
#####Cross-Vaildation predefine
fitControl <- trainControl(method = "repeatedcv",
                           number = 10,  # K-fold
                           repeats = 5,  #Repeat in  K-fold
                           ## Evaluate performance using 
                           ## the following function
                           summaryFunction = multiClassSummary)
#####Modeling
###Cross-Validattion 5-fold, repeat 10 times
#(1)KNN
#(1-1)Without log transform
cl <- makePSOCKcluster(4)
registerDoParallel(cl)
KNN <-train(Label ~ ., data = train_data, 
            method = "knn", 
            trControl = fitControl)
stopCluster(cl)

#(1-2)With log transform
cl <- makePSOCKcluster(4)
registerDoParallel(cl)
KNN_log <-train(Label ~ ., data = train_data_all_log, 
            method = "knn", 
            trControl = fitControl)
stopCluster(cl)

#(2)SVM
#(2-1)Without log transform,without standardization
cl <- makePSOCKcluster(4)
registerDoParallel(cl)
SVM <-train(Label ~ ., data = train_data, 
            method = "svmRadial",
            trControl = fitControl)
stopCluster(cl)

#(2-2)Without log transform,with standardization
cl <- makePSOCKcluster(4)
registerDoParallel(cl)
SVM_std <-train(Label ~ ., data = train_data, 
            method = "svmRadial",
            preProc = c("center", "scale"),
            trControl = fitControl)
stopCluster(cl)

#(2-3)With log transform,without standardization
cl <- makePSOCKcluster(4)
registerDoParallel(cl)
SVM_log <-train(Label ~ ., data = train_data_all_log, 
            method = "svmRadial",
            trControl = fitControl)
stopCluster(cl)

#(3)Xgboost
#(3-1)Without log transform,without standardization
cl <- makePSOCKcluster(4)
registerDoParallel(cl)
Xgboost <-train(Label ~ ., data = train_data, 
                method = "xgbTree",
                trControl = fitControl)
stopCluster(cl)

#(3-2)Without log transform,with standardization
cl <- makePSOCKcluster(4)
registerDoParallel(cl)
Xgboost_std <-train(Label ~ ., data = train_data, 
                method = "xgbTree",
                preProc = c("center", "scale"),
                trControl = fitControl)
stopCluster(cl)
#(3-3)With log transform,without standardization
cl <- makePSOCKcluster(4)
registerDoParallel(cl)
Xgboost_log <-train(Label ~ ., data = train_data_all_log, 
                    method = "xgbTree",
                    trControl = fitControl)
stopCluster(cl)

#(4)Random Forest
#(4-1)Without log transform,without standardization
cl <- makePSOCKcluster(4) 
registerDoParallel(cl)
RF <- train(Label ~ ., data = train_data_all_log, 
             method = "rf", 
             trControl = fitControl,
             verbose = FALSE)
stopCluster(cl)
#(4-2)Without log transform,with standardization
cl <- makePSOCKcluster(4)
registerDoParallel(cl)
RF_std <-train(Label ~ ., data = train_data, 
                    method = "rf",
                    preProc = c("center", "scale"),
                    trControl = fitControl)
stopCluster(cl)
#(4-3)With log transform,without standardization
cl <- makePSOCKcluster(4)
registerDoParallel(cl)
RF_log <-train(Label ~ ., data = train_data_all_log, 
                    method = "rf",
                    trControl = fitControl)
stopCluster(cl)

#(5) Logistic Regression
#(5-1)Without log transform,without standardization
cl <- makePSOCKcluster(4)
registerDoParallel(cl)
LR <-train(Label ~ ., data = train_data, 
           method = "regLogistic",
           trControl = fitControl)
stopCluster(cl)
#(5-2)Without log transform,with standardization
cl <- makePSOCKcluster(4)
registerDoParallel(cl)
LR_std <-train(Label ~ ., data = train_data, 
           method = "regLogistic",
           preProc = c("center", "scale"),
           trControl = fitControl)
stopCluster(cl)
#(5-3)Without log transform,with standardization
cl <- makePSOCKcluster(4)
registerDoParallel(cl)
LR_log <-train(Label ~ ., data = train_data_all_log, 
               method = "regLogistic",
               trControl = fitControl)
stopCluster(cl)

#####Candidate model
mtry = 8; ntree = 500
model <- randomForest(Label~.,data=train_data ,mtry=mtry,
                      ntree =ntree, importance =TRUE)
model
##### Predicting on testing set
predTest <- predict(model, test_data)
CM <- table(test_data_y, predTest, dnn = c("實際", "預測"))
CM <- as.matrix(CM)
###Total Accuracy 
accuracy <- sum(diag(CM)) / sum(CM)
accuracy

###Recall
diag(CM)/apply(CM,1,sum)
(mean_recall <- sum(diag(CM)/apply(CM,1,sum))/3)
###Precision
diag(CM)/apply(CM,2,sum)
(mean_precision <- sum(diag(CM)/apply(CM,2,sum))/3)
