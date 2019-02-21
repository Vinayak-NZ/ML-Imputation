#Study the variable: How many units in each category?
t1<-table(Census$EconAct)
t1

#What is the distribution of the variable: Remove NCR and plot to look at distribution
g <- ggplot(Census[!Census$EconAct==-9,], aes(EconAct))
g + geom_bar()

#Train the model
##Remove units with NCR codes for variable
Census.train.tidy<-Census.train[!Census.train$EconAct==-9,]
Census.test.tidy<-Census.test[!Census.test$EconAct==-9,]
Census.test.miss.tidy<-Census.test.miss[!Census.test.miss$EconAct==-9,]
##Convert the data to matrix and assign output variable
train.outcome <-Census.train.tidy$EconAct
train.predictors <- sparse.model.matrix(EconAct ~ ., data = Census.train.tidy)[,-1]
test.outcome<-Census.test.miss.tidy$EconAct
test.predictors<-model.matrix(EconAct ~ ., data = Census.test.miss.tidy)[,-1]
##Train
trainEA_v1 <- xgboost(data = train.predictors, label = train.outcome, max_depth = 2,
                      eta = 1, nthread = 2, nrounds = 10,objective = "multi:softmax",
                      num_class=10)

#Test the model
predicted <- predict(trainEA_v1, newdata = test.predictors, missing = -999)

#Accuracy of Predictions
##Compare versions of the outcome variable (Actual, Predicted, Missing)
actuals <- Census.test.miss$EconAct
missing <- Census.test.miss.tidy$EconAct

compare_var<-data_frame(Actuals = actuals, Predictions = predicted, Missing = missing)
compare_missing<-compare_var[compare_var$Missing=-999,]

output_test$indicator<-ifelse(output_test$Actuals==output_test$Predictions,
                              'Correct','Wrong')
counts<-table(output_test$indicator)
barplot(counts, main="Accuracy of predictions", 
        xlab="Outcome")