dtrain = xgb.DMatrix(X_train, label=y_train)
dtest = xgb.DMatrix(X_test, label=y_test)

dtrain <- xgb.DMatrix(as.matrix(Census.train.tidy), label=Census.train.tidy$social.grade)
dtest <- xgb.DMatrix(as.matrix(Census.test.tidy), label=Census.test.tidy$social.grade)

param <- list(booster = "gbtree", max_depth = "8", min_child_weight = "7.79734527645633", 
          subsample = "0.876703443238512", colsample_bytree = "0.965550040476955", 
          objective = "multi:softprob", num_class = "5", silent = "1")

test <- xgb.train(param = param, dtrain, nrounds = 1)

xgpred <- predict(test, dtest)

lb <- as.numeric(Census.test.tidy$social.grade)
str(xgpred)
# reshape it to a num_class-columns matrix
pred <- matrix(xgpred, ncol=5, byrow=TRUE)
# convert the probabilities to softmax labels
pred_labels <- max.col(pred)
# the following should result in the same error as seen in the last iteration
sum(pred_labels != lb)/length(lb)
