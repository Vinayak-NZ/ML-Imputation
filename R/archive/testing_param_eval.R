library(mlr)
library(parallel)
library(parallelMap)

newTrain <- Census.train.tidy
newTest <- Census.test.tidy

newTrain$social.grade <- as.factor(newTrain$social.grade)
newTest$social.grade <- as.factor(newTest$social.grade)

traintask <- makeClassifTask (data = newTrain, target = "social.grade")
testtask <- makeClassifTask (data = newTest, target = "social.grade")

lrn <- makeLearner("classif.xgboost", predict.type = "response")
lrn$pars.vals <- list(objective="multi:softmax", eval_metric = "merror", nrounds =100L, eta=0.1)

params <- makeParamSet( makeDiscreteParam("booster",values = "gbtree"), 
                        makeIntegerParam("max_depth",lower = 3L,upper = 10L), 
                        makeNumericParam("min_child_weight",lower = 1L,upper = 10L), 
                        makeNumericParam("subsample",lower = 0.5,upper = 1), 
                        makeNumericParam("colsample_bytree",lower = 0.5,upper = 1))

rdesc <- makeResampleDesc("CV",stratify = T,iters=5L)

ctrl <- makeTuneControlRandom(maxit = 10L)

mytune <- tuneParams(learner = lrn, task = traintask, resampling = rdesc, 
                     measures = acc, par.set = params, control = ctrl, 
                     show.info = T)

lrn_tune <- setHyperPars(lrn,par.vals = mytune$x)

xgmodel <- train(learner = lrn_tune,task = traintask)

xgpred <- predict(xgmodel,testtask)

predicted <- xgpred$data$response

confusionMatrix(xgpred$data$response,xgpred$data$truth)


#set parallel backend
parallelStartSocket(cpus = detectCores())

#parameter tuning
mytune <- tuneParams(learner = lrn, task = traintask, resampling = rdesc, measures = acc, par.set = params, control = ctrl, show.info = T)
> mytune$y 
#0.873069
#set parameter space
params <- makeParamSet( makeDiscreteParam("booster",values = c("gbtree","gblinear")), 
                          makeIntegerParam("max_depth",lower = 3L,upper = 10L), 
                          makeNumericParam("min_child_weight",lower = 1L,upper = 10L), 
                          makeNumericParam("subsample",lower = 0.5,upper = 1), 
                          makeNumericParam("colsample_bytree",lower = 0.5,upper = 1))

#set resampling strategy

predicted <- predict(trainSG_v1, dtest, missing = -999, na.action = na.pass)

lb <- as.numeric(Census.test.tidy$social.grade) - 1
pred <- predict(trainSG_v1, dtest, missing = -999, na.action = na.pass)
str(pred)
# reshape it to a num_class-columns matrix
pred <- matrix(pred, ncol=5, byrow=TRUE)
# convert the probabilities to softmax labels
pred_labels <- max.col(pred) - 1
# the following should result in the same error as seen in the last iteration
sum(pred_labels != lb)/length(lb)
