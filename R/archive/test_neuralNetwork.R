load("data/ohe/Census.train.ohe.Rda")
load("data/ohe/Census.test.ohe.Rda")
library(neuralnet)

train <- Census.train.ohe[1:1000,-1]
test <- Census.test.ohe[,-1]

n <- names(train)
f <- as.formula(paste("student ~", paste(n[!n %in% "student"], collapse = "+")))
nn <- neuralnet(f, data=train, hidden=3,linear.output=FALSE)

pr.nn <- predict(nn,test)

pr.nn_ <- pr.nn$net.result*(max(data$medv)-min(data$medv))+min(data$medv)
test.r <- (test_$medv)*(max(data$medv)-min(data$medv))+min(data$medv)

MSE.nn <- sum((test.r - pr.nn_)^2)/nrow(test_)