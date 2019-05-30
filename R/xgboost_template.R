## ---- load-XG
library(caret)

library(xgboost)

## ---- data-prepXG
# Load data
load("data/Income_tree.RData")

# Remove identifier
Income <- Income[,-1]

## ---- split-XG
# Split data into training and test
set.seed(5)

s <- createDataPartition(Income$income, p = 0.8, list=FALSE)

training <- Income[s,]

test <- Income[-s,]

## ---- matrix-XG
# Convert the data to matrix and assign output variable
train.outcome <- training$income

train.predictors <- sparse.model.matrix(income ~ .,
                                        data = training
)[, -1]

test.outcome <- test$income

test.predictors <- model.matrix(income ~ .,
                                data = test
)[, -1]

# Convert the matrix objects to DMatrix objects
dtrain <- xgb.DMatrix(train.predictors, label=train.outcome)

dtest <- xgb.DMatrix(test.predictors)

## ---- train-XG
# Train the model
model <- xgboost(
  data = dtrain, max_depth = 2, eta = 1, nthread = 2, nrounds = 10,
  objective = "reg:linear")

## ---- test-XG
# Test the model
pred <- predict(model, dtest)

# Evaluate the performance of model
RMSE(pred,test.outcome)

## ---- feature-XG
# Examine feature importance
importance_matrix <- xgb.importance(model = model)

print(importance_matrix)

xgb.plot.importance(importance_matrix = importance_matrix)

## ---- trees-XG
# Plot the trees
# Tree 1
xgb.plot.tree(model = model, tree=0)
# Tree 2
xgb.plot.tree(model = model, tree=1)
# Tree 3
xgb.plot.tree(model = model, tree=2)
