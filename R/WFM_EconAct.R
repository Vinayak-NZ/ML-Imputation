## ---- study-econ-act
# Study the variable: How many units in each category?
EAt <- table(Census$econ.act)

EAt

# What is the distribution of the variable: Remove NCR and plot to look at distribution
g <- ggplot(Census[!Census$econ.act == -9, ], aes(econ.act))

g + geom_bar() + scale_x_discrete(
  name = "Economic Activity",
  breaks = pretty_breaks()
)

## ---- tidy-econ-act
# Tidy/treat the training and test datasets
# Remove units with NCR codes for variable & Remove the personal identifier
Census.train.tidy <- Census.train[!Census.train$econ.act == -9, c(-1, -17)]

Census.test.tidy <- Census.test[!Census.test$econ.act == -9, c(-1, -17)]

## ---- miss-econ-act
# Simulate missingness in test data & Convert all missing responses to -999
Census.test.tidy.amp <- ampute(Census.test.tidy, prop = 0.7)

Census.test.tidy.miss <- Census.test.tidy.amp$amp

# Study the test dataset with missingness: How much missingness per variable?
NumberMissing <- sapply(Census.test.tidy.miss, function(y) sum(length(
    which(is.na(y))
  )))

TestNumberMissing <- data.frame(NumberMissing)

# Convert missing cases to -999
Census.test.tidy.miss[is.na(Census.test.tidy.miss)] <- -999

# Save dataset with missingenss
save(Census.test.tidy.miss, file = "data/EconAct/Census.test.tidy.miss.Rda")

## ---- train-econ-act
# Train the model
# Convert the data to matrix and assign output variable
train.outcome <- Census.train.tidy$econ.act

train.predictors <- sparse.model.matrix(econ.act ~ .,
  data = Census.train.tidy
)[, -1]

test.outcome <- Census.test.tidy.miss$econ.act

test.predictors <- model.matrix(econ.act ~ .,
  data = Census.test.tidy.miss
)[, -1]

# Convert the matrix objects to DMatrix objects
dtrain <- xgb.DMatrix(train.predictors, label = train.outcome)

dtest <- xgb.DMatrix(test.predictors, missing = -999)

# Train a model using training set
trainEA_v1 <- xgboost(
  data = dtrain, max_depth = 2, eta = 1, nthread = 2, nrounds = 10,
  objective = "multi:softmax", num_class = 10, missing = -999
)

# Examine feature importance
importance_matrix <- xgb.importance(model = trainEA_v1)

print(importance_matrix)

xgb.plot.importance(importance_matrix = importance_matrix)

# Save model
xgb.save(trainEA_v1, "XGBoost/xgboost.econAct")

## ---- test-econ-act
# Test the model
predicted <- predict(trainEA_v1, dtest, missing = -999, na.action = na.pass)

# Save predicted values
save(predicted, file = "data/EconAct/XGBoost/predicted.RData")

## ---- imp-econact-CANCEIS
# Impute values using CANCEIS
# Create CANCEIS input file with imputable and matching variables
CANCEIS.input <- Census.test.tidy.miss[, c(
  "econ.act", "student", "industry",
  "age", "occupation", "social.grade"
)]

CANCEIS.input$canceis.id <- 1:nrow(CANCEIS.input)
CANCEIS.input <- CANCEIS.input[, c(
  "canceis.id", "econ.act", "student", "industry",
  "age", "occupation", "social.grade"
)]

write.table(CANCEIS.input,
  file = "data/EconAct/CANCEIS/xxxUNIT01IG01.txt", sep = "\t",
  row.names = FALSE, col.names = FALSE
)

## ---- imp-econact-CANCEISXG
# Impute values using CANCEIS (with XGBoost to advise selection of MVs)
# Create CANCEIS input file with imputable and matching variables
CANCEISXG.input <- Census.test.tidy.miss[, c(
  "econ.act", "hours.cont", "age", "student",
  "sex", "health", "industry"
)]

CANCEISXG.input$canceis.id <- 1:nrow(CANCEISXG.input)
CANCEISXG.input <- CANCEISXG.input[, c(
  "canceis.id", "econ.act", "hours.cont", "age", "student",
  "sex", "health", "industry"
)]

write.table(CANCEISXG.input,
  file = "data/EconAct/MixedMethods/xxxUNIT01IG01.txt", sep = "\t",
  row.names = FALSE, col.names = FALSE
)
