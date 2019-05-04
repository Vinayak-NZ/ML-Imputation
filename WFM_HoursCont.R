#### Study the variable: How many units in each category? ####
summary(Census$hours.cont)

# What is the distribution of the variable: Remove NCR and plot to look at distribution
g <- ggplot(Census[!Census$hours.cont == -9, ], aes(hours.cont))

g + geom_histogram() + stat_bin(binwidth = 10)

#### Tidy/treat the training and test datasets ####
# Remove units with NCR codes for variable & Remove the personal identifier
Census.train.tidy <- Census.train[!Census.train$hours.cont == -9, c(-1,-17)]

Census.test.tidy <- Census.test[!Census.test$hours.cont == -9, c(-1,-17)]

#### Simulate missingness in test data & Convert all missing responses to -999 ####
Census.test.tidy.amp <- ampute(Census.test.tidy, prop = 0.7)

Census.test.tidy.miss <- Census.test.tidy.amp$amp
# Study the test dataset with missingness: How much missingness per variable?
NumberMissing <- sapply(Census.test.tidy.miss, function(y) sum(length(
  which(is.na(y))
)))

TestNumberMissing <- data.frame(NumberMissing)
# Convert missing cases to -999
Census.test.tidy.miss[is.na(Census.test.tidy.miss)] <- -999

#### Train the model ####
# Convert the data to matrix and assign output variable
train.outcome <- Census.train.tidy$hours.cont

train.predictors <- sparse.model.matrix(hours.cont ~ .,
                                        data = Census.train.tidy
)[, -1]

test.outcome <- Census.test.tidy.miss$hours.cont

test.predictors <- model.matrix(hours.cont ~ .,
                                data = Census.test.tidy.miss
)[, -1]

# Convert the matrix objects to DMatrix objects  
dtrain <- xgb.DMatrix(train.predictors, label=train.outcome)

dtest <- xgb.DMatrix(test.predictors, missing=-999)

# Train a model using training set
trainHC_v1 <- xgboost(
  data = dtrain, max_depth = 2, eta = 1, nthread = 2, nrounds = 10,
  objective = "reg:linear", missing=-999
)

#### Test the model ####
predicted <- predict(trainHC_v1, dtest, missing = -999, na.action = na.pass)

#### Evaluate performance of model ####
# Compare versions of the outcome variable (Actual, Predicted, Missing)
actuals <- Census.test.tidy$hours.cont

missing <- Census.test.tidy.miss$hours.cont

compareVar <- tibble(
  Actuals = actuals, Predictions = predicted,
  Missing = missing
)

compareMissing <- compareVar[compareVar$Missing == -999, ]

# Using Mean Absolute Error and Root Mean Square error to evaluate predictions
MAE(compareMissing$Predictions,compareMissing$Actuals)

RMSE(compareMissing$Predictions,compareMissing$Actuals)

#### Examine feature importance ####
importance_matrix <- xgb.importance(model = trainHC_v1)

print(importance_matrix)

xgb.plot.importance(importance_matrix = importance_matrix)

#### Plot the individual trees of the model ####
# Tree 1
xgb.plot.tree(model = trainHC_v1, tree=1)
# Tree 2
xgb.plot.tree(model = trainHC_v1, tree=2)
# Tree 3
xgb.plot.tree(model = trainHC_v1, tree=3)

#### Save model ####
xgb.save(trainHC_v1, "models/xgboost.hoursCont")

#### Load model ####
trainHC_v1 <- xgb.load("models/xgboost.hoursCont")

#### Impute values using CANCEIS ####
# Create CANCEIS input file with imputable and matching variables
CANCEIS.input <- Census.test.tidy.miss[, c(
  "hours.cont", "social.grade", "industry",
  "occupation", "student"
)]

CANCEIS.input$canceis.id <- 1:nrow(CANCEIS.input)

CANCEIS.input <- CANCEIS.input[, c(
  "canceis.id", "hours.cont", "social.grade", "industry",
  "occupation", "student"
)]

write.table(CANCEIS.input,
            file = "data/HoursCont/xxxUNIT01IG01.txt", sep = "\t",
            row.names = FALSE, col.names = FALSE
)

# Read in CANCEIS input and output
CANCEIS.test.in <- read.table("data/HoursCont/xxxUNIT01IG01.txt",
                              header = FALSE,
                              col.names = c(
                                "canceis.id", "hours.cont", "social.grade", 
                                "industry", "occupation", "student"
                              )
)[, -1]

CANCEIS.test.out <- read.table("data/HoursCont/XXXUNITIMP01IG01.txt",
                               header = FALSE,
                               col.names = c(
                                 "canceis.id", "hours.cont", "social.grade", 
                                 "industry", "occupation", "student"
                               )
)[, -1]

# Compare predicted and actuals
actuals.CANCEIS <- Census.test.tidy$hours.cont

missing.CANCEIS <- CANCEIS.test.in$hours.cont

predicted.CANCEIS <- CANCEIS.test.out$hours.cont

compare_var_CANCEIS <- tibble(
  Actuals = actuals.CANCEIS, Predictions =
    predicted.CANCEIS, Missing = missing.CANCEIS
)

compare_missing_CANCEIS <- compare_var_CANCEIS[
  compare_var_CANCEIS$Missing == -999, ]

# Using Mean Absolute Error and Root Mean Square error to evaluate predictions
MAE(compare_missing_CANCEIS$Predictions, compare_missing_CANCEIS$Actuals)

RMSE(compare_missing_CANCEIS$Predictions, compare_missing_CANCEIS$Actuals)