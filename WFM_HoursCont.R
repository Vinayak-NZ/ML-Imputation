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
# Save dataset with missingenss
save(Census.test.tidy.miss, file="data/HoursCont/Census.test.tidy.miss.Rda")
# Load dataset with missingenss
load("data/HoursCont/Census.test.tidy.miss.Rda")

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
# Save predicted values
save(predicted, file = "data/HoursCont/XGBoost/predicted.RData")

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
            file = "data/HoursCont/CANCEIS/xxxUNIT01IG01.txt", sep = "\t",
            row.names = FALSE, col.names = FALSE
)

# Read in CANCEIS input and output
CANCEIS.test.in <- read.table("data/HoursCont/CANCEIS/xxxUNIT01IG01.txt",
                              header = FALSE,
                              col.names = c(
                                "canceis.id", "hours.cont", "social.grade", "industry",
                                "occupation", "student"
                              )
)[, -1]

CANCEIS.test.out <- read.table("data/HoursCont/CANCEIS/XXXUNITIMP01IG01.txt",
                               header = FALSE,
                               col.names = c(
                                 "canceis.id", "hours.cont", "social.grade", "industry",
                                 "occupation", "student"
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

#### Impute values using CANCEIS (with XGBoost to advise selection of MVs) ####
# Create CANCEIS input file with imputable and matching variables
CANCEISXG.input <- Census.test.tidy.miss[, c(
  "hours.cont", "sex", "student",
  "occupation", "social.grade", "age", "industry"
)]

CANCEISXG.input$canceis.id <- 1:nrow(CANCEISXG.input)

CANCEISXG.input <- CANCEISXG.input[, c(
  "canceis.id", "hours.cont", "sex", "student",
  "occupation", "social.grade", "age", "industry"
)]

write.table(CANCEISXG.input,
            file = "data/HoursCont/MixedMethods/xxxUNIT01IG01.txt", sep = "\t",
            row.names = FALSE, col.names = FALSE
)

# Read in CANCEISXG input and output
CANCEISXG.test.in <- read.table("data/HoursCont/MixedMethods/xxxUNIT01IG01.txt",
                                header = FALSE,
                                col.names = c(
                                  "canceis.id", "hours.cont", "sex", "student",
                                  "occupation", "social.grade", "age", "industry"
                                )
)[, -1]

CANCEISXG.test.out <- read.table("data/HoursCont/MixedMethods/XXXUNITIMP01IG01.txt",
                                 header = FALSE,
                                 col.names = c(
                                   "canceis.id", "hours.cont", "sex", "student",
                                   "occupation", "social.grade", "age", "industry"
                                 )
)[, -1]

# Compare predicted and actuals
actuals.CANCEISXG <- Census.test.tidy$hours.cont

missing.CANCEISXG <- CANCEISXG.test.in$hours.cont

predicted.CANCEISXG <- CANCEISXG.test.out$hours.cont

compare_var_CANCEISXG <- tibble(
  Actuals = actuals.CANCEISXG, Predictions =
    predicted.CANCEISXG, Missing = missing.CANCEISXG
)

compare_missing_CANCEISXG <- compare_var_CANCEISXG[
  compare_var_CANCEISXG$Missing == -999, ]

# Using Mean Absolute Error and Root Mean Square error to evaluate predictions
MAE(compare_missing_CANCEISXG$Predictions, compare_missing_CANCEISXG$Actuals)

RMSE(compare_missing_CANCEISXG$Predictions, compare_missing_CANCEISXG$Actuals)

#### Impute values using median imputation ####
# Create a vector of imputable variable excluding missing values
median.dat <- Census.test.tidy.miss[
  Census.test.tidy.miss$hours.cont != -999, ]

median.val <- median(median.dat$hours.cont)

# Compare predicted and actuals
actuals.median <- Census.test.tidy$hours.cont

missing.median <- Census.test.tidy.miss$hours.cont

predicted.median <- ifelse(
  Census.test.tidy.miss$hours.cont == -999, median.val, 
  Census.test.tidy.miss$hours.cont)

compare_var_median <- tibble(
  Actuals = actuals.median, Predictions =
    predicted.median, Missing = missing.median
)

# Using Mean Absolute Error and Root Mean Square error to evaluate predictions
MAE(compare_var_median$Predictions, compare_var_median$Actuals)

RMSE(compare_var_median$Predictions, compare_var_median$Actuals)
