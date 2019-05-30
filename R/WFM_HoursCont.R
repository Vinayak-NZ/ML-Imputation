## ---- study-hours-cont
# Study the variable: How many units in each category?
summary(Census$hours.cont)

# What is the distribution of the variable: Remove NCR and plot to look at distribution
g <- ggplot(Census[!Census$hours.cont == -9, ], aes(hours.cont))

g + geom_histogram() + stat_bin(binwidth = 10)

## ---- tidy-hours-cont
# Tidy/treat the training and test datasets
# Remove units with NCR codes for variable & Remove the personal identifier
Census.train.tidy <- Census.train[!Census.train$hours.cont == -9, c(-1,-17)]

Census.test.tidy <- Census.test[!Census.test$hours.cont == -9, c(-1,-17)]

## ---- miss-hours-cont
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
save(Census.test.tidy.miss, file="data/HoursCont/Census.test.tidy.miss.Rda")

## ---- train-hours-cont
# Train the model
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

# Examine feature importance
importance_matrix <- xgb.importance(model = trainHC_v1)

print(importance_matrix)

xgb.plot.importance(importance_matrix = importance_matrix)

# Save model
xgb.save(trainHC_v1, "XGBoost/xgboost.hoursCont")

## ---- test-hours-cont
# Test the model
predicted <- predict(trainHC_v1, dtest, missing = -999, na.action = na.pass)

# Save predicted values
save(predicted, file = "data/HoursCont/XGBoost/predicted.RData")

## ---- imp-hourscont-CANCEIS
# Impute values using CANCEIS
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

## ---- imp-hourscont-CANCEISXG
# Impute values using CANCEIS (with XGBoost to advise selection of MVs)
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