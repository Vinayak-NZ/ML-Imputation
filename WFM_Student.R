#### Study the variable: How many units in each category? ####
St <- table(Census$student)

St
# What is the distribution of the variable: Remove NCR and plot to look at distribution
g <- ggplot(Census[!Census$student == -9, ], aes(student))

g + geom_bar() + scale_x_discrete(name = "Student status",
  breaks = pretty_breaks())

#### Tidy/treat the training and test datasets ####
# Remove units with NCR codes for variable & Remove the personal identifier
Census.train.tidy <- Census.train[!Census.train$student == -9, -1]

Census.test.tidy <- Census.test[!Census.test$student == -9, -1]

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
train.outcome <- Census.train.tidy$student

train.predictors <- sparse.model.matrix(student ~ .,
                                        data = Census.train.tidy
)[, -1]

test.outcome <- Census.test.tidy.miss$student

test.predictors <- model.matrix(student ~ .,
                                data = Census.test.tidy.miss
)[, -1]

# Convert the matrix objects to DMatrix objects  
dtrain <- xgb.DMatrix(train.predictors, label=train.outcome)

dtest <- xgb.DMatrix(test.predictors, missing=-999)

# Train a model using training set
trainS_v1 <- xgboost(
  data = dtrain, max_depth = 2, eta = 1, nthread = 2, nrounds = 10,
  objective = "reg:logistic", missing=-999
)

#### Test the model ####
predicted <- ifelse(
  predict(trainS_v1, dtest, missing = -999, na.action = na.pass) > 0.5, 1, 0)

#### Evaluate performance of model ####
# Compare versions of the outcome variable (Actual, Predicted, Missing)
actuals <- Census.test.tidy$student

missing <- Census.test.tidy.miss$student

compareVar <- tibble(
  Actuals = actuals, Predictions = predicted,
  Missing = missing
)

compareMissing <- compareVar[compareVar$Missing == -999, ]

compareMissing$indicator <- ifelse(compareMissing$Actuals ==
                                     compareMissing$Predictions,"Correct", "Wrong")

counts <- table(compareMissing$indicator)

barplot(counts, main = "Accuracy of predictions", xlab = "Outcome")

# Using Confusion Matrix to evaluate predictions
confusionML <- confusionMatrix(
  as.factor(compareVar$Actuals),
  as.factor(compareVar$Predictions)
)

qplot(Actuals, Predictions,
      data = compareVar, colour = Actuals,
      geom = c("jitter"), main = "predicted vs. observed in test data",
      xlab = "Observed Class", ylab = "Predicted Class"
)

#### Examine feature importance ####
importance_matrix <- xgb.importance(model = trainS_v1)

print(importance_matrix)

xgb.plot.importance(importance_matrix = importance_matrix)

#### Plot the individual trees of the model ####
# Tree 1
xgb.plot.tree(model = trainS_v1, tree=1)
# Tree 2
xgb.plot.tree(model = trainS_v1, tree=2)
# Tree 3
xgb.plot.tree(model = trainS_v1, tree=3)

#### Save model ####
xgb.save(trainS_v1, "models/xgboost.student")

#### Load model ####
trainS_v1 <- xgb.load("models/xgboost.student")

#### Impute values using CANCEIS ####
# Create CANCEIS input file with imputable and matching variables
CANCEIS.input <- Census.test.tidy.miss[, c(
  "student", "social.grade", "occupation",
  "industry", "age", "econ.act"
)]

CANCEIS.input$canceis.id <- 1:nrow(CANCEIS.input)

CANCEIS.input <- CANCEIS.input[, c(
  "canceis.id", "student", "social.grade", "occupation",
  "industry", "age", "econ.act"
)]

write.table(CANCEIS.input,
            file = "data/Student/xxxUNIT01IG01.txt", sep = "\t",
            row.names = FALSE, col.names = FALSE
)

# Read in CANCEIS input and output
CANCEIS.test.in <- read.table("data/Student/xxxUNIT01IG01.txt",
                              header = FALSE,
                              col.names = c(
                                "canceis.id", "student", "social.grade",
                                "occupation", "industry", "age", "econ.act"
                              )
)[, -1]

CANCEIS.test.out <- read.table("data/Student/XXXUNITIMP01IG01.txt",
                               header = FALSE,
                               col.names = c(
                                 "canceis.id", "student", "social.grade",
                                 "occupation", "industry", "age", "econ.act"
                               )
)[, -1]

# Compare predicted and actuals
actuals.CANCEIS <- Census.test.tidy$student

missing.CANCEIS <- CANCEIS.test.in$student

predicted.CANCEIS <- CANCEIS.test.out$student

compare_var_CANCEIS <- data_frame(
  Actuals = actuals.CANCEIS, Predictions =
    predicted.CANCEIS, Missing = missing.CANCEIS
)

compare_missing_CANCEIS <- compare_var_CANCEIS[
  compare_var_CANCEIS$Missing == -999, ]

compare_missing_CANCEIS$indicator <- ifelse(
  compare_missing_CANCEIS$Actuals ==
    compare_missing_CANCEIS$Predictions,
  "Correct", "Wrong"
)

counts_CANCEIS <- table(compare_missing_CANCEIS$indicator)

barplot(counts_CANCEIS, main = "Accuracy of predictions", xlab = "Outcome")

# Using Confusion Matrix to evaluate predictions
confusion_CANCEIS <- confusionMatrix(
  as.factor(compare_missing_CANCEIS$Actuals),
  as.factor(compare_missing_CANCEIS$Predictions)
)

qplot(Actuals, Predictions,
      data = compare_missing_CANCEIS, colour = Actuals,
      geom = c("jitter"), main = "predicted vs. observed in validation data",
      xlab = "Observed Class", ylab = "Predicted Class"
)
