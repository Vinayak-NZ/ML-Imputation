#### Study the variable: How many units in each category? ####
EAt <- table(Census$econ.act)

EAt
# What is the distribution of the variable: Remove NCR and plot to look at distribution
g <- ggplot(Census[!Census$econ.act == -9, ], aes(econ.act))

g + geom_bar() + scale_x_discrete(name = "Economic Activity",
  breaks = pretty_breaks())

#### Tidy/treat the training and test datasets ####
# Remove units with NCR codes for variable & Remove the personal identifier
Census.train.tidy <- Census.train[!Census.train$econ.act == -9, c(-1,-17)]

Census.test.tidy <- Census.test[!Census.test$econ.act == -9, c(-1,-17)]

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
save(Census.test.tidy.miss, file="data/EconAct/Census.test.tidy.miss.Rda")
# Load dataset with missingenss
load("data/EconAct/Census.test.tidy.miss.Rda")
#### Train the model ####
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
dtrain <- xgb.DMatrix(train.predictors, label=train.outcome)

dtest <- xgb.DMatrix(test.predictors, missing=-999)
  
# Train a model using training set
trainEA_v1 <- xgboost(
  data = dtrain, max_depth = 2, eta = 1, nthread = 2, nrounds = 10,
  objective = "multi:softmax", num_class = 10, missing=-999
)

#### Test the model ####
predicted <- predict(trainEA_v1, dtest, missing = -999, na.action = na.pass)
# Save predicted values
save(predicted, file = "data/EconAct/XGBoost/predicted.RData")

#### Evaluate performance of model ####
# Compare versions of the outcome variable (Actual, Predicted, Missing)
actuals <- Census.test.tidy$econ.act

missing <- Census.test.tidy.miss$econ.act

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
) + scale_x_discrete(breaks = pretty_breaks()
) + scale_y_discrete(breaks = pretty_breaks())

#### Examine feature importance ####
importance_matrix <- xgb.importance(model = trainEA_v1)

print(importance_matrix)

xgb.plot.importance(importance_matrix = importance_matrix)

#### Plot the individual trees of the model ####
# Tree 1
xgb.plot.tree(model = trainEA_v1, tree=1)
# Tree 2
xgb.plot.tree(model = trainEA_v1, tree=2)
# Tree 3
xgb.plot.tree(model = trainEA_v1, tree=3)

#### Save model ####
xgb.save(trainEA_v1, "models/xgboost.econAct")

#### Load model ####
trainEA_v1 <- xgb.load("models/xgboost.econAct")

#### Impute values using CANCEIS ####
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

# Read in CANCEIS input and output
CANCEIS.test.in <- read.table("data/EconAct/CANCEIS/xxxUNIT01IG01.txt",
  header = FALSE,
  col.names = c(
    "canceis.id", "econ.act", "student",
    "industry", "age", "occupation",
    "social.grade"
  )
)[, -1]

CANCEIS.test.out <- read.table("data/EconAct/CANCEIS/XXXUNITIMP01IG01.txt",
  header = FALSE,
  col.names = c(
    "canceis.id", "econ.act", "student",
    "industry", "age", "occupation",
    "social.grade"
  )
)[, -1]

# Compare predicted and actuals
actuals.CANCEIS <- Census.test.tidy$econ.act

missing.CANCEIS <- CANCEIS.test.in$econ.act

predicted.CANCEIS <- CANCEIS.test.out$econ.act

compare_var_CANCEIS <- tibble(
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

#### Impute values using CANCEIS (with XGBoost to advise selection of MVs) ####
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

# Read in CANCEIS input and output
CANCEISXG.test.in <- read.table("data/EconAct/MixedMethods/xxxUNIT01IG01.txt",
                              header = FALSE,
                              col.names = c(
                                "canceis.id", "econ.act", "hours.cont", 
                                "age", "student",
                                "sex", "health", "industry"
                              )
)[, -1]

CANCEISXG.test.out <- read.table("data/EconAct/MixedMethods/XXXUNITIMP01IG01.txt",
                               header = FALSE,
                               col.names = c(
                                 "canceis.id", "econ.act", "hours.cont", 
                                 "age", "student",
                                 "sex", "health", "industry"
                               )
)[, -1]

# Compare predicted and actuals
actuals.CANCEISXG <- Census.test.tidy$econ.act

missing.CANCEISXG <- CANCEISXG.test.in$econ.act

predicted.CANCEISXG <- CANCEISXG.test.out$econ.act

compare_var_CANCEISXG <- tibble(
  Actuals = actuals.CANCEISXG, Predictions =
    predicted.CANCEISXG, Missing = missing.CANCEISXG
)

compare_missing_CANCEISXG <- compare_var_CANCEISXG[
  compare_var_CANCEISXG$Missing == -999, ]

compare_missing_CANCEISXG$indicator <- ifelse(
  compare_missing_CANCEISXG$Actuals ==
    compare_missing_CANCEISXG$Predictions,
  "Correct", "Wrong"
)

counts_CANCEISXG <- table(compare_missing_CANCEISXG$indicator)

barplot(counts_CANCEISXG, main = "Accuracy of predictions", xlab = "Outcome")

# Using Confusion Matrix to evaluate predictions
confusion_CANCEISXG <- confusionMatrix(
  as.factor(compare_missing_CANCEISXG$Actuals),
  as.factor(compare_missing_CANCEISXG$Predictions)
)

qplot(Actuals, Predictions,
      data = compare_missing_CANCEISXG, colour = Actuals,
      geom = c("jitter"), main = "predicted vs. observed in validation data",
      xlab = "Observed Class", ylab = "Predicted Class"
)

#### Impute values using mode imputation ####
# Create a vector of imputable variable excluding missing values
mode.dat <- Census.test.tidy.miss[
  Census.test.tidy.miss$econ.act != -999, ]

mode.val <- Mode(mode.dat$econ.act)

# Compare predicted and actuals
actuals.mode <- Census.test.tidy$econ.act

missing.mode <- Census.test.tidy.miss$econ.act

predicted.mode <- ifelse(
  Census.test.tidy.miss$econ.act == -999, mode.val, 
  Census.test.tidy.miss$econ.act)

compare_var_mode <- tibble(
  Actuals = actuals.mode, Predictions =
    predicted.mode, Missing = missing.mode
)

compare_missing_mode <- compare_var_mode[
  compare_var_mode$Missing == -999, ]

compare_missing_mode$indicator <- ifelse(
  compare_missing_mode$Actuals ==
    compare_missing_mode$Predictions,
  "Correct", "Wrong"
)

counts_mode <- table(compare_missing_mode$indicator)

barplot(counts_mode, main = "Accuracy of predictions", xlab = "Outcome")
