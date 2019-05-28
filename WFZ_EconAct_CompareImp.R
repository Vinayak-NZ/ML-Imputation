#### Load datasets ####
# Test and Training data
load("data/Census.train.Rda")
load("data/Census.test.Rda")

# Load dataset with missingenss
load("data/EconAct/Census.test.tidy.miss.Rda")

# Create test.tidy and train.tidy datasets (Remove units with NCR codes for variable 
# & Remove the personal identifier)
Census.train.tidy <- Census.train[!Census.train$econ.act == -9, c(-1,-17)]

Census.test.tidy <- Census.test[!Census.test$econ.act == -9, c(-1,-17)]

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

# Read in CANCEISXG input and output
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


# Load predicted values from XGBoost
load("data/EconAct/XGBoost/predicted.RData")

#### Load model ####
trainEA_v1 <- xgb.load("models/xgboost.econAct")

#### Evaluate performance of XGBoost model ####
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

#### Evaluate performance of CANCEIS ####
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

#### Evaluate performance of CANCEISXG ####
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
