## ---- load-student
# Load datasets
# Test and Training data
load("data/Census.train.Rda")

load("data/Census.test.Rda")

# Load dataset with missingenss
load("data/Student/Census.test.tidy.miss.Rda")

# Create test.tidy and train.tidy datasets (Remove units with NCR codes for variable 
# & Remove the personal identifier)
Census.train.tidy <- Census.train[!Census.train$student == -9, c(-1,-17)]

Census.test.tidy <- Census.test[!Census.test$student == -9, c(-1,-17)]

# Read in CANCEIS input and output
CANCEIS.test.in <- read.table("data/Student/CANCEIS/xxxUNIT01IG01.txt",
                              header = FALSE,
                              col.names = c(
                                "canceis.id", "student", "social.grade",
                                "occupation", "industry", "age", "econ.act"
                              )
)[, -1]

CANCEIS.test.out <- read.table("data/Student/CANCEIS/XXXUNITIMP01IG01.txt",
                               header = FALSE,
                               col.names = c(
                                 "canceis.id", "student", "social.grade",
                                 "occupation", "industry", "age", "econ.act"
                               )
)[, -1]

# Read in CANCEISXG input and output
CANCEISXG.test.in <- read.table("data/Student/MixedMethods/xxxUNIT01IG01.txt",
                                header = FALSE,
                                col.names = c(
                                  "canceis.id", "student", "age", "econ.act", "social.grade",
                                  "occupation", "birth.country", "fam.comp"
                                )
)[, -1]

CANCEISXG.test.out <- read.table("data/Student/MixedMethods/XXXUNITIMP01IG01.txt",
                                 header = FALSE,
                                 col.names = c(
                                   "canceis.id", "student", "age", "econ.act", "social.grade",
                                   "occupation", "birth.country", "fam.comp"
                                 )
)[, -1]


# Load predicted values from XGBoost
load("data/Student/XGBoost/predicted.RData")

# Load model
trainS_v1 <- xgb.load("XGBoost/xgboost.student")

## ---- eval-student
# Evaluate performance of XGBoost model
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

compareVarP <- compareVar

compareVarP$Actuals <- ifelse(compareVarP$Actuals == 0, "Yes", "No")

compareVarP$Predictions <- ifelse(compareVarP$Predictions == 0, "Yes", "No")

qplot(Actuals, Predictions,
      data = compareVarP,
      geom = c("jitter"), main = "predicted vs. observed in test data",
      xlab = "Observed Class", ylab = "Predicted Class"
) + scale_x_discrete(labels = c("Yes", "No")
) + scale_y_discrete(labels = c("Yes", "No"))

ggsave("images/STXGqplot.png")

# Evaluate performance of CANCEIS
# Compare predicted and actuals
actuals.CANCEIS <- Census.test.tidy$student

missing.CANCEIS <- CANCEIS.test.in$student

predicted.CANCEIS <- CANCEIS.test.out$student

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

compare_missing_CANCEISP <- compare_missing_CANCEIS

compare_missing_CANCEISP$Actuals <- ifelse(compare_missing_CANCEISP$Actuals == 0, "Yes", "No")

compare_missing_CANCEISP$Predictions <- ifelse(compare_missing_CANCEISP$Predictions == 0, "Yes", "No")

qplot(Actuals, Predictions,
      data = compare_missing_CANCEISP,
      geom = c("jitter"), main = "predicted vs. observed in validation data",
      xlab = "Observed Class", ylab = "Predicted Class"
) + scale_x_discrete(labels=c("Yes","No")
) + scale_y_discrete(labels=c("Yes","No"))

ggsave("images/STCANCEISqplot.png")

# Evaluate performance of CANCEISXG
# Compare predicted and actuals
actuals.CANCEISXG <- Census.test.tidy$student

missing.CANCEISXG <- CANCEISXG.test.in$student

predicted.CANCEISXG <- CANCEISXG.test.out$student

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

compare_missing_CANCEISXGP <- compare_missing_CANCEISXG

compare_missing_CANCEISXGP$Actuals <- ifelse(compare_missing_CANCEISXGP$Actuals == 0, "Yes", "No")

compare_missing_CANCEISXGP$Predictions <- ifelse(compare_missing_CANCEISXGP$Predictions == 0, "Yes", "No")

qplot(Actuals, Predictions,
      data = compare_missing_CANCEISXGP,
      geom = c("jitter"), main = "predicted vs. observed in validation data",
      xlab = "Observed Class", ylab = "Predicted Class"
) + scale_x_discrete(labels=c("Yes","No")
) + scale_y_discrete(labels=c("Yes","No"))

ggsave("images/STCANCEISXGqplot.png")

# Impute values using mode imputation
# Create a vector of imputable variable excluding missing values
mode.dat <- Census.test.tidy.miss[
  Census.test.tidy.miss$student != -999, ]

mode.val <- Mode(mode.dat$student)

# Compare predicted and actuals
actuals.mode <- Census.test.tidy$student

missing.mode <- Census.test.tidy.miss$student

predicted.mode <- ifelse(
  Census.test.tidy.miss$student == -999, mode.val, 
  Census.test.tidy.miss$student)

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

## ---- compare-student
XGBoost <- confusionML$overall[c('Accuracy','Kappa')]

CANCEIS <- confusion_CANCEIS$overall[c('Accuracy','Kappa')]

MixedMethods <- confusion_CANCEISXG$overall[c('Accuracy','Kappa')]

Mode <- c(counts_mode[['Correct']]/(counts_mode[['Correct']]+counts_mode[['Wrong']]), NA)

CompareStudent <- cbind(XGBoost, CANCEIS, MixedMethods, Mode)

save(CompareStudent, file = "data/Student/CompareStudent.RData")
