#### Load datasets ####
# Test and Training data
load("data/Census.train.Rda")
load("data/Census.test.Rda")

# Load dataset with missingenss
load("data/HoursCont/Census.test.tidy.miss.Rda")

# Create test.tidy and train.tidy datasets (Remove units with NCR codes for variable 
# & Remove the personal identifier)
Census.train.tidy <- Census.train[!Census.train$hours.cont == -9, c(-1,-17)]

Census.test.tidy <- Census.test[!Census.test$hours.cont == -9, c(-1,-17)]

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

# Load predicted values from XGBoost
load("data/HoursCont/XGBoost/predicted.RData")

#### Load model ####
trainHC_v1 <- xgb.load("XGBoost/xgboost.hoursCont")

#### Evaluate performance of XGBoost model ####
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

#### Evaluate performance of CANCEIS ####
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

#### Evaluate performance of CANCEISXG ####
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

compare_missing_median <- compare_var_median[
  compare_var_median$Missing == -999, ]

# Using Mean Absolute Error and Root Mean Square error to evaluate predictions
MAE(compare_missing_median$Predictions, compare_missing_median$Actuals)

RMSE(compare_missing_median$Predictions, compare_missing_median$Actuals)
