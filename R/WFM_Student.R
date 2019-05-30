## ---- study-student
# Study the variable: How many units in each category?
St <- table(Census$student)

St
# What is the distribution of the variable: Remove NCR and plot to look at distribution
g <- ggplot(Census[!Census$student == -9, ], aes(student))

g + geom_bar() + scale_x_discrete(name = "Student status",
  breaks = pretty_breaks())

## ---- tidy-student
# Tidy/treat the training and test datasets
# Remove units with NCR codes for variable & Remove the personal identifier
Census.train.tidy <- Census.train[!Census.train$student == -9, c(-1,-17)]

Census.test.tidy <- Census.test[!Census.test$student == -9, c(-1,-17)]

## ---- miss-student
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
save(Census.test.tidy.miss, file="data/Student/Census.test.tidy.miss.Rda")

## ---- train-student
# Train the model
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

# Examine feature importance
importance_matrix <- xgb.importance(model = trainS_v1)

print(importance_matrix)

xgb.plot.importance(importance_matrix = importance_matrix)

# Save model
xgb.save(trainS_v1, "XGBoost/xgboost.student")

## ---- test-student
# Test the model
predicted <- ifelse(
  predict(trainS_v1, dtest, missing = -999, na.action = na.pass) > 0.5, 1, 0)

# Save predicted values
save(predicted, file = "data/Student/XGBoost/predicted.RData")

## ---- imp-student-CANCEIS
# Impute values using CANCEIS
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
            file = "data/Student/CANCEIS/xxxUNIT01IG01.txt", sep = "\t",
            row.names = FALSE, col.names = FALSE
)

## ---- imp-student-CANCEISXG
# Impute values using CANCEIS (with XGBoost to advise selection of MVs)
# Create CANCEIS input file with imputable and matching variables
CANCEISXG.input <- Census.test.tidy.miss[, c(
  "student", "age", "econ.act", "social.grade",
  "occupation", "birth.country", "fam.comp"
)]

CANCEISXG.input$canceis.id <- 1:nrow(CANCEISXG.input)

CANCEISXG.input <- CANCEISXG.input[, c(
  "canceis.id", "student", "age", "econ.act", "social.grade",
  "occupation", "birth.country", "fam.comp"
)]

write.table(CANCEISXG.input,
            file = "data/Student/MixedMethods/xxxUNIT01IG01.txt", sep = "\t",
            row.names = FALSE, col.names = FALSE
)