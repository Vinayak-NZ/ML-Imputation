## ---- study-soc-grad
# Study the variable: How many units in each category?
SGt <- table(Census$social.grade)

SGt

# What is the distribution of the variable: Remove NCR and plot to look at distribution
g <- ggplot(Census[!Census$social.grade == -9, ], aes(social.grade))

g + geom_bar() + scale_x_discrete(name = "Social Grade",
                                  breaks = pretty_breaks())

## ---- tidy-soc-grad
# Tidy/treat the training and test datasets
# Remove units with NCR codes for variable & Remove the personal identifier
Census.train.tidy <- Census.train[!Census.train$social.grade == -9, c(-1,-17)]

Census.test.tidy <- Census.test[!Census.test$social.grade == -9, c(-1,-17)]

## ---- miss-soc-grad
# Simulate missingness in test data & Convert all missing responses to -999
Census.test.tidy.amp <- ampute(Census.test.tidy, prop = 0.7)

Census.test.tidy.miss <- Census.test.tidy.amp$amp

# Study the test dataset with missingness: How much missingness per variable?
NumberMissing <- sapply(Census.test.tidy.miss, function(y) sum(length(
  which(is.na(y)))))

TestNumberMissing <- data.frame(NumberMissing)

# Convert missing cases to -999
Census.test.tidy.miss[is.na(Census.test.tidy.miss)] <- -999

# Save dataset with missingenss
save(Census.test.tidy.miss, file="data/SocialGrade/Census.test.tidy.miss.Rda")

## ---- train-soc-grad
# Train the model
# Convert the data to matrix and assign output variable
train.outcome <- Census.train.tidy$social.grade

train.predictors <- sparse.model.matrix(social.grade ~ .,
                                        data = Census.train.tidy
)[, -1]

test.outcome <- Census.test.tidy.miss$social.grade

test.predictors <- model.matrix(social.grade ~ .,
                                data = Census.test.tidy.miss
)[, -1]

# Convert the matrix objects to DMatrix objects  
dtrain <- xgb.DMatrix(train.predictors, label=train.outcome)

dtest <- xgb.DMatrix(test.predictors, missing=-999)

# Train a model using training set
trainSG_v1 <- xgboost(
  data = dtrain, max_depth = 2, eta = 1, nthread = 2, nrounds = 10,
  objective = "multi:softmax", num_class = 5, missing=-999
)

# Examine feature importance
importance_matrix <- xgb.importance(model = trainSG_v1)

print(importance_matrix)

xgb.plot.importance(importance_matrix = importance_matrix)

# Save model
xgb.save(trainSG_v1, "XGBoost/xgboost.socialGrade")

## ---- test-soc-grad
#### Test the model ####
predicted <- predict(trainSG_v1, dtest, missing = -999, na.action = na.pass)

# Save predicted values
save(predicted, file = "data/SocialGrade/XGBoost/predicted.RData")

## ---- imp-socgrad-CANCEIS
# Impute values using CANCEIS
# Create CANCEIS input file with imputable and matching variables
CANCEIS.input <- Census.test.tidy.miss[, c(
  "social.grade", "industry", "occupation",
  "econ.act", "student", "age"
)]

CANCEIS.input$canceis.id <- 1:nrow(CANCEIS.input)

CANCEIS.input <- CANCEIS.input[, c(
  "canceis.id", "social.grade", "industry", "occupation",
  "econ.act", "student", "age"
)]

write.table(CANCEIS.input,
            file = "data/SocialGrade/CANCEIS/xxxUNIT01IG01.txt", sep = "\t",
            row.names = FALSE, col.names = FALSE
)

## ---- imp-socgrad-CANCEISXG
# Impute values using CANCEIS (with XGBoost to advise selection of MVs)
# Create CANCEIS input file with imputable and matching variables
CANCEISXG.input <- Census.test.tidy.miss[, c(
  "social.grade", "occupation", "student", 
  "industry", "hours.cont", "marital.status", 
  "econ.act"
)]

CANCEISXG.input$canceis.id <- 1:nrow(CANCEISXG.input)

CANCEISXG.input <- CANCEISXG.input[, c(
  "canceis.id", "social.grade", "occupation", "student", 
  "industry", "hours.cont", "marital.status", 
  "econ.act"
)]

write.table(CANCEISXG.input,
            file = "data/SocialGrade/MixedMethods/xxxUNIT01IG01.txt", sep = "\t",
            row.names = FALSE, col.names = FALSE
)