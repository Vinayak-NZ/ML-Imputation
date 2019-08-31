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
imp.var.train <- Census.train.full[, c("person.id", "social.grade")]
imp.var.train <- imp.var.train[order(imp.var.train$person.id),] 
Census.train <- Census.train[order(Census.train$person.id),]
Census.train.var <- cbind(Census.train, imp.var.train)

imp.var.test <- Census.test.full[, c("person.id", "social.grade")]
imp.var.test <- imp.var.test[order(imp.var.test$person.id),]
Census.test <- Census.test[order(Census.test$person.id),]
Census.test.var <- cbind(Census.test, imp.var.test)

drop_vars <- c("person.id", "hours.worked", "social.grade.NCR", "social.grade.1", "social.grade.2", "social.grade.3", "social.grade.4")

Census.train.tidy <- Census.train.var[!Census.train.var$social.grade.NCR==1, !(names(Census.train.var) %in% drop_vars)]

Census.test.tidy <- Census.test.var[!Census.test.var$social.grade.NCR==1, !(names(Census.test.var) %in% drop_vars)]

Census.train.tidy <- Census.train.tidy %>% mutate_if(is.character, as.numeric)

Census.test.tidy <- Census.test.tidy %>% mutate_if(is.character, as.numeric)

## ---- miss-soc-grad
# Simulate missingness in test data & Convert all missing responses to -999
set.seed(5)

Census.test.tidy.amp <- ampute(Census.test.tidy, prop = 0.7)

Census.test.tidy.miss <- Census.test.tidy.amp$amp

# Convert missing cases to -999
Census.test.tidy.miss[is.na(Census.test.tidy.miss)] <- -999

Census.test.tidy.miss <- ohe.miss(dat=Census.test.tidy.miss, varlist=
                                    c('region','resident.type','fam.comp',
                                      'marital.status','birth.country','health',
                                      'ethnicity','religion','occupation',
                                      'industry','econ.act'))

# Study the test dataset with missingness: How much missingness per variable?
NumberMissing <- sapply(Census.test.tidy.miss, function(y) sum(length(
  which((y)==-999))))

TestNumberMissing <- data.frame(NumberMissing)

# Save dataset with missingenss
save(Census.test.tidy.miss, file="data/SocialGrade/Census.test.tidy.miss.Rda")

## ---- train-soc-grad
# Train the model
# Convert the data to matrix and assign output variable
train.outcome <- as.numeric(Census.train.tidy$social.grade)

train.predictors <- sparse.model.matrix(social.grade ~ .,
                                        data = Census.train.tidy
)[, -1]

test.outcome <- as.numeric(Census.test.tidy.miss$social.grade)

test.predictors <- model.matrix(social.grade ~ .,
                                data = Census.test.tidy.miss
)[, -1]

# Convert the matrix objects to DMatrix objects  
dtrain <- xgb.DMatrix(train.predictors, label = as.numeric(train.outcome))

dtest <- xgb.DMatrix(test.predictors, label = as.numeric(test.outcome), missing=-999)

# Train a model using training set
trainSG_v1 <- xgboost(
  data = dtrain, max_depth = 3, eta = 0.1, nthread = 2, nrounds = 92,
  objective = "multi:softmax", gamma = 10, num_class = 5, colssample_bytree = 0.966,
  missing=-999
)

trainSG_v1 <- xgboost(
  data = dtrain, booster = "gbtree", max_depth = 8, eta = 0.1, nrounds = 1,
  objective = "multi:softmax", num_class = 5, colssample_bytree = 0.966,
  min_child_weight = 7.8, subsample = 0.877, missing=-999
)

trainSG_v1 <-xgboost(
  data = dtrain, nrounds = 1, booster = "gbtree", max_depth = "8", min_child_weight = "7.79734527645633", 
  subsample = "0.876703443238512", colsample_bytree = "0.965550040476955", 
  objective = "multi:softprob", num_class = "5", silent = "1" 
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