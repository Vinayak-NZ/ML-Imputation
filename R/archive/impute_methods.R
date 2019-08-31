## ---- study-econ-act
# Study the variable: How many units in each category?
EAt <- table(Census$econ.act)

EAt

# What is the distribution of the variable: Remove NCR and plot to look at distribution
g <- ggplot(Census[!Census$econ.act == -9, ], aes(econ.act))

g + geom_bar() + scale_x_discrete(
  name = "Economic Activity",
  breaks = pretty_breaks()
)

## ---- Build-XGBoost-econ-act
econAct <- trainDT(
  train = Census.train.label,
  test.orig = Census.test.label,
  test.miss = Census.SocialGrade.label,
  ident = 'person.id',
  cat = 1,
  var = 'social.grade',
  exc = -9,
  col_order = c("region", "residence.type", "fam.comp", "resident.type", "sex",
                "age", "marital.status", "student", "birth.country", "health", "ethnicity",
                "religion", "econ.act", "occupation", "industry", "social.grade", "hours.cont"),
  objective = "multi:softmax",
  max_depth = 2,
  eta = 1,
  nrounds = 100,
  num_class = 5
)

objective = "multi:softmax", num_class = 10,

# Examine quality metrics
compareMissing <- econAct[[6]]

counts <- table(compareMissing$indicator)

barplot(counts, main = "Accuracy of predictions", xlab = "Outcome")

confusionMatrix(
  factor(compareMissing$Actuals, levels = 1:9),
  factor(compareMissing$Predictions, levels = 1:9)
)

# Save model
trainEA_v1 <- econAct[[2]]

xgb.save(trainEA_v1, "models/XGBoost/xgboost.econAct")

# Save predicted values
predicted.econAct <- econAct[[5]]

save(predicted.econAct, file = "data/predicted/XGBoost/predicted.econAct.RData")

## ---- imp-econact-CANCEIS
# Impute values using CANCEIS
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

## ---- imp-econact-CANCEISXG
# Impute values using CANCEIS (with XGBoost to advise selection of MVs)
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