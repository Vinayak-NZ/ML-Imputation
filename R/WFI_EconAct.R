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
  test.miss = Census.EconAct.label,
  ident = 'person.id',
  cat = 1,
  var = 'econ.act',
  exc = -9,
  col_order = c("region", "residence.type", "fam.comp", "resident.type", "sex",
                "age", "marital.status", "student", "birth.country", "health", "ethnicity",
                "religion", "econ.act", "occupation", "industry", "social.grade", "hours.cont"),
  objective = "multi:softmax",
  max_depth = 5,
  eta = 0.3,
  nrounds = 30,
  num_class = 10
)

## ---- eval-econ-actXG
# Save feature importance plot
png(filename="images/featuresXG_econAct.png")
xgb.plot.importance(importance_matrix = econAct[[3]])
dev.off()

# Examine quality metrics
compareMissing <- econAct[[7]]

counts <- table(compareMissing$indicator)

counts.melt <- melt(counts)

counts.melt$Outcome <- counts.melt$Var1

accuracy_plot <- ggplot(data=counts.melt, aes(x=Outcome, y=value)) +
  geom_bar(stat="identity") + ggtitle("Accuracy of Predictions: XGBoost")

accuracy_plot

ggsave("images/accuracyXG_econAct.png")

confusion_XGBoost <- confusionMatrix(
  factor(compareMissing$Actuals, levels = 1:9),
  factor(compareMissing$Predictions, levels = 1:9)
)

qplot(Actuals, Predictions,
      data = compareMissing,
      geom = c("jitter"), main = "XGBoost: predicted vs. observed in test data",
      xlab = "Observed Class", ylab = "Predicted Class"
) + scale_x_discrete(limits=c("1","2","3","4","5","6","7","8","9")
) + scale_y_discrete(limits=c("1","2","3","4","5","6","7","8","9"))

ggsave("images/EAXGBoostqplot.png")

# What is the distribution of the variable before and after imputation
Compare_val <- econAct[[6]]

actuals <- ggplot(econAct[[6]], aes(Actuals))

actuals + geom_bar() + ggtitle("True Distribution") + 
  scale_x_discrete(
    name = "Economic Activity",
    breaks = pretty_breaks()
  )

ggsave("images/actuals_socialGrade.png")

actuals <- table(Compare_val$Actuals)

actuals <- as.data.frame(actuals)

actuals <- plyr::rename(actuals, c(
  "Var1" = "econ.act",
  "Freq" = "actuals"))

imputed <- table(Compare_val$PI)

imputed <- as.data.frame(imputed)

imputed <- plyr::rename(imputed, c(
  "Var1" = "econ.act",
  "Freq" = "imputed"))

plot.dat <- merge(actuals, imputed, by = "econ.act")

plot.dat.melt <- melt(plot.dat, id="econ.act")

plot.dat.melt <- plyr::rename(plot.dat.melt, c(
  "variable" = "output"))

predictions <- ggplot(plot.dat.melt, aes(x=econ.act,y=value,fill=output)) + 
  geom_bar(stat="identity", position = "identity", alpha=.5)

predictions + ggtitle("Post Imputation: XGBoost") +
  scale_x_discrete(
    name = "Economic Activity", 
    breaks = pretty_breaks()
  )

ggsave("images/PIXG_econAct.png")

# Save model
trainEA_v1 <- econAct[[2]]

xgb.save(trainEA_v1, "models/XGBoost/xgboost.econAct")

# Save predicted values
predicted.econAct <- econAct[[5]]

save(predicted.econAct, file = "data/predicted/XGBoost/predicted.econAct.RData")


## ---- CANCEIS-input-econ-act
econAct.CANCEIS <- CANCEIS.in(
  dat.cor=Census,
  dat.miss=Census.EconAct.label,
  var='econ.act',
  exc=-9,
  varlist=c("region", "residence.type", "fam.comp", "resident.type", "sex",
            "age", "marital.status", "student", "birth.country", "health", "ethnicity",
            "religion", "econ.act", "occupation", "industry", "social.grade", "hours.worked"))

write.table(econAct.CANCEIS[[2]],
            file = "data/CANCEIS/EconAct/xxxUNIT01IG01.txt", sep = "\t",
            row.names = FALSE, col.names = FALSE
)

## ---- CANCEISXG-input-econ-act
# Impute values using CANCEIS (with XGBoost to advise selection of MVs)
# Create CANCEIS input file with imputable and matching variables
econAct.CANCEISXG <- CANCEISXG(
  dat.miss = Census.EconAct.label, 
  var = 'econ.act', 
  featureList = econAct[[3]])

write.table(econAct.CANCEISXG,
            file = "data/CANCEISXG/EconAct/xxxUNIT01IG01.txt", sep = "\t",
            row.names = FALSE, col.names = FALSE
)

## ---- Evaluate-CANCEIS-econ-act
# Load CANCEIS input and output files
econAct.CANCEIS.load <- loadCANCEIS(
  var='EconAct',
  varlist=names(econAct.CANCEIS[[2]]),
  xg=0)

eval.econAct.CANCEIS <- evalCANCEIS(cat=1,
                                    var = 'econ.act', 
                                    exc = -9, 
                                    actuals = Census.test.label, 
                                    missing = econAct.CANCEIS.load[[1]], 
                                    predicted = econAct.CANCEIS.load[[2]])

# Compare predicted and actuals
counts <- eval.econAct.CANCEIS[[3]]

counts.melt <- melt(counts)

counts.melt$Outcome <- counts.melt$Var1

accuracy_plot <- ggplot(data=counts.melt, aes(x=Outcome, y=value)) +
  geom_bar(stat="identity") + ggtitle("Accuracy of Predictions: CANCEIS")

accuracy_plot

ggsave("images/accuracyCANCEIS_econAct.png")

# Using Confusion Matrix to evaluate predictions
compareMissing_CANCEIS <- eval.econAct.CANCEIS[[2]]

confusion_CANCEIS <- confusionMatrix(
  factor(compareMissing_CANCEIS$Actuals[[1]], levels = 1:9),
  factor(compareMissing_CANCEIS$Predictions, levels = 1:9)
)

qplot(Actuals[[1]], Predictions,
      data = compareMissing_CANCEIS,
      geom = c("jitter"), main = "CANCEIS: predicted vs. observed in test data",
      xlab = "Observed Class", ylab = "Predicted Class"
) + scale_x_discrete(limits=c("1","2","3","4","5","6","7","8","9")
) + scale_y_discrete(limits=c("1","2","3","4","5","6","7","8","9"))

ggsave("images/EACANCEISqplot.png")

# What is the distribution of the variable after imputation?
Compare_val <- eval.econAct.CANCEIS[[1]]

actuals <- table(Compare_val$Actuals)

actuals <- as.data.frame(actuals)

actuals <- plyr::rename(actuals, c(
  "Var1" = "econ.act",
  "Freq" = "actuals"))

imputed <- table(Compare_val$Predictions)

imputed <- as.data.frame(imputed)

imputed <- plyr::rename(imputed, c(
  "Var1" = "econ.act",
  "Freq" = "imputed"))

plot.dat <- merge(actuals, imputed, by = "econ.act")

plot.dat.melt <- melt(plot.dat, id="econ.act")

plot.dat.melt <- plyr::rename(plot.dat.melt, c(
  "variable" = "output"))

predictions <- ggplot(plot.dat.melt, aes(x=econ.act,y=value,fill=output)) + 
  geom_bar(stat="identity", position = "identity", alpha=.5)

predictions + ggtitle("Post Imputation: CANCEIS") +
  scale_x_discrete(
    name = "Economic Activity", 
    breaks = pretty_breaks()
  )

ggsave("images/PICANCEIS_econAct.png")

## ---- Evaluate-CANCEISXG-econ-act
# Load CANCEIS input and output files
econAct.CANCEISXG.load <- loadCANCEIS(
  var='EconAct',
  varlist=names(econAct.CANCEISXG),
  xg=1)

eval.econAct.CANCEISXG <- evalCANCEIS(
                                    cat =1,
                                    var = 'econ.act', 
                                    exc = -9, 
                                    actuals = Census.test.label, 
                                    missing = econAct.CANCEISXG.load[[1]], 
                                    predicted = econAct.CANCEISXG.load[[2]])

# Compare predicted and actuals
counts <- eval.econAct.CANCEISXG[[3]]

counts.melt <- melt(counts)

counts.melt$Outcome <- counts.melt$Var1

accuracy_plot <- ggplot(data=counts.melt, aes(x=Outcome, y=value)) +
  geom_bar(stat="identity") + ggtitle("Accuracy of Predictions: CANCEISXG")

accuracy_plot

ggsave("images/accuracyCANCEISXG_econAct.png")

# Using Confusion Matrix to evaluate predictions
compareMissing_CANCEISXG <- eval.econAct.CANCEISXG[[2]]

confusion_CANCEISXG <- confusionMatrix(
  factor(compareMissing_CANCEISXG$Actuals[[1]], levels = 1:9),
  factor(compareMissing_CANCEISXG$Predictions, levels = 1:9)
)

qplot(Actuals[[1]], Predictions,
      data = compareMissing_CANCEISXG,
      geom = c("jitter"), main = "predicted vs. observed in test data",
      xlab = "Observed Class", ylab = "Predicted Class"
) + scale_x_discrete(limits=c("1","2","3","4","5","6","7","8","9")
) + scale_y_discrete(limits=c("1","2","3","4","5","6","7","8","9"))

ggsave("images/EACANCEISXGqplot.png")

# What is the distribution of the variable after imputation?
Compare_val <- eval.econAct.CANCEISXG[[1]]

actuals <- table(Compare_val$Actuals[[1]])

actuals <- as.data.frame(actuals)

actuals <- plyr::rename(actuals, c(
  "Var1" = "econ.act",
  "Freq" = "actuals"))

imputed <- table(Compare_val$Predictions)

imputed <- as.data.frame(imputed)

imputed <- plyr::rename(imputed, c(
  "Var1" = "econ.act",
  "Freq" = "imputed"))

plot.dat <- merge(actuals, imputed, by = "econ.act")

plot.dat.melt <- melt(plot.dat, id="econ.act")

plot.dat.melt <- plyr::rename(plot.dat.melt, c(
  "variable" = "output"))

predictions <- ggplot(plot.dat.melt, aes(x=econ.act,y=value,fill=output)) + 
  geom_bar(stat="identity", position = "identity", alpha=.5)

predictions + ggtitle("Post Imputation: CANCEISXG") +
  scale_x_discrete(
    name = "Economic Activity", 
    breaks = pretty_breaks()
  )

ggsave("images/PICANCEISXG_econAct.png")

## ---- Evaluate-mode/median-econ-act
# Compare predicted and actuals
econAct.modmed <- modmedImp(
  cat = 1,
  var = 'econ.act',
  exc = -9,
  actuals = Census.test.label,
  missing = Census.EconAct.label,
  mod=1)

# Compare predicted and actuals
counts <- econAct.modmed[[3]]

counts.melt <- melt(counts)

counts.melt$Outcome <- counts.melt$Var1

accuracy_plot <- ggplot(data=counts.melt, aes(x=Outcome, y=value)) +
  geom_bar(stat="identity") + ggtitle("Accuracy of Predictions")

accuracy_plot

ggsave("images/accuracyModMed_econAct.png")

# What is the distribution of the variable after imputation?
Compare_val <- econAct.modmed[[1]]

actuals <- table(Compare_val$Actuals[[1]])

actuals <- as.data.frame(actuals)

actuals <- plyr::rename(actuals, c(
  "Var1" = "econ.act",
  "Freq" = "actuals"))

imputed <- table(Compare_val$Predictions)

imputed <- as.data.frame(imputed)

imputed <- plyr::rename(imputed, c(
  "Var1" = "econ.act",
  "Freq" = "imputed"))

plot.dat <- merge(actuals, imputed, by = "econ.act")

plot.dat.melt <- melt(plot.dat, id="econ.act")

plot.dat.melt <- plyr::rename(plot.dat.melt, c(
  "variable" = "output"))

predictions <- ggplot(plot.dat.melt, aes(x=econ.act,y=value,fill=output)) + 
  geom_bar(stat="identity", position = "identity", alpha=.5)

predictions + ggtitle("Post Imputation: Mode") +
  scale_x_discrete(
    name = "Economic Activity", 
    breaks = pretty_breaks()
  )

ggsave("images/PIMode_econAct.png")

## ---- compare-econ-act
XGBoost <- round(confusion_XGBoost$overall[c('Accuracy','Kappa')],2)

CANCEIS <- round(confusion_CANCEIS$overall[c('Accuracy','Kappa')],2)

MixedMethods <- round(confusion_CANCEISXG$overall[c('Accuracy','Kappa')],2)

Mode <- c(round(econAct.modmed[[3]][['Correct']]/(econAct.modmed[[3]][['Correct']]+econAct.modmed[[3]][['Wrong']]),2), NA)

CompareEconAct <- cbind(XGBoost, CANCEIS, MixedMethods, Mode)

save(CompareEconAct, file = "data/output/CompareEconAct.RData")
