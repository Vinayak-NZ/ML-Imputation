## ---- study-soc-grad
# Study the variable: How many units in each category?
SGt <- table(Census$social.grade)

SGt

# What is the distribution of the variable: Remove NCR and plot to look at distribution
g <- ggplot(Census[!Census$social.grade == -9, ], aes(social.grade))

g + geom_bar() + scale_x_discrete(
  name = "Social Grade",
  breaks = pretty_breaks())


## ---- Build-XGBoost-econ-act
socialGrade <- trainDT(
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
  max_depth = 5,
  eta = 0.3,
  nrounds = 30,
  num_class = 5
)

# Save feature importance plot
png(filename="images/featuresXG_socialGrade.png")
xgb.plot.importance(importance_matrix = socialGrade[[3]])
dev.off()

# Examine quality metrics
compareMissing <- socialGrade[[7]]

counts <- table(compareMissing$indicator)

counts.melt <- melt(counts)

counts.melt$Outcome <- counts.melt$Var1

accuracy_plot <- ggplot(data=counts.melt, aes(x=Outcome, y=value)) +
  geom_bar(stat="identity") + ggtitle("Accuracy of Predictions: XGBoost")

accuracy_plot

ggsave("images/accuracyXG_socialGrade.png")

confusion_XGBoost <- confusionMatrix(
  factor(compareMissing$Actuals, levels = 1:4),
  factor(compareMissing$Predictions, levels = 1:4)
)

qplot(Actuals, Predictions,
      data = compareMissing,
      geom = c("jitter"), main = "predicted vs. observed in test data",
      xlab = "Observed Class", ylab = "Predicted Class"
) + scale_x_discrete(limits=c("1","2","3","4")
) + scale_y_discrete(limits=c("1","2","3","4"))

ggsave("images/SGXGBoostqplot.png")

# What is the distribution of the variable before and after imputation
Compare_val <- socialGrade[[6]]

actuals <- ggplot(socialGrade[[6]], aes(Actuals))

actuals + geom_bar() + ggtitle("True Distribution") + 
  scale_x_discrete(
    name = "Social Grade",
    breaks = pretty_breaks()
  )

ggsave("images/actuals_socialGrade.png")

actuals <- table(Compare_val$Actuals)

actuals <- as.data.frame(actuals)

actuals <- plyr::rename(actuals, c(
  "Var1" = "social.grade",
  "Freq" = "actuals"))

imputed <- table(Compare_val$PI)

imputed <- as.data.frame(imputed)

imputed <- plyr::rename(imputed, c(
  "Var1" = "social.grade",
  "Freq" = "imputed"))

plot.dat <- merge(actuals, imputed, by = "social.grade")

plot.dat.melt <- melt(plot.dat, id="social.grade")

plot.dat.melt <- plyr::rename(plot.dat.melt, c(
  "variable" = "output"))

predictions <- ggplot(plot.dat.melt, aes(x=social.grade,y=value,fill=output)) + 
  geom_bar(stat="identity", position = "identity", alpha=.5)

predictions + ggtitle("Post Imputation: XGBoost") +
  scale_x_discrete(
    name = "Social Grade", 
    breaks = pretty_breaks()
  )  + coord_cartesian(ylim=c(0,35000))

ggsave("images/PIXG_socialGrade.png")

# Save model
trainSG_v1 <- socialGrade[[2]]

xgb.save(trainSG_v1, "models/XGBoost/xgboost.socialGrade")

# Save predicted values
predicted.socialGrade <- socialGrade[[5]]

save(predicted.socialGrade, file = "data/predicted/XGBoost/predicted.socialGrade.RData")

## ---- CANCEIS-input-econ-act
socialGrade.CANCEIS <- CANCEIS.in(
  dat.cor=Census,
  dat.miss=Census.SocialGrade.label,
  var='social.grade',
  exc=-9,
  varlist=c("region", "residence.type", "fam.comp", "resident.type", "sex",
            "age", "marital.status", "student", "birth.country", "health", "ethnicity",
            "religion", "econ.act", "occupation", "industry", "social.grade", "hours.worked"))

write.table(socialGrade.CANCEIS[[2]],
            file = "data/CANCEIS/SocialGrade/xxxUNIT01IG01.txt", sep = "\t",
            row.names = FALSE, col.names = FALSE
)

## ---- CANCEISXG-input-econ-act
# Impute values using CANCEIS (with XGBoost to advise selection of MVs)
# Create CANCEIS input file with imputable and matching variables
socialGrade.CANCEISXG <- CANCEISXG(
  dat.miss = Census.SocialGrade.label, 
  var = 'social.grade', 
  featureList = socialGrade[[3]])

write.table(socialGrade.CANCEISXG,
            file = "data/CANCEISXG/SocialGrade/xxxUNIT01IG01.txt", sep = "\t",
            row.names = FALSE, col.names = FALSE
)

## ---- Evaluate-CANCEIS-econ-act
# Load CANCEIS input and output files
socialGrade.CANCEIS.load <- loadCANCEIS(
  var='SocialGrade',
  varlist=names(socialGrade.CANCEIS[[2]]),
  xg=0)

eval.socialGrade.CANCEIS <- evalCANCEIS(
                                    cat = 1,
                                    var = 'social.grade', 
                                    exc = -9, 
                                    actuals = Census.test.label, 
                                    missing = socialGrade.CANCEIS.load[[1]], 
                                    predicted = socialGrade.CANCEIS.load[[2]])

# Compare predicted and actuals
counts <- eval.socialGrade.CANCEIS[[3]]

counts.melt <- melt(counts)

counts.melt$Outcome <- counts.melt$Var1

accuracy_plot <- ggplot(data=counts.melt, aes(x=Outcome, y=value)) +
  geom_bar(stat="identity") + ggtitle("Accuracy of Predictions: CANCEIS")

accuracy_plot

ggsave("images/accuracyCANCEIS_socialGrade.png")

# Using Confusion Matrix to evaluate predictions
compareMissing_CANCEIS <- eval.socialGrade.CANCEIS[[2]]

confusion_CANCEIS <- confusionMatrix(
  factor(compareMissing_CANCEIS$Actuals[[1]], levels = 1:4),
  factor(compareMissing_CANCEIS$Predictions, levels = 1:4)
)

qplot(Actuals[[1]], Predictions,
      data = compareMissing_CANCEIS,
      geom = c("jitter"), main = "predicted vs. observed in test data",
      xlab = "Observed Class", ylab = "Predicted Class"
) + scale_x_discrete(limits=c("1","2","3","4")
) + scale_y_discrete(limits=c("1","2","3","4"))

ggsave("images/SGCANCEISqplot.png")

# What is the distribution of the variable after imputation?
Compare_val <- eval.socialGrade.CANCEIS[[1]]

actuals <- table(Compare_val$Actuals)

actuals <- as.data.frame(actuals)

actuals <- plyr::rename(actuals, c(
  "Var1" = "social.grade",
  "Freq" = "actuals"))

imputed <- table(Compare_val$Predictions)

imputed <- as.data.frame(imputed)

imputed <- plyr::rename(imputed, c(
  "Var1" = "social.grade",
  "Freq" = "imputed"))

plot.dat <- merge(actuals, imputed, by = "social.grade")

plot.dat.melt <- melt(plot.dat, id="social.grade")

plot.dat.melt <- plyr::rename(plot.dat.melt, c(
  "variable" = "output"))

predictions <- ggplot(plot.dat.melt, aes(x=social.grade,y=value,fill=output)) + 
  geom_bar(stat="identity", position = "identity", alpha=.5)

predictions + ggtitle("Post Imputation: CANCEIS") +
  scale_x_discrete(
    name = "Social Grade", 
    breaks = pretty_breaks()
  ) + coord_cartesian(ylim=c(0,35000))

ggsave("images/PICANCEIS_socialGrade.png")

## ---- Evaluate-CANCEISXG-econ-act
# Load CANCEIS input and output files
socialGrade.CANCEISXG.load <- loadCANCEIS(
  var='SocialGrade',
  varlist=names(socialGrade.CANCEISXG),
  xg=1)

eval.socialGrade.CANCEISXG <- evalCANCEIS(
                                      cat = 1,
                                      var = 'social.grade', 
                                      exc = -9, 
                                      actuals = Census.test.label, 
                                      missing = socialGrade.CANCEISXG.load[[1]], 
                                      predicted = socialGrade.CANCEISXG.load[[2]])

# Compare predicted and actuals
counts <- eval.socialGrade.CANCEISXG[[3]]

counts.melt <- melt(counts)

counts.melt$Outcome <- counts.melt$Var1

accuracy_plot <- ggplot(data=counts.melt, aes(x=Outcome, y=value)) +
  geom_bar(stat="identity") + ggtitle("Accuracy of Predictions: CANCEISXG")

accuracy_plot

ggsave("images/accuracyCANCEISXG_socialGrade.png")

# Using Confusion Matrix to evaluate predictions
compareMissing_CANCEISXG <- eval.socialGrade.CANCEISXG[[2]]

confusion_CANCEISXG <- confusionMatrix(
  factor(compareMissing_CANCEISXG$Actuals[[1]], levels = 1:4),
  factor(compareMissing_CANCEISXG$Predictions, levels = 1:4)
)

qplot(Actuals[[1]], Predictions,
      data = compareMissing_CANCEISXG,
      geom = c("jitter"), main = "predicted vs. observed in validation data",
      xlab = "Observed Class", ylab = "Predicted Class"
) + scale_x_discrete(limits=c("1","2","3","4")
) + scale_y_discrete(limits=c("1","2","3","4"))

ggsave("images/SGCANCEISXGqplot.png")

# What is the distribution of the variable after imputation?
Compare_val <- eval.socialGrade.CANCEISXG[[1]]

actuals <- table(Compare_val$Actuals[[1]])

actuals <- as.data.frame(actuals)

actuals <- plyr::rename(actuals, c(
  "Var1" = "social.grade",
  "Freq" = "actuals"))

imputed <- table(Compare_val$Predictions)

imputed <- as.data.frame(imputed)

imputed <- plyr::rename(imputed, c(
  "Var1" = "social.grade",
  "Freq" = "imputed"))

plot.dat <- merge(actuals, imputed, by = "social.grade")

plot.dat.melt <- melt(plot.dat, id="social.grade")

plot.dat.melt <- plyr::rename(plot.dat.melt, c(
  "variable" = "output"))

predictions <- ggplot(plot.dat.melt, aes(x=social.grade,y=value,fill=output)) + 
  geom_bar(stat="identity", position = "identity", alpha=.5)

predictions + ggtitle("Post Imputation: CANCEISXG") +
  scale_x_discrete(
    name = "Social Grade", 
    breaks = pretty_breaks()
  )  + coord_cartesian(ylim=c(0,35000))

ggsave("images/PICANCEISXG_socialGrade.png")

## ---- Evaluate-mode/median-econ-act
# Compare predicted and actuals
socialGrade.modmed <- modmedImp(
  cat = 1,
  var = 'social.grade',
  exc = -9,
  actuals = Census.test.label,
  missing = Census.SocialGrade.label,
  mod=1)

# Compare predicted and actuals
counts <- socialGrade.modmed[[3]]

counts.melt <- melt(counts)

counts.melt$Outcome <- counts.melt$Var1

accuracy_plot <- ggplot(data=counts.melt, aes(x=Outcome, y=value)) +
  geom_bar(stat="identity") + ggtitle("Accuracy of Predictions")

accuracy_plot

ggsave("images/accuracyModMed_socialGrade.png")

# What is the distribution of the variable after imputation?
Compare_val <- socialGrade.modmed[[1]]

actuals <- table(Compare_val$Actuals[[1]])

actuals <- as.data.frame(actuals)

actuals <- plyr::rename(actuals, c(
  "Var1" = "social.grade",
  "Freq" = "actuals"))

imputed <- table(Compare_val$Predictions)

imputed <- as.data.frame(imputed)

imputed <- plyr::rename(imputed, c(
  "Var1" = "social.grade",
  "Freq" = "imputed"))

plot.dat <- merge(actuals, imputed, by = "social.grade")

plot.dat.melt <- melt(plot.dat, id="social.grade")

plot.dat.melt <- plyr::rename(plot.dat.melt, c(
  "variable" = "output"))

predictions <- ggplot(plot.dat.melt, aes(x=social.grade,y=value,fill=output)) + 
  geom_bar(stat="identity", position = "identity", alpha=.5)

predictions + ggtitle("Post Imputation: Mode") +
  scale_x_discrete(
    name = "Social Grade", 
    breaks = pretty_breaks()
  )  + coord_cartesian(ylim=c(0,60000))

## ---- compare-econ-act
XGBoost <- round(confusion_XGBoost$overall[c('Accuracy','Kappa')],2)

CANCEIS <- round(confusion_CANCEIS$overall[c('Accuracy','Kappa')],2)

MixedMethods <- round(confusion_CANCEISXG$overall[c('Accuracy','Kappa')],2)

Mode <- c(round(socialGrade.modmed[[3]][['Correct']]/(socialGrade.modmed[[3]][['Correct']]+socialGrade.modmed[[3]][['Wrong']]),2), NA)

CompareSocialGrade <- cbind(XGBoost, CANCEIS, MixedMethods, Mode)

save(CompareSocialGrade, file = "data/output/CompareSocialGrade.RData")
