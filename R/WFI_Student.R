## ---- study-student
# Study the variable: How many units in each category?
St <- table(Census$student)

St
# What is the distribution of the variable: Remove NCR and plot to look at distribution
g <- ggplot(Census[!Census$student == -9, ], aes(student))

g + geom_bar() + scale_x_discrete(name = "Student status",
                                  breaks = pretty_breaks())

## ---- Build-XGBoost-econ-act
student <- trainDT(
  train = Census.train.label,
  test.orig = Census.test.label,
  test.miss = Census.Student.label,
  ident = 'person.id',
  cat = 1,
  var = 'student',
  exc = -9,
  col_order = c("region", "residence.type", "fam.comp", "resident.type", "sex",
                "age", "marital.status", "student", "birth.country", "health", "ethnicity",
                "religion", "econ.act", "occupation", "industry", "social.grade", "hours.cont"),
  objective = "reg:logistic",
  max_depth = 3,
  eta = 0.3,
  nrounds = 30
)

# Save feature importance plot
png(filename="images/featuresXG_student.png")
xgb.plot.importance(importance_matrix = student[[3]])
dev.off()

# Examine quality metrics
compareMissing <- student[[7]]

counts <- table(compareMissing$indicator)

counts.melt <- melt(counts)

counts.melt$Outcome <- counts.melt$Var1

accuracy_plot <- ggplot(data=counts.melt, aes(x=Outcome, y=value)) +
  geom_bar(stat="identity") + ggtitle("Accuracy of Predictions: XGBoost")

accuracy_plot

ggsave("images/accuracyXG_student.png")

confusion_XGBoost <- confusionMatrix(
  factor(compareMissing$Actuals),
  factor(compareMissing$Predictions)
)

compareMissing$Actuals <- ifelse(compareMissing$Actuals == 0, "Yes", "No")

compareMissing$Predictions <- ifelse(compareMissing$Predictions == 0, "Yes", "No")

qplot(Actuals, Predictions,
      data = compareMissing,
      geom = c("jitter"), main = "predicted vs. observed in test data",
      xlab = "Observed Class", ylab = "Predicted Class"
) + scale_x_discrete(labels = c("Yes", "No")
) + scale_y_discrete(labels = c("Yes", "No"))

ggsave("images/STXGBoostqplot.png")

# What is the distribution of the variable before and after imputation
Compare_val <- student[[6]]

Label_val <- Compare_val

actuals <- ggplot(Compare_val, aes(Actuals))

actuals + geom_bar() + ggtitle("True Distribution") + 
  scale_x_discrete(
    name = "Student",
    labels = c("Yes","No"),
    breaks = pretty_breaks()
  )

ggsave("images/actuals_student.png")

actuals <- table(Compare_val$Actuals)

actuals <- as.data.frame(actuals)

actuals <- plyr::rename(actuals, c(
  "Var1" = "student",
  "Freq" = "actuals"))

imputed <- table(Compare_val$PI)

imputed <- as.data.frame(imputed)

imputed <- plyr::rename(imputed, c(
  "Var1" = "student",
  "Freq" = "imputed"))

plot.dat <- merge(actuals, imputed, by = "student")

plot.dat.melt <- melt(plot.dat, id="student")

plot.dat.melt <- plyr::rename(plot.dat.melt, c(
  "variable" = "output"))

predictions <- ggplot(plot.dat.melt, aes(x=student,y=value,fill=output)) + 
  geom_bar(stat="identity", position = "identity", alpha=.5)

predictions + ggtitle("Post Imputation: XGBoost") +
  scale_x_discrete(
    name = "Student", 
    breaks = pretty_breaks()
  )

ggsave("images/PIXG_student.png")

# Save model
trainS_v1 <- student[[2]]

xgb.save(trainS_v1, "models/XGBoost/xgboost.student")

# Save predicted values
predicted.student <- student[[5]]

save(predicted.student, file = "data/predicted/XGBoost/predicted.student.RData")

## ---- CANCEIS-input-econ-act
student.CANCEIS <- CANCEIS.in(
  dat.cor=Census,
  dat.miss=Census.Student.label,
  var='student',
  exc=-9,
  varlist=c("region", "residence.type", "fam.comp", "resident.type", "sex",
            "age", "marital.status", "student", "birth.country", "health", "ethnicity",
            "religion", "econ.act", "occupation", "industry", "social.grade", "hours.worked"))

write.table(student.CANCEIS[[2]],
            file = "data/CANCEIS/Student/xxxUNIT01IG01.txt", sep = "\t",
            row.names = FALSE, col.names = FALSE
)

## ---- CANCEISXG-input-econ-act
# Impute values using CANCEIS (with XGBoost to advise selection of MVs)
# Create CANCEIS input file with imputable and matching variables
student.CANCEISXG <- CANCEISXG(
  dat.miss = Census.Student.label, 
  var = 'student', 
  featureList = student[[3]])

write.table(student.CANCEISXG,
            file = "data/CANCEISXG/Student/xxxUNIT01IG01.txt", sep = "\t",
            row.names = FALSE, col.names = FALSE
)

## ---- Evaluate-CANCEIS-econ-act
# Load CANCEIS input and output files
student.CANCEIS.load <- loadCANCEIS(
  var='student',
  varlist=names(student.CANCEIS[[2]]),
  xg=0)

eval.student.CANCEIS <- evalCANCEIS(
                                    cat = 1,
                                    var = 'student', 
                                    exc = -9, 
                                    actuals = Census.test.label, 
                                    missing = student.CANCEIS.load[[1]], 
                                    predicted = student.CANCEIS.load[[2]])

# Compare predicted and actuals
counts <- eval.student.CANCEIS[[3]]

counts.melt <- melt(counts)

counts.melt$Outcome <- counts.melt$Var1

accuracy_plot <- ggplot(data=counts.melt, aes(x=Outcome, y=value)) +
  geom_bar(stat="identity") + ggtitle("Accuracy of Predictions: CANCEIS")

accuracy_plot

ggsave("images/accuracyCANCEIS_student.png")

# Using Confusion Matrix to evaluate predictions
compareMissing_CANCEIS <- eval.student.CANCEIS[[2]]

confusion_CANCEIS <- confusionMatrix(
  factor(compareMissing_CANCEIS$Actuals[[1]]),
  factor(compareMissing_CANCEIS$Predictions)
)

compareMissing_CANCEIS$Actuals[[1]] <- ifelse(compareMissing_CANCEIS$Actuals[[1]] == 0, "Yes", "No")

compareMissing_CANCEIS$Predictions <- ifelse(compareMissing_CANCEIS$Predictions == 0, "Yes", "No")

qplot(Actuals[[1]], Predictions,
      data = compareMissing_CANCEIS,
      geom = c("jitter"), main = "predicted vs. observed in test data",
      xlab = "Observed Class", ylab = "Predicted Class"
) + scale_x_discrete(limits=c("Yes","No")
) + scale_y_discrete(limits=c("Yes","No"))

ggsave("images/STCANCEISqplot.png")

# What is the distribution of the variable after imputation?
Compare_val <- eval.student.CANCEIS[[1]]

actuals <- table(Compare_val$Actuals)

actuals <- as.data.frame(actuals)

actuals <- plyr::rename(actuals, c(
  "Var1" = "student",
  "Freq" = "actuals"))

imputed <- table(Compare_val$Predictions)

imputed <- as.data.frame(imputed)

imputed <- plyr::rename(imputed, c(
  "Var1" = "student",
  "Freq" = "imputed"))

plot.dat <- merge(actuals, imputed, by = "student")

plot.dat.melt <- melt(plot.dat, id="student")

plot.dat.melt <- plyr::rename(plot.dat.melt, c(
  "variable" = "output"))

predictions <- ggplot(plot.dat.melt, aes(x=student,y=value,fill=output)) + 
  geom_bar(stat="identity", position = "identity", alpha=.5)

predictions + ggtitle("Post Imputation: CANCEIS") +
  scale_x_discrete(
    name = "Student", 
    breaks = pretty_breaks()
  )

ggsave("images/PICANCEIS_student.png")

## ---- Evaluate-CANCEISXG-econ-act
# Load CANCEIS input and output files
student.CANCEISXG.load <- loadCANCEIS(
  var='Student',
  varlist=names(student.CANCEISXG),
  xg=1)

eval.student.CANCEISXG <- evalCANCEIS(
                                      cat = 1,
                                      var = 'student', 
                                      exc = -9, 
                                      actuals = Census.test.label, 
                                      missing = student.CANCEISXG.load[[1]], 
                                      predicted = student.CANCEISXG.load[[2]])

# Compare predicted and actuals
counts <- eval.student.CANCEISXG[[3]]

counts.melt <- melt(counts)

counts.melt$Outcome <- counts.melt$Var1

accuracy_plot <- ggplot(data=counts.melt, aes(x=Outcome, y=value)) +
  geom_bar(stat="identity") + ggtitle("Accuracy of Predictions: CANCEISXG")

accuracy_plot

ggsave("images/accuracyCANCEISXG_student.png")

# Using Confusion Matrix to evaluate predictions
compareMissing_CANCEISXG <- eval.student.CANCEISXG[[2]]

confusion_CANCEISXG <- confusionMatrix(
  factor(compareMissing_CANCEISXG$Actuals[[1]]),
  factor(compareMissing_CANCEISXG$Predictions)
)

compareMissing_CANCEISXG$Actuals[[1]] <- ifelse(compareMissing_CANCEISXG$Actuals[[1]] == 0, "Yes", "No")

compareMissing_CANCEISXG$Predictions <- ifelse(compareMissing_CANCEISXG$Predictions == 0, "Yes", "No")

qplot(Actuals[[1]], Predictions,
      data = compareMissing_CANCEISXG,
      geom = c("jitter"), main = "predicted vs. observed in test data",
      xlab = "Observed Class", ylab = "Predicted Class"
) + scale_x_discrete(limits=c("Yes","No")
) + scale_y_discrete(limits=c("Yes","No"))

ggsave("images/STCANCEISXGqplot.png")

# What is the distribution of the variable after imputation?
Compare_val <- eval.student.CANCEISXG[[1]]

actuals <- table(Compare_val$Actuals[[1]])

actuals <- as.data.frame(actuals)

actuals <- plyr::rename(actuals, c(
  "Var1" = "student",
  "Freq" = "actuals"))

imputed <- table(Compare_val$Predictions)

imputed <- as.data.frame(imputed)

imputed <- plyr::rename(imputed, c(
  "Var1" = "student",
  "Freq" = "imputed"))

plot.dat <- merge(actuals, imputed, by = "student")

plot.dat.melt <- melt(plot.dat, id="student")

plot.dat.melt <- plyr::rename(plot.dat.melt, c(
  "variable" = "output"))

predictions <- ggplot(plot.dat.melt, aes(x=student,y=value,fill=output)) + 
  geom_bar(stat="identity", position = "identity", alpha=.5)

predictions + ggtitle("Post Imputation: CANCEISXG") +
  scale_x_discrete(
    name = "Student", 
    breaks = pretty_breaks()
  )

ggsave("images/PICANCEISXG_student.png")

## ---- Evaluate-mode/median-econ-act
# Compare predicted and actuals
student.modmed <- modmedImp(
  cat = 1,
  var = 'student',
  exc = -9,
  actuals = Census.test.label,
  missing = Census.Student.label,
  mod=1)

# Compare predicted and actuals
counts <- student.modmed[[3]]

counts.melt <- melt(counts)

counts.melt$Outcome <- counts.melt$Var1

accuracy_plot <- ggplot(data=counts.melt, aes(x=Outcome, y=value)) +
  geom_bar(stat="identity") + ggtitle("Accuracy of Predictions")

accuracy_plot

ggsave("images/accuracyModMed_student.png")

## ---- compare-econ-act
XGBoost <- round(confusion_XGBoost$overall[c('Accuracy','Kappa')],2)

CANCEIS <- round(confusion_CANCEIS$overall[c('Accuracy','Kappa')],2)

MixedMethods <- round(confusion_CANCEISXG$overall[c('Accuracy','Kappa')],2)

Mode <- c(round(student.modmed[[3]][['Correct']]/(student.modmed[[3]][['Correct']]+student.modmed[[3]][['Wrong']]),2), NA)

CompareStudent <- cbind(XGBoost, CANCEIS, MixedMethods, Mode)

save(CompareStudent, file = "data/output/CompareStudent.RData")

