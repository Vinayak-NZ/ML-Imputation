## ---- study-hours-cont
# Study the variable: How many units in each category?
summary(Census$hours.cont)

# What is the distribution of the variable: Remove NCR and plot to look at distribution
g <- ggplot(Census[!Census$hours.cont == -9, ], aes(hours.cont))

g + geom_histogram() + stat_bin(binwidth = 10) + scale_x_discrete(
  name = "Hours worked",
  breaks = pretty_breaks()
)

## ---- Build-XGBoost-econ-act
hoursCont <- trainDT(
  train = Census.train.label,
  test.orig = Census.test.label,
  test.miss = Census.HoursCont.label,
  ident = 'person.id',
  cat = 0,
  var = 'hours.cont',
  exc = -9,
  col_order = c("region", "residence.type", "fam.comp", "resident.type", "sex",
                "age", "marital.status", "student", "birth.country", "health", "ethnicity",
                "religion", "econ.act", "occupation", "industry", "social.grade", "hours.cont"),
  objective = "reg:linear",
  max_depth = 6,
  eta = 0.2,
  nrounds = 50
)

# Save feature importance plot
png(filename="images/featuresXG_hoursCont.png")
xgb.plot.importance(importance_matrix = hoursCont[[3]])
dev.off()

# Examine quality metrics
MAE_XG <- hoursCont[[8]]

RMSE_XG <- hoursCont[[9]]

# What is the distribution of the variable before and after imputation
Compare_val <- hoursCont[[6]]

actuals <- ggplot(hoursCont[[6]], aes(Actuals))

actuals + geom_histogram() + stat_bin(binwidth = 10) +
  ggtitle("True Distribution") + 
  scale_x_discrete(
    name = "Hours Worked",
    breaks = pretty_breaks()
  )

ggsave("images/actuals_hoursCont.png")

# What is the distribution of the variable before and after imputation
Compare_val <- plyr::rename(Compare_val, c(
  "PI" = "Imputed"))

plot.dat <- Compare_val[,c('Actuals','Imputed')]

plot.dat.melt <- melt(plot.dat)

plot.dat.melt <- plyr::rename(plot.dat.melt, c(
  "variable" = "output"))

cdat <- plyr::ddply(plot.dat.melt, "output", summarise, value.mean=mean(value))

predictions <- ggplot(plot.dat.melt, aes(x=value, fill=output)) +
  geom_histogram(binwidth=10, alpha=.5, position="identity") +
  geom_vline(data=cdat, aes(xintercept=value.mean,  colour=output),
             linetype="dashed", size=1)

predictions + ggtitle("Post Imputation: XGBoost") +
  scale_x_discrete(
    name = "Hours Worked", 
    breaks = pretty_breaks()
  )

ggsave("images/PIXG_hoursCont.png")

# Save model
trainHC_v1 <- hoursCont[[2]]

xgb.save(trainHC_v1, "models/XGBoost/xgboost.hoursCont")

# Save predicted values
predicted.hoursCont <- hoursCont[[5]]

save(predicted.hoursCont, file = "data/predicted/XGBoost/predicted.hoursCont.RData")

## ---- CANCEIS-input-hours-cont
hoursCont.CANCEIS <- CANCEIS.in(
  dat.cor=Census,
  dat.miss=Census.HoursCont.label,
  var='hours.cont',
  exc=-9,
  varlist=c("region", "residence.type", "fam.comp", "resident.type", "sex",
            "age", "marital.status", "student", "birth.country", "health", "ethnicity",
            "religion", "econ.act", "occupation", "industry", "social.grade", "hours.worked"))

write.table(hoursCont.CANCEIS[[2]],
            file = "data/CANCEIS/HoursCont/xxxUNIT01IG01.txt", sep = "\t",
            row.names = FALSE, col.names = FALSE
)

## ---- CANCEISXG-input-hours-cont
# Impute values using CANCEIS (with XGBoost to advise selection of MVs)
# Create CANCEIS input file with imputable and matching variables
hoursCont.CANCEISXG <- CANCEISXG(
  dat.miss = Census.HoursCont.label, 
  var = 'hours.cont', 
  featureList = hoursCont[[3]])

write.table(hoursCont.CANCEISXG,
            file = "data/CANCEISXG/HoursCont/xxxUNIT01IG01.txt", sep = "\t",
            row.names = FALSE, col.names = FALSE
)

## ---- Evaluate-CANCEIS-econ-act
# Load CANCEIS input and output files
hoursCont.CANCEIS.load <- loadCANCEIS(
  var='HoursCont',
  varlist=names(hoursCont.CANCEIS[[2]]),
  xg=0)

eval.hoursCont.CANCEIS <- evalCANCEIS(
                                    cat = 0,
                                    var = 'hours.cont', 
                                    exc = -9, 
                                    actuals = Census.test.label, 
                                    missing = hoursCont.CANCEIS.load[[1]], 
                                    predicted = hoursCont.CANCEIS.load[[2]])

# Examine quality metrics
MAE_CANCEIS <- eval.hoursCont.CANCEIS[[3]]

RMSE_CANCEIS <- eval.hoursCont.CANCEIS[[4]]

# What is the distribution of the variable before and after imputation
Compare_val <- eval.hoursCont.CANCEIS[[1]]

Compare_val <- plyr::rename(Compare_val, c(
  "Predictions" = "Imputed"))

actuals <- ceiling(Compare_val$Actuals[[1]])

imputed <- ceiling(Compare_val$Imputed)

plot.dat <- cbind(actuals, imputed)

plot.dat.melt <- melt(plot.dat)

plot.dat.melt <- plot.dat.melt[,-1]

plot.dat.melt <- plyr::rename(plot.dat.melt, c(
  "Var2" = "output"))

cdat <- plyr::ddply(plot.dat.melt, "output", summarise, value.mean=mean(value))

predictions <- ggplot(plot.dat.melt, aes(x=value, fill=output)) +
  geom_histogram(binwidth=10, alpha=.5, position="identity") +
  geom_vline(data=cdat, aes(xintercept=value.mean,  colour=output),
             linetype="dashed", size=1)

predictions + ggtitle("Post Imputation: CANCEIS") +
  scale_x_discrete(
    name = "Hours Worked", 
    breaks = pretty_breaks()
  )

ggsave("images/PICANCEIS_hoursCont.png")

## ---- Evaluate-CANCEISXG-econ-act
# Load CANCEIS input and output files
hoursCont.CANCEISXG.load <- loadCANCEIS(
  var='HoursCont',
  varlist=names(hoursCont.CANCEISXG),
  xg=1)

eval.hoursCont.CANCEISXG <- evalCANCEIS(
                                      cat = 0,
                                      var = 'hours.cont', 
                                      exc = -9, 
                                      actuals = Census.test.label, 
                                      missing = hoursCont.CANCEISXG.load[[1]], 
                                      predicted = hoursCont.CANCEISXG.load[[2]])

# Examine quality metrics
MAE_CANCEISXG <- eval.hoursCont.CANCEISXG[[3]]

RMSE_CANCEISXG <- eval.hoursCont.CANCEISXG[[4]]

# What is the distribution of the variable before and after imputation
Compare_val <- eval.hoursCont.CANCEISXG[[1]]

Compare_val <- plyr::rename(Compare_val, c(
  "Predictions" = "Imputed"))

actuals <- ceiling(Compare_val$Actuals[[1]])

imputed <- ceiling(Compare_val$Imputed)

plot.dat <- cbind(actuals, imputed)

plot.dat.melt <- melt(plot.dat)

plot.dat.melt <- plot.dat.melt[,-1]

plot.dat.melt <- plyr::rename(plot.dat.melt, c(
  "Var2" = "output"))

cdat <- plyr::ddply(plot.dat.melt, "output", summarise, value.mean=mean(value))

predictions <- ggplot(plot.dat.melt, aes(x=value, fill=output)) +
  geom_histogram(binwidth=10, alpha=.5, position="identity") +
  geom_vline(data=cdat, aes(xintercept=value.mean,  colour=output),
             linetype="dashed", size=1)

predictions + ggtitle("Post Imputation: CANCEISXG") +
  scale_x_discrete(
    name = "Hours Worked", 
    breaks = pretty_breaks()
  )

ggsave("images/PICANCEISXG_hoursCont.png")

## ---- Evaluate-mode/median-econ-act
# Compare predicted and actuals
hoursCont.modmed <- modmedImp(
  cat = 0,
  var = 'hours.cont',
  exc = -9,
  actuals = Census.test.label,
  missing = Census.HoursCont.label,
  mod=0)

# Examine quality metrics
MAE_median <- hoursCont.modmed[[3]]

RMSE_median <- hoursCont.modmed[[4]]

# What is the distribution of the variable before and after imputation
Compare_val <- hoursCont.modmed[[1]]

Compare_val <- plyr::rename(Compare_val, c(
  "Predictions" = "Imputed"))

actuals <- ceiling(Compare_val$Actuals[[1]])

imputed <- ceiling(Compare_val$Imputed)

plot.dat <- cbind(actuals, imputed)

plot.dat.melt <- melt(plot.dat)

plot.dat.melt <- plot.dat.melt[,-1]

plot.dat.melt <- plyr::rename(plot.dat.melt, c(
  "Var2" = "output"))

cdat <- plyr::ddply(plot.dat.melt, "output", summarise, value.mean=mean(value))

predictions <- ggplot(plot.dat.melt, aes(x=value, fill=output)) +
  geom_histogram(binwidth=10, alpha=.5, position="identity") +
  geom_vline(data=cdat, aes(xintercept=value.mean,  colour=output),
             linetype="dashed", size=1)

predictions + ggtitle("Post Imputation: Median") +
  scale_x_discrete(
    name = "Hours Worked", 
    breaks = pretty_breaks()
  )

## ---- compare-econ-act
XGBoost <- list(MAE = round(MAE_XG, 2), RMSE = round(RMSE_XG, 2))

CANCEIS <- list(MAE = round(MAE_CANCEIS, 2), RMSE = round(RMSE_CANCEIS, 2))

MixedMethods <- list(MAE = round(MAE_CANCEISXG, 2), RMSE = round(RMSE_CANCEISXG, 2))

Median <- list(MAE = round(MAE_median, 2), RMSE = round(RMSE_median, 2))

CompareHoursCont <- as.data.frame(cbind(XGBoost, CANCEIS, MixedMethods, Median))

save(CompareHoursCont, file = "data/output/CompareHoursCont.Rdata")
