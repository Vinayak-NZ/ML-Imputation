## ---- set-missing-ohe
# Purpose: Extend missingness to all one hot encoded variables.
# Input: Dataset with one hot encoding, where missingness has been simulated for each level of categorical variable independently
# Output: Dataset with one hot enocding, where missingness is extended to all levels of the categorical variables
# Parameters for the function are included below
## dat: Name of dataset to modify
## varlist: A vector of variable names with one hot encoding applied. 
## varlist: If "Sex" is coded as "Sex.1" and "Sex.2" in the one hot encoded dataset, this vaariable would simply be expressed as "Sex" in this argument
ohe.miss <- function(dat, varlist) {
  for (i in 1:length(varlist)) {
    var <- varlist[[i]]

    names <- select_vars(names(dat), starts_with(var, ignore.case = TRUE))

    max <- length(names)

    for (j in 1:max) {
      for (k in 1:max) {
        dat[, as.character(names[[j]])] <- ifelse((dat[
          ,
          as.character(names[[k]])
        ] == -999),
        -999, eval(as.name(names[[j]]), dat)
        )
      }
    }
  }

  return(dat)
}

## ---- Subset datasets
# Purpose: Subset datasets with one hot encoding
# Input: Dataset with one hot encoded variables
# Output: Dataset that is subset to a selection of variables from initial list
# Parameters for the function are included below
## dat: Name of datset to subset
## varlist: A list of variables to subset the data to. 
## In cases where one hot encoding is applied, simply specify the original name of the variable. For example, if the variable "Sex" is coded as "Sex.1" and "Sex.2", simply enter "Sex" for this argument.
sub.dat.ohe <- function(dat,varlist){
  names_all <- as.list(NULL)
  for (i in 1:length(varlist)) {
    names <- select_vars(names(dat), starts_with(varlist[i], ignore.case = TRUE))
    names_all[[i]] <- names
  }
  dat <- dat[,unlist(names_all)]
  return(dat)
  
}

## ---- Simulate-missingness
# Purpose: Simulate missingness in one hot encoded dataset (with respect to imputable variable)
# Input: Dataset without missingness
# Output: Dataset with missingness
# Parameters for the function are included below
## dat: Name of input dataset
## ident: Name of unique identifier in the dataset
## var.aux: List of auxiliary variables in dataset
## ohe.vars: List of variables that have been one hot encoded
## imp.vars: List of imputable variables
## var: Name of imputable variable to have increased missingness for
## ohe: An indicator of whether imputable variable (in var) is one hot encoded
## exc: Exclude rows that are not eligible to answer imputable item
## crop: Exclude columns not eligible to answer imputable item (only eligible if imputable variable is one hot encoded)
SimMissV <- function(dat,ident,var.aux,ohe.vars,imp.vars,var,ohe,exc,drop){
  
  # Order dataset by identifier
  dat <- dat[order(dat[,as.character(ident)]),]
  
  # Remove all instances where imputable variable is NCR
  if(ohe==1){
    dat.tidy <- subset(dat, !(grepl(exc, dat[,drop])))
    dat.tidy <- dat.tidy[,!names(dat.tidy)==drop]
  } else{
    dat.tidy <- subset(dat, !(grepl(exc, dat[,var])))
  }
  
  # Remove the personal identifier in order to stimulate missingness
  person.id <- dat.tidy[,as.character(ident)]
  dat.tidy <- dat.tidy[, -1]
  
  # Subset dataset to auxiliary variables
  dat.aux <- sub.dat.ohe(dat=dat.tidy, 
                                 varlist=c(var.aux))
  
  #Setting up two missingness patterns for the auxiliary variables
  #One pattern for core auxiliary variables
  #One pattern for other auxiliary variables
  pattern_core <- c(rep(0,28), rep(1,46))
  pattern_oth <- c(rep(1,28), rep(0,46))
  pattern_aux <- as.matrix(rbind(pattern_core, pattern_oth))
  
  dat.aux.amp <- ampute(dat.aux, prop = 0.4, mech = "MAR", 
                                set.seed(5), pattern=pattern_aux, freq=c(0.2,0.8))
  
  dat.aux.miss <- dat.aux.amp$amp
  
  instance_sub <- setdiff(imp.vars,var)
  
  # Subset data to mputable variables
  dat.imp <- sub.dat.ohe(dat=dat.tidy, 
                                 varlist=c(
                                   var,
                                   instance_sub))
  
  #Setting up two missingness patterns for the imputable variables
  #One pattern for the imputable variable of interest
  #One pattern for other imputable variables
  iteration <- select_vars(names(dat.imp), starts_with(var, ignore.case = TRUE))
  
  pattern_pr <- c(rep(0,length(iteration)), rep(1,eval(ncol(dat.imp)-length(iteration))))
  pattern_sc <- c(rep(1,length(iteration)), rep(0,eval(ncol(dat.imp)-length(iteration))))
  pattern_imp <- as.matrix(rbind(pattern_pr, pattern_sc))
  
  dat.imp.amp <- ampute(dat.imp, prop = 0.5, mech = "MAR", 
                                set.seed(5), pattern=pattern_imp, freq=c(0.8,0.2))
  
  dat.imp.miss <- dat.imp.amp$amp
  
  dat.tidy.miss <- cbind(dat.imp.miss, dat.aux.miss)
  
  # Convert missing cases to -999
  dat.tidy.miss[is.na(dat.tidy.miss)] <- -999
  
  instance_miss <- setdiff(ohe.vars,var)
    
  dat.tidy.miss <- ohe.miss(dat=dat.tidy.miss, 
                              varlist=c(instance_miss))
  
  dat.miss <- cbind(person.id, dat.tidy.miss)
  
  # Return dataset with missingenss
  return(dat.miss)
  
  
}

## ---- Label-encoding-missingness
# Purpose: Replicate missingness pattern in label encoded dataset
# Input: Label encoded dataset with no missingness 
# Output: Label encoded dataset with missingness that matches one hot encoded dataset
# Parameters for the function are included below
## dat.label: Name of label encoded dataset
## dat.ohe: Name of one hot encoded dataset with missingness
## ident: Unique identifier (found in both dat.label and dat.ohe)
## ohe_vars: Variables with one hot encoding applied
## keep_var: Variables to keep in final label encoded dataset with missingness
LabelMiss <- function(dat.label,dat.ohe,ident,ohe_vars,keep_var){
  
  dat.label <- dat.label[,names(dat.label) %in% c(ident,ohe_vars)]
  
  dat.sub <- merge(dat.label, dat.ohe, by='person.id')
  
  for (i in 1:length(ohe_vars)) {
    var <- ohe_vars[[i]]
    
    names <- select_vars(names(dat.sub), starts_with(var, ignore.case = TRUE))
    
    dat.sub[, as.character(ohe_vars[[i]])] <- ifelse((dat.sub[,as.character(names[[2]])] == -999),-999, eval(as.name(ohe_vars[[i]]), dat.sub))
  }
  
  dat.label.miss <- dat.sub[, names(dat.sub) %in% keep_var]
  
  return(dat.label.miss)
  
}


## ---- Run-XGBoost-models
# Purpose: Train XGBoost model to impute variable
# Input: Dataset with multiple auxiliary variables and single imputable variable 
# Output: Model for using auxiliary variables to predict imputable variable
# Parameters for the function are included below
## train: Name of training dataset
## test.orig: Name of test dataset without missingess
## test.miss: Name of test dataset with missingness simulated
## ident: Name of unique identifier in dataset
## cat: An indicator if imputable variable is categorical. cat = 1 if categorical and cat = 0 if continuous
## var: Name of imputable variable
## exc: Values of imputable variable to exclude from model training
## col_order: The order in which to arrange the features in the training and test matrices
## objective: The objective function to use in the XGBoost model
## max_depth: Maximum depth of trees for the XGBoost model
## eta: Learning rate for the XGBoost model
## nrounds: Number of rounds (i.e. trees) to use in the XGBoost model
## num_class: Number of classes in categorical variable + 1. Only use if imputable variable is categorical (i.e. if cat = 1)
trainDT <- function(train, test.orig, test.miss, ident, cat, var, exc, col_order,
                    objective, max_depth, eta, nrounds, num_class){
  ## ---- tidy-data
  # Sort all test datasets by identifier to ensure final comparison is valid
  test.full <- test.orig[order(test.orig[,as.character(ident)]),]
  
  test.miss <- test.miss[order(test.miss[,as.character(ident)]),]
  
  # Tidy/treat the training and test datasets
  # Remove units with NCR codes and remove the personal identifier
  train.tidy <- subset(train, !(grepl(exc, train[,var])), select = col_order)
  
  test.full.tidy <- subset(test.full, !(grepl(exc, test.full[,var])), select = col_order)
  
  test.miss.tidy <- subset(test.miss, select = col_order)
  
  train.tidy <- train.tidy[, col_order]
  
  test.full.tidy <- test.full.tidy[, col_order]
  
  test.miss.tidy <- test.miss.tidy[, col_order]
  
  ## ---- missingness-examine
  # Study the test dataset with missingness: How much missingness per variable?
  NumberMissing <- sapply(test.miss.tidy, function(y) sum(length(
    which((y)==-999)
  )))
  
  TestNumberMissing <- data.frame(NumberMissing)
  
  ## ---- train-model
  # Train the model
  # Convert the data to matrix and assign output variable
  train.outcome <- train.tidy[,var]
  
  train.ex <- train.tidy[,!(names(train.tidy)==var)]
  
  train.predictors <- sparse.model.matrix(train.tidy[,var] ~ .,
                                          data = train.ex
  )[, -1]
  
  test.outcome <- test.miss.tidy[,var]
  
  test.ex <- test.miss.tidy[,!(names(test.miss.tidy)==var)]
  
  test.predictors <- model.matrix(test.miss.tidy[,var] ~ .,
                                  data = test.ex
  )[, -1]
  
  # Convert the matrix objects to DMatrix objects
  dtrain <- xgb.DMatrix(train.predictors, label = train.outcome)
  
  dtest <- xgb.DMatrix(test.predictors, missing = -999)
  
  # Train a model using training set
  if(objective == "multi:softmax"){
    model <- xgboost(
      data = dtrain, max_depth = max_depth, eta = eta, nthread = 2, nrounds = nrounds,
      objective = "multi:softmax", num_class = num_class, missing = -999
    )
  } else if (objective == "reg:linear"){
    model <- xgboost(
      data = dtrain, max_depth = max_depth, eta = eta, nthread = 2, nrounds = nrounds,
      objective = "reg:linear", missing=-999
    )
  } else {
    model <- xgboost(
      data = dtrain, max_depth = max_depth, eta = eta, nthread = 2, nrounds = nrounds,
      objective = "reg:logistic", missing=-999
    )
  }

  # Examine feature importance
  importance_matrix <- xgb.importance(model = model)
  
  print(importance_matrix)
  
  plot.matrix <- xgb.plot.importance(importance_matrix = importance_matrix)
  
  ## ---- test-model
  # Test the model
  if(objective=="reg:logistic"){
  predicted <- ifelse(
    predict(model, dtest, missing = -999, na.action = na.pass) > 0.5, 1, 0)
  } else {
  predicted <- predict(model, dtest, missing = -999, na.action = na.pass)
  }
  
  ## ---- eval-model
  # Evaluate performance of XGBoost model
  # Compare versions of the outcome variable (Actual, Predicted, Missing)
  
  if(cat==1){
    actuals <- test.full.tidy[,var]
    
    missing <- test.miss.tidy[,var]
    
    compareVar <- tibble(
      Actuals = actuals, Predictions = predicted,
      Missing = missing
    )
    
    compareVar$PI <- ifelse(compareVar$Missing==-999, 
                            compareVar$Predictions,
                            compareVar$Actuals)
    
    compareMissing <- compareVar[compareVar$Missing == -999, ]
    
    compareMissing$indicator <- ifelse(compareMissing$Actuals ==
                                         compareMissing$Predictions,"Correct", "Wrong")
    
    output <- list(TestNumberMissing, 
                   model,
                   importance_matrix,
                   plot.matrix,
                   predicted,
                   compareVar,
                   compareMissing)
    
  } else{
    actuals <- test.full.tidy[,var]
    
    missing <- test.miss.tidy[,var]
    
    compareVar <- tibble(
      Actuals = actuals, Predictions = predicted,
      Missing = missing)
    
    compareVar$PI <- ifelse(compareVar$Missing==-999, 
                            compareVar$Predictions,
                            compareVar$Actuals)
    
    compareMissing <- compareVar[compareVar$Missing == -999, ]
    
    # Using Mean Absolute Error and Root Mean Square error to evaluate predictions
    MAE_XG <- MAE(compareMissing$Predictions,compareMissing$Actuals)
    
    RMSE_XG <- RMSE(compareMissing$Predictions,compareMissing$Actuals)
    
    output <- list(TestNumberMissing, 
                   model,
                   importance_matrix,
                   plot.matrix,
                   predicted,
                   compareVar,
                   compareMissing,
                   MAE_XG,
                   RMSE_XG)
    
  }
  
  return(output)
  
}


## ---- CANCEIS-matching-vars
# Purpose: Create a CANCEIS input dataset with matching variables for a singe imputable variable
# Input: Dataset with multiple auxiliary variables and single imputable variable 
# Output: Dataset with most highly correlated auxiliary variables and single imputable variable
# Parameters for the function are included below
## dat.cor: Name of dataset to use to derive correlation coefficients between imputable variable and auxiliary variables
## dat.miss: Name of dataset with missingness simulated
## var: Name of imputable variable
## exc: Exclusion condition for variables
## varlist: List of auxiliary variables to consider for selecting matching variables
CANCEIS.in <- function(dat.cor, dat.miss, var, exc, varlist) {

  dat.cor[dat.cor==exc] <- NA
  
  dat.tidy <- na.omit(dat.cor)
  
  variable <- NULL
  
  correlation <- NULL
  
  if(var=='hours.cont'){
    
    inputvar <- setdiff(varlist, 'hours.worked')
    
    for (i in 1:length(inputvar)){
      
      variable[i] <- inputvar[i]
      
      correlation[i] <- Lambda(dat.tidy[,'hours.worked'], dat.tidy[,inputvar[i]])
      }
    
    } else{
      
      inputvar <- setdiff(varlist, var)
      
      for (i in 1:length(inputvar)){
        
        variable[i] <- inputvar[i]
        
        correlation[i] <- Lambda(dat.tidy[,var], dat.tidy[,inputvar[i]])
        }
      }
  
  corlist <- data.frame(variable, correlation)
  
  corlist_high <- corlist[order(corlist$correlation, decreasing = TRUE),]
  
  corlist_high <- corlist_high[1:4,]
  
  CANCEIS.var <- corlist_high$variable
  
  var_imp <- dat.miss[,var]
  
  canceis.id <- 1:nrow(dat.miss)
  
  CANCEIS.phase1 <- cbind(canceis.id, var_imp)
  
  matching <- dat.miss[, names(dat.miss) %in% CANCEIS.var]
  
  CANCEIS.input <- cbind(CANCEIS.phase1, matching)
  
  CANCEIS.input <- plyr::rename(CANCEIS.input, c(
    "var_imp" = var))
  
  output <- list(corlist_high,
                 CANCEIS.input)
  
  return(output)
  
}


## ---- CANCEISXG-matching-vars
# Purpose: Create a CANCEIS input dataset with matching variables for a singe imputable variable, using feature importance from XGBoost
# Input: Dataset with multiple auxiliary variables and single imputable variable, along with feature importance from XGBoost model 
# Output: Dataset with auxiliary variables (from highest scores feature importance list) and single imputable variable
# Parameters for the function are included below
## dat.miss: Name of dataset with missingness simulated
## var: Name of imputable variable
## featureList: List of features used in XGBoost model with corresponding scores
CANCEISXG <- function(dat.miss, var, featureList){
  
  featureList <- featureList[order(featureList$Cover, decreasing = TRUE),]
  
  featureImp <- featureList[1:5,]
  
  featureSel <- featureImp$Feature
  
  canceis.id <- 1:nrow(dat.miss)
  
  var_imp <- dat.miss[,var]
  
  CANCESXG.phase1 <- cbind(canceis.id, var_imp)
  
  CANCEISXG.matching <- dat.miss[, featureSel]
  
  CANCEISXG.input <- cbind(CANCESXG.phase1, CANCEISXG.matching)
  
  CANCEISXG.input <- plyr::rename(CANCEISXG.input, c(
    "var_imp" = var))
  
  return(CANCEISXG.input)
  
}


## ---- load-CANCEIS-files
# Purpose: Load CANCEIS intput and output files
# Input: Name of imputable variable and auxiliary variables used for CANCEIS imputation 
# Output: Datasets with auxiliary variables and imputable variable, both before and after imputation
# Parameters for the function are included below
## var: Name of imputable variable
## varlist: List of matching variables used for CANCEIS imputation (must be presented in order in which input files were originally created)
## xg: An indicator of whether matching variables were selected using XGBoost (=1 if Yes and =0 if No)
loadCANCEIS <- function(var,varlist,xg){
  
  # Read in CANCEISXG input and output
  if(xg==1){
    CANCEIS.test.in <- read.table(paste0("data/CANCEISXG/",var,"/xxxUNIT01IG01.txt"),
                                    header = FALSE,
                                    col.names = varlist
    )[, -1]
    
    CANCEIS.test.out <- read.table(paste0("data/CANCEISXG/",var,"/XXXUNITIMP01IG01.txt"),
                                     header = FALSE,
                                     col.names = varlist
    )[, -1]
  } else {
    CANCEIS.test.in <- read.table(paste0("data/CANCEIS/",var,"/xxxUNIT01IG01.txt"),
                                    header = FALSE,
                                    col.names = varlist
  )[, -1]
  
    CANCEIS.test.out <- read.table(paste0("data/CANCEIS/",var,"/XXXUNITIMP01IG01.txt"),
                                     header = FALSE,
                                     col.names = varlist
  )[, -1]
  
  }
  
  output <- list(
    CANCEIS.test.in,
    CANCEIS.test.out
  )
  
  return(output)
}


## ---- eval-CANCEIS-performance
# Purpose: Evaluate performance of CANCEIS imputation
# Input: Dataset with CANCEIS imputation 
# Output: Metrics to evaluate quality of imputation
# Parameters for the function are included below
## cat: An indicator of whether imputable variable was categorical (=1 if Yes and =0 if No)
## var: Name of imputable variable
## exc: Values of imputable variable to exclude from from evaluating quality of predictions
## actuals: Name of dataset without missingness
## missing: Name of dataset with missingness simulated
## predicted: Name of dataset with imputed values
evalCANCEIS <- function(cat, var, exc, actuals, missing, predicted){
  
  actuals.CANCEIS <- subset(actuals, !(grepl(exc, actuals[,var])), select = var)
  
  colnames(actuals.CANCEIS) <- NULL
  
  missing.CANCEIS <- missing[, var]
  
  predicted.CANCEIS <- predicted[, var]
  
  compare_var_CANCEIS <- tibble(
    Actuals = actuals.CANCEIS, Predictions =
      predicted.CANCEIS, Missing = missing.CANCEIS
  )
  
  compare_missing_CANCEIS <- compare_var_CANCEIS[
    compare_var_CANCEIS$Missing == -999, ]
  
  if(cat==1){
  compare_missing_CANCEIS$indicator <- ifelse(
    compare_missing_CANCEIS$Actuals[[1]] ==
      compare_missing_CANCEIS$Predictions,
    "Correct", "Wrong"
  )
  
  counts_CANCEIS <- table(compare_missing_CANCEIS$indicator)
  
  output <- list(
    compare_var_CANCEIS,
    compare_missing_CANCEIS,
    counts_CANCEIS
  )
  } else {

    MAE <- MAE(compare_missing_CANCEIS$Predictions,compare_missing_CANCEIS$Actuals[[1]])
    
    RMSE <- RMSE(compare_missing_CANCEIS$Predictions,compare_missing_CANCEIS$Actuals[[1]])
    
    output <- list(
      compare_var_CANCEIS,
      compare_missing_CANCEIS,
      MAE,
      RMSE
    )
    
  }
  return(output)
  
}

## ---- mode-median-impute
# Purpose: Perform mode/ median imputation for imputable variable
# Input: Dataset with imputable variable
# Output: Dataset with mode/ median imputation for imputable variable
# Parameters for the function are included below
## var: Name of imputable variable
## exc: Values of imputable variable to exclude from calculating mode/ median
## actuals: Name of dataset without missingness
## missing: Name of dataset with missingness simulated
## mod: An indicator of whether to use mode/ median imputation (=1 if mode imputation =0 if median imputation)
modmedImp <- function(cat, var, exc, actuals, missing, mod){
  
  actuals.base <- subset(actuals, !(grepl(exc, actuals[,var])), select = var)
  
  colnames(actuals.base) <- NULL
  
  missing.base <- missing[, var]
  
  # Create a vector of imputable variable excluding missing values
  if(mod==1){
    
    base.dat <- subset(missing, !(grepl(-999, missing[,var])), select = var)
    
    base.imp <- Mode(base.dat[,var])

  } else {
    
    base.dat <- subset(missing, !(grepl(-999, missing[,var])), select = var)
    
    base.imp <- median(base.dat[,var])
    
  }

  compare_var_base <- tibble(
    Actuals = actuals.base, Missing = missing.base
  )
  
  compare_var_base$Predictions <- ifelse(compare_var_base$Missing==-999,
                                         base.imp,
                                         compare_var_base$Actuals[[1]])
  
  compare_missing_base <- compare_var_base[
    compare_var_base$Missing == -999, ]
  
  if(cat==1){
    compare_missing_base$indicator <- ifelse(
      compare_missing_base$Actuals[[1]] ==
        compare_missing_base$Predictions,
      "Correct", "Wrong"
    )
    
    counts_base <- table(compare_missing_base$indicator)
    
    output <- list(
      compare_var_base,
      compare_missing_base,
      counts_base
    )
    
  } else {
    MAE <- MAE(compare_missing_base$Predictions,compare_missing_base$Actuals[[1]])
    
    RMSE <- RMSE(compare_missing_base$Predictions,compare_missing_base$Actuals[[1]])
    
    output <- list(
      compare_var_base,
      compare_missing_base,
      MAE,
      RMSE
    )
    
  }
  
  return(output)

  
}
