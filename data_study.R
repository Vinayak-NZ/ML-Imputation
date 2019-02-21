#Load datasets
##Full Census dataset
load("data/Census.Rda")
##Test and Training data
load("data/Census.train.Rda")
load("data/Census.test.Rda")
##Test data with missingness
load("data/Census.test.miss.Rda")

#Study the data: 1) How many units, 2) How many attributes? and 3) How many missing units?
str(Census)
summary(Census)
missing_values <-sapply(Census, function(y) sum(length(which(is.na(y)))))
missing_values <- data.frame(missing_values)

#Compare the test and training datasets
str(Census.train)
str(Census.test)
summary(Census.train)
summary(Census.test)
summary(Census.test.miss)

#Study the test dataset with missingness: 1) How much missingness per variable?
Number_Missing <-sapply(Census.test.miss, function(y) sum(length(which(is.na(y)))))
TestNumberMissing <- data.frame(Number_Missing)

#Plot distribution of variables
melt.Census <- melt(Census)
head(melt.Census)
ggplot(data = melt.Census, aes(x = value)) + 
  geom_bar() + 
  facet_wrap(~variable, scales = "free")

#Plot distribution of variables
melt.Census.train <- melt(Census.train)
head(melt.Census.train)
ggplot(data = melt.Census.train, aes(x = value)) + 
  geom_bar() + 
  facet_wrap(~variable, scales = "free")
melt.Census.test <- melt(Census.test)
head(melt.Census.test)
ggplot(data = melt.Census.test, aes(x = value)) + 
  geom_bar() + 
  facet_wrap(~variable, scales = "free")

#Look for relationship between variables
ggcorr(Census[,-1], nbreaks=8, palette='RdGy', 
       label=TRUE, label_size=3, label_color='white')

