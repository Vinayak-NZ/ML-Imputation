## ---- load-edited-data
# Load datasets
## Full Census dataset
load("data/Census.Rda")

## Test and Training data
load("data/Census.train.Rda")

load("data/Census.test.Rda")

## ---- study-census-data
# Study the data: 1) How many units, 2) How many attributes? and 
# 3) How many missing units?
str(Census)

summary(Census)

sapply(apply(Census[, c(-1, -19)], 2, table), function(x) x / sum(x))

missing_values <- sapply(Census, function(y) sum(length(which(is.na(y)))))

missing_values <- data.frame(missing_values)

# Look for relationship between variables
ggcorr(Census[, -1],
       nbreaks = 8, palette = "RdGy",
       label = TRUE, label_size = 3, label_color = "white"
)

ggsave("images/cor_all.png")

## ---- compare-test-train
# Compare the test and training datasets
str(Census.train)

str(Census.test)

summary(Census.train)

summary(Census.test)

sapply(apply(Census.train[, c(-1, -19)], 2, table), function(x) x / sum(x))

sapply(apply(Census.test[, c(-1, -19)], 2, table), function(x) x / sum(x))

# Plot distribution of variables
melt.Census <- melt(Census)

head(melt.Census)

ggplot(data = melt.Census, aes(x = value)) +
  geom_bar() +
  facet_wrap(~variable, scales = "free")

ggsave("images/dist_all.png")

# Plot distribution of variables
melt.Census.train <- melt(Census.train)

head(melt.Census.train)

ggplot(data = melt.Census.train, aes(x = value)) +
  geom_bar() +
  facet_wrap(~variable, scales = "free")

ggsave("images/dist_train.png")

melt.Census.test <- melt(Census.test)

head(melt.Census.test)

ggplot(data = melt.Census.test, aes(x = value)) +
  geom_bar() +
  facet_wrap(~variable, scales = "free")

ggsave("images/dist_test.png")
