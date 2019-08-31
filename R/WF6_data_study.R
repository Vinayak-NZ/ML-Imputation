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
str(Census.train.label)

str(Census.test.label)

summary(Census.train.label)

summary(Census.test.label)

sapply(apply(Census.train.label[, c(-1, -19)], 2, table), function(x) x / sum(x))

sapply(apply(Census.test.label[, c(-1, -19)], 2, table), function(x) x / sum(x))

# Plot distribution of variables
Census.graph <- subset(Census, select=-hours.cont)

melt.Census <- melt(Census.graph)

melt.Census <- melt.Census[!melt.Census$value==-9,]

head(melt.Census)

ggplot(data = melt.Census, aes(x = value)) +
  geom_bar() +
  facet_wrap(~variable, scales = "free")

ggsave("images/dist_all.png")

# Plot distribution of variables
Census.train.graph <- subset(Census.train.label, select=-hours.cont)

melt.Census.train.graph <- melt(Census.train.graph)

melt.Census.train.graph <- melt.Census.train.graph[!melt.Census.train.graph$value==-9,]

head(melt.Census.train.graph)

ggplot(data = melt.Census.train.graph, aes(x = value)) +
  geom_bar() +
  facet_wrap(~variable, scales = "free")

ggsave("images/dist_train.png")

Census.test.graph <- subset(Census.test.label, select=-hours.cont)

melt.Census.test.graph <- melt(Census.test.graph)

melt.Census.test.graph <- melt.Census.test.graph[!melt.Census.test.graph$value==-9,]

head(melt.Census.test.graph)

ggplot(data = melt.Census.test.graph, aes(x = value)) +
  geom_bar() +
  facet_wrap(~variable, scales = "free")

ggsave("images/dist_test.png")
