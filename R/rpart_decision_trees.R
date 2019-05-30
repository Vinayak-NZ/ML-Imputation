library(ggparty)

pidno <- c(1:5000)
Bachelors <- rbinom(n=5000, size=1, prob=0.5)
Masters <- rbinom(n=5000, size=1, prob=0.3)
pHD <- rbinom(n=5000, size=1, prob=0.1)
Income <- as.data.frame(cbind(pidno, Bachelors, Masters, pHD))

Bt <- table(Income$Bachelors)
Mt <- table(Income$Masters)
Pt <- table(Income$pHD)
Income$income <- ifelse(Income$Bachelors == 1, rnorm(1:Bt[names(Bt) == 1], 55000, 10000),
                 rnorm(1:Bt[names(Bt) == 0], 30000, 10000))
Income$income <- ifelse(Income$Masters == 1, rnorm(1:Mt[names(Mt) == 1], 70000, 5000),
                       Income$income)
Income$income <- ifelse(Income$pHD == 1, rnorm(1:Pt[names(Pt) == 1], 90000, 2500),
                        Income$income)

Income$Bachelors[Income$Bachelors == 1] <- "Yes"
Income$Bachelors[Income$Bachelors == 0] <- "No"

Income$Masters[Income$Masters == 1] <- "Yes"
Income$Masters[Income$Masters == 0] <- "No"

Income$pHD[Income$pHD == 1] <- "Yes"
Income$pHD[Income$pHD == 0] <- "No"

save(Income, file = "data/Income_tree.RData")
load("data/Income_tree.RData")

fit <- rpart(income ~ Bachelors, data=Income)
fit <- ctree(income ~ Bachelors, data=Income)
rattle::fancyRpartPlot(fit,type=4)

fit2 <- rpart(income ~ Masters, data=Income)
fit2 <- ctree(income ~ Masters, data=Income)
rattle::fancyRpartPlot(fit2, type=4)

fit3 <- rpart(income ~ pHD, data=Income)
rattle::fancyRpartPlot(fit3,type=4)

fit4 <- rpart(income ~ Bachelors + Masters + pHD, data=Income)
fit4 <- ctree(income ~ Bachelors + Masters + pHD, data=Income)
rattle::fancyRpartPlot(fit4, type=4)

ggparty(fit) +
  geom_edge() +
  geom_edge_label() +
  geom_node_label(aes(label = splitvar), ids = "inner") +
  geom_node_plot(gglist = list(geom_histogram(aes(x = !!fit$terms[[2]],
                                            fill = !!fit$terms[[2]])),
                               theme_minimal()))

ggparty(fit4) +
  geom_edge() +
  geom_edge_label() +
  geom_node_label(aes(label = splitvar), ids = "inner") +
  geom_node_plot(gglist = list(geom_histogram(aes(x = !!fit$terms[[2]],
                                                  fill = !!fit$terms[[2]])),
                               theme_minimal()))
