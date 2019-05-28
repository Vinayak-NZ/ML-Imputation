## ---- census-load
# Read CSV into R
CensusRaw <- read.csv(
  file = "Data/CensusTeachingFile.csv", skip = 1,
  header = TRUE, sep = ","
)

## ---- rename-recode-derive
# Rename variables
Census <- plyr::rename(CensusRaw, c(
  "Person.ID" = "person.id",
  "Region" = "region",
  "Residence.Type" = "residence.type",
  "Family.Composition" = "fam.comp",
  "Population.Base" = "resident.type",
  "Sex"="sex",
  "Age"="age",
  "Marital.Status" = "marital.status",
  "Student"="student",
  "Country.of.Birth" = "birth.country",
  "Health"="health",
  "Ethnic.Group" = "ethnicity",
  "Religion"="religion",
  "Economic.Activity" = "econ.act",
  "Occupation" = "occupation",
  "Industry" = "industry",
  "Hours.worked.per.week" = "hours.worked",
  "Approximated.Social.Grade" = "social.grade"
))

# Recode variables (dataset is mutated in order to recode variables)
Census <- Census %>% mutate_if(is.factor, as.character)

# Recode the Region variable so that it is numeric
Census$region[Census$region == "E12000001"] <- 1
Census$region[Census$region == "E12000002"] <- 2
Census$region[Census$region == "E12000003"] <- 3
Census$region[Census$region == "E12000004"] <- 4
Census$region[Census$region == "E12000005"] <- 5
Census$region[Census$region == "E12000006"] <- 6
Census$region[Census$region == "E12000007"] <- 7
Census$region[Census$region == "E12000008"] <- 8
Census$region[Census$region == "E12000009"] <- 9
Census$region[Census$region == "W92000004"] <- 10

Census$residence.type[Census$residence.type == "C"] <- 1
Census$residence.type[Census$residence.type == "H"] <- 2

Census$student[Census$student == 1] <- 0
Census$student[Census$student == 2] <- 1

Census <- Census %>% mutate_if(is.character, as.numeric)

Census$person.id <- as.character(Census$person.id)

Ht <- table(Census$hours.worked)
Census$hours.cont <- ifelse(Census$hours.worked == 1, runif(
  1:Ht[names(Ht) == 1],
  1, 15
),
ifelse(Census$hours.worked == 2, runif(1:Ht[names(Ht) == 2], 16, 30),
  ifelse(Census$hours.worked == 3, runif(1:Ht[names(Ht) == 3], 31, 48),
    ifelse(Census$hours.worked == 4, runif(1:Ht[names(Ht) == 4], 49, 60),
      Census$hours.worked
    )
  )
)
)

save(Census, file = "data/Census.Rda")

## ---- test-train-split
# Randomly select 80% of Census units and split into Train and Test data
set.seed(5)
Census80 <- sample(1:nrow(Census), 0.8 * nrow(Census), replace = FALSE)
Census20 <- setdiff(1:nrow(Census), Census80)

Census.train <- Census[Census80, ]
Census.test <- Census[Census20, ]

save(Census.train, file = "data/Census.train.Rda")
save(Census.test, file = "data/Census.test.Rda")
