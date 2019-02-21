# Read CSV into R
CensusRaw <- read.csv(file="Data/CensusTeachingFile.csv", skip=1, header=TRUE, sep=",")

#Rename variables
Census<-plyr::rename(CensusRaw, c("Person.ID"="PersonID",
                                  "Residence.Type"="ResidenceType", 
                                  "Family.Composition"="FamComp", 
                                  "Population.Base"="ResidentType", 
                                  "Marital.Status"="MaritalStatus",
                                  "Country.of.Birth"="BirthCountry",
                                  "Ethnic.Group"="Ethnicity",
                                  "Economic.Activity"="EconAct",
                                  "Hours.worked.per.week"="HoursWorked",
                                  "Approximated.Social.Grade"="SocialGrade"))

#Recode variables
Census %>% mutate_if(is.factor, as.character) -> Census

#Recode the Region variable so that it is numeric
Census$Region[Census$Region=="E12000001"] <- 1
Census$Region[Census$Region=="E12000002"] <- 2
Census$Region[Census$Region=="E12000003"] <- 3
Census$Region[Census$Region=="E12000004"] <- 4
Census$Region[Census$Region=="E12000005"] <- 5
Census$Region[Census$Region=="E12000006"] <- 6
Census$Region[Census$Region=="E12000007"] <- 7
Census$Region[Census$Region=="E12000008"] <- 8
Census$Region[Census$Region=="E12000009"] <- 9
Census$Region[Census$Region=="W92000004"] <- 10

Census$ResidenceType[Census$ResidenceType=="C"] <- 1
Census$ResidenceType[Census$ResidenceType=="H"] <- 2

Census %>% mutate_if(is.character, as.numeric) -> Census
Census$PersonID<-as.character(Census$PersonID)

save(Census,file="data/Census.Rda")

#Randomly select 80% of Census units and split into Train and Test data
set.seed(5)
Census80 <- sample(1:nrow(Census),0.8 * nrow(Census), replace=FALSE)
Census20 <- setdiff(1:nrow(Census), Census80)

Census.train<-Census[Census80,]
Census.test<-Census[Census20,]

save(Census.train,file="data/Census.train.Rda")
save(Census.test,file="data/Census.test.Rda")

#Ampute test dataset (i.e. simulate missingness)
Census.test.amp<-ampute(Census.test[,-1], prop=0.5)
Census.test.miss<-Census.test.amp$amp
Census.test.miss[is.na(Census.test.miss)] <- -999

save(Census.test.miss,file="data/Census.test.miss.Rda")


