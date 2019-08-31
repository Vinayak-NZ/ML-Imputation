## ---- load-core
load("data/core/Census.Rda")

## ---- load-ohe
# Load Train and Test
load("data/ohe/Census.train.ohe.Rda")
load("data/ohe/Census.test.ohe.Rda")

# Load Missing
load("data/ohe/missingness/Census.EconAct.ohe.RData")
load("data/ohe/missingness/Census.HoursCont.ohe.RData")
load("data/ohe/missingness/Census.SocialGrade.ohe.RData")
load("data/ohe/missingness/Census.Student.ohe.RData")

## ---- load-label
load("data/label/Census.train.label.Rda")
load("data/label/Census.test.label.Rda")

# Load Missing
load("data/label/missingness/Census.EconAct.label.RData")
load("data/label/missingness/Census.HoursCont.label.RData")
load("data/label/missingness/Census.SocialGrade.label.RData")
load("data/label/missingness/Census.Student.label.RData")
