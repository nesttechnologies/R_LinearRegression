#########Upload the data#########
#####Define the session/ Set working directory########
mydata <- read.csv("risk1.csv")
View(mydata)
########Change Column Names##########
colnames(mydata)[colnames(mydata) == 'Number.of.sexual.partners'] <- 'SexualPartners'
colnames(mydata)[colnames(mydata) == 'Num.of.pregnancies'] <- 'Pregnancies'
colnames(mydata)[colnames(mydata) == 'First.sexual.intercourse'] <- 'FirstSexualIntercourse'
colnames(mydata)[colnames(mydata) == 'Smokes..years'] <- 'SmokeYears'
colnames(mydata)[colnames(mydata) == 'Smokes..packs.year.'] <- 'SmokesPYears'
colnames(mydata)[colnames(mydata) == 'Dx.Cancer'] <- 'DCancer'
colnames(mydata)[colnames(mydata) == 'Dx.Cancer'] <- 'DCancer'
colnames(mydata)[colnames(mydata) == 'Dx.CIN'] <- 'DCIN'
colnames(mydata)[colnames(mydata) == 'Dx.HPV'] <- 'DHPV'

##########Subset the data###########
library(sqldf)
df <- sqldf("Select Dxx, Age, SexualPartners, Pregnancies from mydata")
View(df)


########Removing NA#############

df <- na.omit(mydata)
View(df)

#################Train and Test################
dt <- sort(sample(nrow(df), nrow(df)*.7))
dt
##########Divide Data Here##########
train <- df[dt,]
train
test <- df[-dt,]
test
########Linear##########
linear <- lm(Dxx ~ Age + SexualPartners + Pregnancies, data = train )
summary(linear)
library(tree)
tree1 <- tree(Dxx ~ Age + SexualPartners + Pregnancies, data = train)
summary(tree1)
text(tree1, pretty = 0)

library(CORElearn)
library(RWeka)
library(FSelector)

vb1 <- information.gain(Dxx ~ Age + SexualPartners + Pregnancies, data = train)
vb1
vb2 <- attrEval(Dxx ~ Age + SexualPartners + Pregnancies, data = train, estimator = "InfGain")
vb2