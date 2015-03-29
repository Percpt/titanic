# Evernotes:
#  Kaggle Titanic Tutorial Part 1: Start w/ R
#  Kaggle Titanic Tutorial Part 2: A Gender-Class Model
#  Kaggle Titanic Tutorial Part 3: Decision Trees
#  Kaggle Titanic Tutorial Part 4: Feature Engineering
#  Kaggle Titanic Tutorial Part 5: Random Forests



# Setup data set ----------------------------------------------------------
setwd('~/Desktop/titanic/')
train <- read.csv("~/Desktop/titanic/train.csv") # Import training data
test <- read.csv("~/Desktop/titanic/test.csv")   # Import test data


# Part 1: Starting w/ R ---------------------------------------------------
#
# Functions learned:
#     View
#     str
#     table
#     prop.table(table(...))
#     summary

# Checking out the dataframe structure
View(train) # Preview the dataset 
str(train)

table(train$Survived)
prop.table(table(train$Survived))


# Everyone dies prediction
test$Survived <- rep(0, 418) # Simple prediction: everyone dies
# Create dataframe in the appropriate format + save to file
submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = "theyallperish.csv", row.names = FALSE)
View(submit)



# Sex and Age factors
summary(train$Sex) # view sex data
prop.table(table(train$Sex, train$Survived))
prop.table(table(train$Sex, train$Survived), 1) # Row-wise proportions
prop.table(table(train$Sex, train$Survived), 2) # Col-wise proportions

# Updating predictions based on male/female survival rates
# ~74% of all females survived
# ~19% of all males   survived

test$Survived <- 0
test$Survived[test$Sex == 'female'] <- 1
View(test)

submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = "jfaFemalesSurvive.csv", row.names = FALSE)
View(submit)

# Age
summary(train$Age)

# Convert continuous variable (AGE) to categorical variable (CHILD)
train$Child <- 0
train$Child[train$Age < 18] <- 1
View(train)

# Create a table with both gender and age to see survival proportions for 
# different subsets
aggregate(Survived ~ Child + Sex, data = train, FUN = sum)

# Create a table with both gender and age to see the total number of people in 
# each subset
aggregate(Survived ~ Child + Sex, data = train, FUN = length)

# Create a table with GENDER and AGE to see the PROPORTION of survivors
aggregate(Survived ~ Child + Sex, data = train, FUN = function(x) {sum(x)/length(x)})


# Bin the fares into <$10, $10-$20, $20-$30, and >$30
train$Fare2 <- '30+'
train$Fare2[train$Fare < 10]  <- '<10'
train$Fare2[train$Fare >= 10 & train$Fare < 20] <- '10-20'
train$Fare2[train$Fare >= 20 & train$Fare < 30] <- '20-30'
View(cbind(train$Fare, train$Fare2))

aggregate(Survived ~ Fare2 + Pclass + Sex, data = train, 
          FUN = function(x){sum(x)/length(x)})

test$Survived <- 0
test$Survived[test$Sex == 'female'] <- 1
test$Survived[test$Sex == 'female' & test$Pclass == 3 & test$Fare >= 20] <- 0
submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = "jfaMostFemalesSurvive.csv", row.names = FALSE)
View(submit)


# Part 3: Decision Trees --------------------------------------------------
# Functions:
#   rpart         - Recursive partitioning & Regression Trees package
#     rpart
#   rattle        - plotting decision trees
#     fancyRpartPlot
#   rpart.plot    - plotting decision trees
#   RColorBrewer  - plotting decision trees

# Setup data set 
setwd('~/Desktop/titanic/')
train <- read.csv("~/Desktop/titanic/train.csv") # Import training dataset
test <- read.csv("~/Desktop/titanic/test.csv")   # Import test dataset

# Install better display packages
# install.packages('rattle')
# install.packages('rpart.plot')
# install.packages('RColorBrewer')
library(rpart) # Import 'Recursive partitioning and Regression Trees' module
library('rattle')
library('rpart.plot')
library('RColorBrewer')


fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, 
             data = train, 
             method = "class")
plot(fit)
text(fit)




# Better plot decision tree
fancyRpartPlot(fit)

# Create prediction to submit
Prediction <- predict(fit, test, type = "class")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "jfaMyfirstdtree.csv", row.names = FALSE)


# Remove RPART limits 
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, 
             data = train, 
             method = "class", 
             control = rpart.control(minsplit = 2, cp = 0))
fancyRpartPlot(fit)

Prediction <- predict(fit, test, type = "class")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "jfaDtreeNoLimits.csv", row.names = FALSE)


# Playing around with decision tree parameters
fit <- rpart(Survived ~ Sex + Age + Pclass + SibSp + Parch + Fare + Embarked,
             data = train,
             method = "class",
             control = rpart.control(minsplit       = 1,
                                     cp             = 0.007,
                                     maxcompete     = 4,
                                     surrogatestyle = 1,
                                     maxdepth       = 6
                                     ))
fancyRpartPlot(fit)

Prediction <- predict(fit, test, type = "class")
submit     <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "jfaDtreePlay.csv", row.names = FALSE)


# Part 4: Feature Engineering Section ---------------------------------------------
test$Survived <- NA
combi <- rbind(train, test)

combi$Name <- as.character(combi$Name) # cast the factor var NAME as chars
combi$Name[1]

# To break apart a string, we need hooks to tell the program to look for.  There is a comma right after the person’s last name, and a full stop after their title. Use the function strsplit, which stands for string split, to break apart our original name over these two symbols.

strsplit(combi$Name[1], split = '[,.]') # split on either commas or periods
strsplit(combi$Name[1], split = '[,.]')[[1]]
strsplit(combi$Name[1], split = '[,.]')[[1]][2]

combi$Title <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
combi$Title <- sub(' ', '', combi$Title)

table(combi$Title)

# Reduce the number of factor levels (i.e. redundancies)
combi$Title[combi$Title %in% c('Mme','Mlle')] <- 'Mlle' # combine Mme and Mlle into single category
combi$Title[combi$Title %in% c('Capt','Don','Major','Sir')] <- 'Sir'
combi$Title[combi$Title %in% c('Dona','Jonkheer','Lady','the Countess')] <- 'Lady'

# Change title strings back to factors
combi$Title <- factor(combi$Title)

combi$FamilySize <- combi$SibSp + combi$Parch + 1
combi$Surname <- sapply(combi$Name, 
                        FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})
combi$FamilyID <- paste(as.character(combi$FamilySize), combi$Surname, sep="")

# we were originally hypothesising that large families might have trouble
# sticking together in the panic, let’s knock out any family size of two or less
# and call it a “small” family

combi$FamilyID[combi$FamilySize <= 2] <- 'Small'
table(combi$FamilyID)

# There’s plenty of FamilyIDs with only one or two members, even though we
# wanted only family sizes of 3 or more. Perhaps some families had different
# last names, but whatever the case, all these one or two people groups is what
# we sought to avoid with the three person cut-off.

famIDs <- data.frame(table(combi$FamilyID))
View(famIDs)

famIDs <- famIDs[famIDs$Freq <= 2,]
combi$FamilyID[combi$FamilyID %in% famIDs$Var1] <- 'Small'
combi$FamilyID <- factor(combi$FamilyID)


train  <- combi[1:891,]
test <- combi[892:1309,]

fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + 
               Title + FamilySize + FamilyID,
             data = combi,
             method = "class")
fancyRpartPlot(fit)

Prediction <- predict(fit, test, type = "class")
submit     <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file="jfaFeatureEngineeredPredictions.csv", row.names=FALSE)


# Part 5: Random Forests --------------------------------------------------





# Notes -------------------------------------------------------------------
# VARIABLE DESCRIPTIONS:
# survival        Survival         (0 = No; 1 = Yes)
# pclass          Passenger Class  (1 = 1st; 2 = 2nd; 3 = 3rd)
# name            Name
# sex             Sex
# age             Age
# sibsp           Number of Siblings/Spouses Aboard
# parch           Number of Parents/Children Aboard
# ticket          Ticket Number
# fare            Passenger Fare
# cabin           Cabin
# embarked        Port of Embarkation
# (C = Cherbourg; Q = Queenstown; S = Southampton)
# 
# SPECIAL NOTES:
#   Pclass is a proxy for socio-economic status (SES)
# 1st ~ Upper; 2nd ~ Middle; 3rd ~ Lower
# 
# Age is in Years; Fractional if Age less than One (1)
# If the Age is Estimated, it is in the form xx.5
# 
# With respect to the family relation variables (i.e. sibsp and parch)
# some relations were ignored.  The following are the definitions used
# for sibsp and parch.
# 
# Sibling:  Brother, Sister, Stepbrother, or Stepsister of Passenger Aboard 
# Titanic
#
# Spouse:   Husband or Wife of Passenger Aboard Titanic (Mistresses and Fiances
# Ignored)
#
# Parent:   Mother or Father of Passenger Aboard Titanic
# Child:    Son, Daughter, Stepson, or Stepdaughter of Passenger Aboard Titanic
# 
# Other family relatives excluded from this study include cousins,
# nephews/nieces, aunts/uncles, and in-laws.  Some children travelled
# only with a nanny, therefore parch=0 for them.  As well, some
# travelled with very close friends or neighbors in a village, however,
# the definitions do not support such relations.
