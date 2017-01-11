setwd("C:/Users/Baoco/Desktop/kaggle/Titanic")
titanic.train <- read.csv(file = "train.csv", stringsAsFactors = FALSE)
titanic.test <- read.csv(file = "test.csv", stringsAsFactors = FALSE)

str(titanic.train)

titanic.train$IsTrainSet <- TRUE
titanic.test$IsTrainSet <- FALSE

titanic.test$Survived <- NA

titanic.full <- rbind(titanic.train, titanic.test)

titanic.full[titanic.full$Embarked=='','Embarked'] <- 'S'

#Clean missing values of Age
age.median <- median(titanic.full$Age, na.rm = TRUE)
titanic.full[is.na(titanic.full$Age), "Age"] <- age.median

#Clean missing values of Fare
fare.median <- median(titanic.full$Fare)
titanic.full[is.na(titanic.full$Fare), 'Fare'] <- fare.median

# Categorical casting
titanic.full$Pclass <- as.factor(titanic.full$Pclass)
titanic.full$Sex <- as.factor(titanic.full$Sex)
titanic.full$Embarked <- as.factor(titanic.full$Embarked)

# Split dataset back out into train and test
titanic.train <- titanic.full[titanic.full$IsTrainSet==TRUE,]
titanic.test <- titanic.full[titanic.full$IsTrainSet==FALSE,]

titanic.train$Survived <- as.factor(titanic.train$Survived)

survived.equation <- "Survived ~ Pclass + Sex + Age + SibSp"
survived.formula <- as.formula(survived.equation)

install.packages('randomForest')
library(randomForest)

titanic.model <- randomForest(formula = survived.formula, data = titanic.train, ntree = 500, mtry = 3, nodesize = 0.01 * nrow(titanic.test))

features.equation <- "Survived ~ Pclass + Sex + Age + SibSp"
Survived <- predict(titanic.model, newdata = titanic.test)

PassengerId <- titanic.test$PassengerId
output.df <- as.data.frame(PassengerId)
output.df$Survived <- Survived

write.csv(output.df, file = "kaggle_submission.csv")
