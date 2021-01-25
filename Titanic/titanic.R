library(randomForest)
titanic_train <- read.csv("train.csv", stringsAsFactors = FALSE, header=TRUE)
titanic_test <- read.csv("test.csv", stringsAsFactors = FALSE,header=TRUE)

titanic_train$IsTrainSet = TRUE
titanic_test$IsTrainSet = FALSE 

#since we already know the test set has no survived column we need to add one for cbind
titanic_test$Survived = NA
titanic_full <- rbind(titanic_train,titanic_test)




#data cleaning by replacing all the empty age values with the median
#square bracket stands for query, and we only want to query the Age column


# median_fare <- median(titanic_full$Fare, na.rm = TRUE)
# titanic_full[is.na(titanic_full$Fare),"Fare"] <- median_fare
maxi_Age <- boxplot.stats(titanic_full$Age)$stats[5]
lm_titanic_full <- titanic_full[titanic_full$Age > maxi_Age,]
Age.equation <- "Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked"
Age.model <- lm(
  formula = Age.equation,
  data = lm_titanic_full
)
Age_row <- titanic_full[is.na(titanic_full$Age), c("Pclass","Sex","SibSp","Parch","Fare","Embarked")]
Age_predict <- predict(Age.model, newdata=Age_row)
titanic_full[is.na(titanic_full$Age), "Age"] <- Age_predict

#instead of filling all NA with median, we need to create a new model to predict what to fill NA
maxi_Fare <- boxplot.stats(titanic_full$Fare)$stats[5]
lm_titanic_full <- titanic_full[titanic_full$Fare<maxi_Fare,]  #this is the dataset without the outlier
fare.equation = "Fare ~ Pclass + Sex + Age + SibSp + Parch + Embarked"
fare.model <- lm(
  formula = fare.equation,
  data = lm_titanic_full,
)
#but we only want to predict for specific columns stated above
#and fill the predicted value into NA 
fare_row <- titanic_full[is.na(titanic_full$Fare), c("Pclass","Sex","Age","SibSp","Parch","Embarked")]
fare_prediction <- predict(fare.model, newdata = fare_row )
#if there are multiple NA, the NA will be replaced in order of fare_prediction
titanic_full[is.na(titanic_full$Fare),"Fare"] <- fare_prediction


titanic_full[titanic_full$Embarked == '', "Embarked"] <-'S'





#categorical casting, to name columns with categorical data as factor
titanic_full$Pclass <- as.factor(titanic_full$Pclass)
titanic_full$Sex <- as.factor(titanic_full$Sex)
titanic_full$Embarked <- as.factor(titanic_full$Embarked)
titanic_full$SibSp <- as.numeric(titanic_full$SibSp)

#now we split titanic_full back into 2 data sets
titanic_train <- titanic_full[titanic_full$IsTrainSet == TRUE,] #query needs to take it 2 arguments if not leave a ,
titanic_test <- titanic_full[!titanic_full$IsTrainSet == TRUE,]

#only set survive as factor after splitting because we do not want NA as factor
titanic_train$Survived <- factor(titanic_train$Survived)





#set which columns is the predictor and which to ignore 
#randomForest(Survive ~.) this will use everything other than survive to predict survive


#specify the columns to be used to predict survive
survive.equation <- "Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked"
survive.formula <- as.formula(survive.equation)


#split the training data into validation and train
set.seed(1)
val <- sample(1:2, size=nrow(titanic_train), prob=c(0.8,0.2), replace=TRUE)
titanic_train <- titanic_train[val==1,]
titanic_validation <- titanic_train[val==2,]






# start training with random forest
titanic_model <- randomForest(formula = survive.formula, data=titanic_train, ntree=500, mtry=3, nodesize = 0.01*nrow(titanic_test))


features.equation <- "Pclass + Sex + Age + SibSp + Parch + Fare + Embarked"
survived <- predict(titanic_model, newdata = titanic_test)

PassengerID <- titanic_test$PassengerId
output <- as.data.frame(PassengerID)
output$Survived <- survived

write.csv(output, file="titanic_prediction", row.names = FALSE)
