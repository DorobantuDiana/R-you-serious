# Load pre-requisite packages
pacman :: p_load(pacman, stats, dplyr, randomForest)

# Load data into "mydata" object
mydata <- iris

# Inspect data
View(mydata)

# Variable Selection
str(mydata)

# Splitting data in Train & Test
index <- sample(2,nrow(mydata), replace=T, prob=c(0.7, 0.3))

Train <- mydata[index==1,]
Test <- mydata[index==2,]

# Random Forest Model
RFM <- randomForest(Species~.,data=Train)

# Evaluating Model Accuracy
predictions <- predict(RFM,Test)
Test$predictions <- predictions
View(Test)

# Confusion Matrix
CFM <- table(Test$Species, Test$predictions)
CFM

Classification_Accuracy <- sum(diag(CFM)/sum(CFM))
Classification_Accuracy
