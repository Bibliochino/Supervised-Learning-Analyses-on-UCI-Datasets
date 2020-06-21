# Loading data manipulation and visualization libraries

library(dplyr)
library(tidyr)
library(corrplot)
library(ggplot2)
library(MASS)
library(Boruta)
library(gridExtra)
library(DataExplorer)

# Importing Data

glass_data <- read.csv("glass.csv")
glass_data$Type <- as.factor(glass_data$Type)
colnames(glass_data)[1] <- "Refractive.index"
dim(glass_data)
summary(glass_data)
head(glass_data, 2)

# Cleaning Data

plot_missing(glass_data)

anyDuplicated(glass_data)
glass_data <- glass_data[!duplicated(glass_data),]

plot_bar(glass_data)
plot_histogram(glass_data)

# Finding pearson correlation among all variables except the target with Refractive Index

glass_data_not_type <- glass_data[, which(!(names(glass_data) %in% 'Type'))]
corrplot(cor(glass_data_not_type))

boxplot(glass_data$Refractive.index ~ glass_data$Type)

# Aggregating frequencies of all elements from data

elements_data <- glass_data[c(2:9)]
element_freq <- apply(elements_data!=0, 2, sum)
element_freq

# Plotting frequencies with a barplot

barplot(element_freq)
abline(h=nrow(glass_data), lty=2, col=2)

# Table to describe frequencies of each element within each Type of glass

(glass_data[, which(!(names(glass_data) %in% c('Refractive.index', 'Type')))] > 0) %>% 
  as.data.frame() %>% 
  cbind(Type=glass_data$Type) %>% 
  group_by(Type) %>% 
  summarise(Na=sum(Na), Mg=sum(Mg), Al=sum(Al), Si=sum(Si), K=sum(K), Ca=sum(Ca), Ba=sum(Ba), Fe=sum(Fe))

# Classification

# Decision Trees

library(rpart) # Recursive Partitioning and Regression Tress R library for Classification (CART) Algorithm
library(caret) # Easy Machine Learning Workflow
library(e1071) # Additional for model building
library(maptree) # Decision Tree graphics

# Splitting dataset into 70% training data and remaining 30% testing data

set.seed(123)
training.samples <- glass_data$Type %>%
                    createDataPartition(p = 0.7, list = FALSE)
glass_data_train <- glass_data[training.samples,]
glass_data_test <- glass_data[-training.samples,]

# Model1: Basic Decision Tree

model1 <- rpart(Type ~., data = glass_data_train, method = "class")

draw.tree(model1, cex = 1.05)

predicted.classes <- model1 %>%
                     predict(glass_data_test, type = "class")
head(predicted.classes)

mean(predicted.classes == glass_data_test$Type)

confusionMatrix(predicted.classes, glass_data_test$Type)

# Model2: Pruning Decision Tree with caret, setting 10fold cross validation with 50 complexity parameter values 

set.seed(753951)
model2 <- train(Type ~., data = glass_data_train, method = "rpart",
                trControl = trainControl("cv", number = 10),
                tuneLength = 50
)
plot(model2)

# Picking the optimum cp value 

model2$bestTune

draw.tree(model2$finalModel, cex = 1.05)

model2$finalModel

pred.classes <- model2 %>% 
                predict(glass_data_test)

mean(pred.classes == glass_data_test$Type)

confusionMatrix(pred.classes, glass_data_test$Type)

# Changing the splitting ratio to 60/40 for better accuracy

set.seed(951357)
training.samples <- glass_data$Type %>%
  createDataPartition(p = 0.6, list = FALSE)
glass_data_train <- glass_data[training.samples,]
glass_data_test <- glass_data[-training.samples,]

# Running it back within the previous model

set.seed(654852)
model2b <- train(Type ~., data = glass_data_train, method = "rpart",
                trControl = trainControl("cv", number = 10),
                tuneLength = 50
)
plot(model2b)

model2b$bestTune

draw.tree(model2b$finalModel, cex = 1.05)

model2b$finalModel

pred.classes <- model2b %>% 
                predict(glass_data_test)

mean(pred.classes == glass_data_test$Type)

confusionMatrix(pred.classes, glass_data_test$Type)


# k Nearest Neighbours

library(class) # Functions for kNN classification

# k=1, target = Type, source = all other columns
glass_type <- glass_data_train$Type
knn1 <- knn(glass_data_train[-10], glass_data_test[-10], cl = glass_type)

mean(knn1 == glass_data_test$Type)

confusionMatrix(knn1, glass_data_test$Type)

# Splitting the dataset into 70/30 ratio for better accuracy in prediction

set.seed(1234)
ind<-sample(2,nrow(glass_data),prob=c(0.7,0.3),replace=TRUE)
glass_data_train<-glass_data[ind==1,]
glass_data_test<-glass_data[ind==2,]

# Applying on same knn model

glass_type <- glass_data_train$Type
knn1 <- knn(glass_data_train[-10], glass_data_test[-10], cl = glass_type)

mean(knn1 == glass_data_test$Type)
confusionMatrix(knn1, glass_data_test$Type)

# k=10
knn10 <- knn(glass_data_train[-10], glass_data_test[-10], cl = glass_type, k = 10)

mean(knn10 == glass_data_test$Type)
confusionMatrix(knn10, glass_data_test$Type)

# Applying 10fold Cross Validation to find the optimum k

mycontrol<-trainControl(method="cv",number=10)
model_knn <- train(Type ~., data=glass_data_train, method="knn", metric="Accuracy", trControl=mycontrol)
model_knn

# k=7
knn7 <- knn(glass_data_train[-10], glass_data_test[-10], cl = glass_type, k = 7)

mean(knn7 == glass_data_test$Type)
confusionMatrix(knn7, glass_data_test$Type)

