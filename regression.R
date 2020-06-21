# Loading data manipulation and visualization libraries

library(dplyr)
library(tidyr)
library(corrplot)
library(ggplot2)
library(MASS)
library(Boruta)
library(gridExtra)
library(DataExplorer)

# Additional library for regression plots

library(visreg)

# Importing Data

auto_mpg_data <- read.csv("auto-mpg.csv")
colnames(auto_mpg_data) <- c("mpg","cylinders","displacement","horsepower","weight","acceleration","model_year", "origin","car_name")

dim(auto_mpg_data)
summary(auto_mpg_data)
head(auto_mpg_data, 2)

# Cleaning Data

plot_missing(auto_mpg_data)
anyDuplicated(auto_mpg_data)

# Converting horsepower values to numerical and model_year, origin to factors

auto_mpg_data$horsepower = as.numeric(levels(auto_mpg_data$horsepower))[auto_mpg_data$horsepower]
auto_mpg_data$model_year = auto_mpg_data$model_year %>%
                           factor(labels = sort(unique(auto_mpg_data$model_year)))
auto_mpg_data$origin = auto_mpg_data$origin %>%
                       factor(labels = sort(unique(auto_mpg_data$origin)))

# Counting total null values in each column

colSums(is.na(auto_mpg_data))

# Replacing NA values with mean

auto_mpg_data$horsepower[is.na(auto_mpg_data$horsepower)] = mean(auto_mpg_data$horsepower, na.rm = T)

# Counting the occurance of each cylinder category

auto_mpg_data %>% 
  group_by(cylinders) %>% 
  count(cylinders)

# Eliminating cylinder type 3 and 5 due to very low frequencies

auto_mpg_data <- auto_mpg_data %>% 
                 filter(cylinders != 3 & cylinders != 5)

# Boxplots to identify outliers

boxplot(auto_mpg_data$cylinders, plot=TRUE)$out
boxplot(auto_mpg_data$displacement, plot=TRUE)$out
boxplot(auto_mpg_data$horsepower, plot=TRUE)$out
boxplot(auto_mpg_data$weight, plot=TRUE)$out
boxplot(auto_mpg_data$acceleration, plot=TRUE)$out

# Eliminating outliers from horsepower and acceleration

outliers <- boxplot(auto_mpg_data$horsepower, plot=FALSE)$out
auto_mpg_data <- auto_mpg_data[-which(auto_mpg_data$horsepower %in% outliers),]

outliers <- boxplot(auto_mpg_data$acceleration, plot=FALSE)$out
auto_mpg_data <- auto_mpg_data[-which(auto_mpg_data$acceleration %in% outliers),]

dim(auto_mpg_data)

# Plotting pearson correlation among numerical data

auto_mpg_data_num <- auto_mpg_data[c(1:6)]
corrplot(cor(auto_mpg_data_num))

# Acknowledging relations among numerical data via Simple Linear Regression Analyses 

# Eliminating acceleration variable due to low correlation with mpg

auto_mpg_data_reg <- auto_mpg_data[c(1:5)]

# mpg, cylinders
model_cy <- lm(auto_mpg_data_reg$mpg ~ auto_mpg_data_reg$cylinders)
summary(model_cy)

attributes(model_cy)
model_cy$coefficients

plot(auto_mpg_data_reg$cylinders, auto_mpg_data_reg$mpg, main = "Scatterplot on Mpg vs. Cylinders")
abline(model_cy, col = 2, lwd = 3)

confint(model_cy, level = 0.99)

# mpg, displacement
model_dis <- lm(auto_mpg_data_reg$mpg ~ auto_mpg_data_reg$displacement)
summary(model_dis)

attributes(model_dis)
model_dis$coefficients

plot(auto_mpg_data_reg$displacement, auto_mpg_data_reg$mpg, main = "Scatterplot on Mpg vs. Displacement")
abline(model_dis, col = 3, lwd = 3)

confint(model_dis, level = 0.99)

# mpg, horsepower
model_hor <- lm(auto_mpg_data_reg$mpg ~ auto_mpg_data_reg$horsepower)
summary(model_hor)

attributes(model_hor)
model_hor$coefficients

plot(auto_mpg_data_reg$horsepower, auto_mpg_data_reg$mpg, main = "Scatterplot on Mpg vs. Horsepower")
abline(model_hor, col = 4, lwd = 3)

confint(model_hor, level = 0.99)

# mpg, weight
model_w <- lm(auto_mpg_data_reg$mpg ~ auto_mpg_data_reg$weight)
summary(model_w)

attributes(model_w)
model_w$coefficients

plot(auto_mpg_data_reg$weight, auto_mpg_data_reg$mpg, main = "Scatterplot on Mpg vs. Weight")
abline(model_w, col = 6, lwd = 3)

confint(model_w, level = 0.99)


# Multiple Linear Regression

# Splitting the dataset into 80/20 ratio

set.seed(852456)
indexes <- sample(nrow(auto_mpg_data), (0.8*nrow(auto_mpg_data)), replace = FALSE)
auto_mpg_train <- auto_mpg_data[indexes, ]
auto_mpg_test <- auto_mpg_data[-indexes, ]

# Model1: Multiple Linear Regression Analysis on numerical variables only

model1 <- lm(mpg ~ cylinders + displacement + horsepower + weight, data = auto_mpg_train)
summary(model1)

par(mfrow = c(2,2))
plot(model1)

# Model2: Multiple Linear Regression Analysis on numerical variables except horsepower

model2 <- lm(mpg ~ cylinders + displacement + weight, data = auto_mpg_train)
summary(model2)

par(mfrow = c(2,2))
plot(model2)

# Model3: Multiple Linear Regression Analysis on all data

model3 <- lm(mpg ~ cylinders + displacement + horsepower + weight + acceleration + model_year + origin, 
             data = auto_mpg_train)
summary(model3)

par(mfrow = c(2,2))
plot(model3)

# Plotting all the variables and their characteristic nature within the model

visreg(model3)

# Predicting by model3 on test data

predictions <- predict(model3, newdata = auto_mpg_test)
df <- cbind.data.frame(auto_mpg_test$mpg, predictions)
head(df)
sqrt(mean((predictions - auto_mpg_test$mpg)^2))
