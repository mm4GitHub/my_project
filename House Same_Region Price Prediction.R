---
title: "House Same_Region Price Prediction"
author: "Lawal Mariam Omobolanle, 2417893, DAT7303"
date: "`r Sys.Date()`"
output: html_document
---

```{r}
#Install and Run needed Libraries

install.packages("tidyverse")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("reshape2")
install.packages("corrplot")
install.packages("Metrics")
install.packages("caret")
install.packages("rpart")
install.packages("randomForest")
install.packages("e1071")

library(tidyverse)
library(dplyr)
library(corrplot)
library(ggplot2)
library(reshape2)
library(Metrics)
library(caret)
library(rpart)
library(randomForest)
library(e1071)
```

```{r}
#Specifying and Reading the CSv file into a data frame
file_path <- "C:/Users/HP/Desktop/ASSIGNMENT/HousingDataSameRegion.csv"

Housingdata <- read.csv(file_path)
```

```{r}
#Check for the structure of the data frame
str(Housingdata)
```

```{r}
#Check for the number of rows
num_rows <- nrow(Housingdata)
print(num_rows)
```

```{r}
#load first 6 Data from the Data Frame
head(Housingdata)
```

```{r}
#Code to drop Latitude and Longitude Columns
Housingdata1 <- Housingdata %>% select(-c("LATITUDE", "LONGITUDE"))
print(Housingdata1)
```

```{r}
#Code to move Price column to the last column and check 
Housingdata2 <- Housingdata1 %>%
 relocate(SALE_PRC, .after = last_col())

print(Housingdata2)
```

```{r}
#Check for correlated columns
cor_values <- cor(Housingdata2[, -15], Housingdata2[, 15], use = "complete.obs")
cor_values_matrix <- as.matrix(cor_values)#code convert to matrix for heatmap

cor_data <- melt(cor_values_matrix)

ggplot(cor_data, aes(x = Var2, y = Var1, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "darkblue", high = "red", mid = "white" , midpoint = 0) +
  theme_minimal() +
  labs(title = "Features that correlate with Sales Price", fill = "correlation") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
```

```{r}
#Dropping columns that are least correlation with the dataset and do not impact Housing Prediction
HousingDataNew_cleaned <- Housingdata2 %>% select(-c(PARCELNO, RAIL_DIST, OCEAN_DIST, WATER_DIST, CNTR_DIST, SUBCNTR_DI, age,avno60plus, month_sold))
print(HousingDataNew_cleaned)
```

```{r}
#Inspect the structure of the cleaned dataset after dropping some coulmns
str(HousingDataNew_cleaned)
```

```{r}
#Checking the summary statistics 
summary(HousingDataNew_cleaned)
```

```{r}
# Plot histograms for all numeric columns
# to check if the distribution is skew or not, and if there is outliers. 
par(mfrow = c(3, 2))  
for (col_name in colnames(data)) {
  if (is.numeric(HousingDataNew_cleaned[[col_name]])) {
    hist(data[[col_name]], 
         main = paste("Histogram of", col_name), 
         xlab = col_name, 
         col = "lightblue", 
         breaks = 20) 
  }
}
```

```{r}
#Checking for missing values before carrying out analysis
sum(is.na(HousingDataNew_cleaned))
```

```{r}
# Check for zero values in each column
colSums(HousingDataNew_cleaned == 0)
```

```{r}
#Detecting Outliers using IQR
detect_outliers <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  return(x < lower_bound | x > upper_bound)
}

#Detect outliers in each numeric column
outliers <- sapply(HousingDataNew_cleaned, function(x) {
  if (is.numeric(x)) detect_outliers(x) else rep(FALSE, length(x))
})

#Plot box plots for numeric columns
par(pin = c(1, 0.5), mfrow = c(3, 2))  # Arrange plots in a grid
for (col_name in colnames(HousingDataNew_cleaned)) {
  if (is.numeric(HousingDataNew_cleaned[[col_name]])) {
    boxplot(HousingDataNew_cleaned[[col_name]], main = paste("Boxplot of", col_name), ylab = col_name, col = "Green")
  }
}
```

```{r}
#Train with the dataset
#Split the dataset into Train and Test dataset
set.seed(123)
#The dataset is split in train(70%) and test(30%) set
trainIndex <- createDataPartition(HousingDataNew_cleaned$SALE_PRC, p = 0.7, list = FALSE)
trainHousingData <- HousingDataNew_cleaned[trainIndex, ]
testHousingData <- HousingDataNew_cleaned[-trainIndex, ]

```

```{r}
#view train dataset
print(trainHousingData)
```


```{r}
#view test dataset
print(testHousingData)
```

```{r}
#Fitting the models on the training data
# Train multiple models
#Training traindataset using Linear Regression Model
lm_model <- lm(SALE_PRC ~., data = trainHousingData)
summary(lm_model)
```

```{r}
#making prediction on the test dataset
lm_predictions <- predict(lm_model, newdata = testHousingData)
print(lm_predictions)
```

```{r}
#Calculating the performance metrics
lm_performance <- postResample(pred = lm_predictions, obs = testHousingData$SALE_PRC)
print(lm_performance)
```

```{r}
#Training Using Support Vector Regression Model
svr_linear <- svm(SALE_PRC ~., data = trainHousingData, kernel = "linear")
print(svr_linear)
```

```{r}
#making predictions on test data 
svr_linearPredictions <- predict(svr_linear, newdata = testHousingData)
print(svr_linearPredictions)
```

```{r}
#Calculating the performance metrics
svr_linearPerformance <- postResample(pred = svr_linearPredictions, obs = testHousingData$SALE_PRC)
print(svr_linearPerformance)
```

```{r}
#Training Using Radial Basis Functions(RBF)
svr_radial <- svm(SALE_PRC ~., data = trainHousingData, kernel = "radial")
print(svr_radial)
```

```{r}
#making predictions on test data
svr_radialPredictions <- predict(svr_radial, newdata = testHousingData)
print(svr_radialPredictions)
```

```{r}
#Calculating the performance metrics
svr_radialPerformance <- postResample(pred = svr_radialPredictions, obs = testHousingData$SALE_PRC)
print(svr_radialPerformance)
```

```{r}
#Training Using Polynomial
svr_polynomial <- svm(SALE_PRC ~., data = trainHousingData, kernel = "polynomial")
print(svr_polynomial)
```

```{r}
#making predictions
svr_polynomialPredictions <- predict(svr_polynomial, newdata = testHousingData)
print(svr_polynomialPredictions)
```

```{r}
#Calculating the performance metrics
svr_polynomialPerformance <- postResample(pred = svr_polynomialPredictions, obs = testHousingData$SALE_PRC)
print(svr_polynomialPerformance)
```

```{r}
#Training Using Decision Tree
dt_model <- rpart(SALE_PRC ~., data = trainHousingData, )
print(dt_model)
```

```{r}
#making predictions
dt_predictions <- predict(dt_model, newdata = testHousingData)
print(dt_predictions)
```

```{r}
#Calculating the performance metrics
dt_performance <- postResample(pred = dt_predictions, obs = testHousingData$SALE_PRC)
print(dt_performance)
```

```{r}
#Training Using RandomForest
#For RandomForest = 100
rf_model_n100 <- randomForest(SALE_PRC ~., data = trainHousingData, ntree = 100)
print(rf_model_n100)
```

```{r}
#making predictions
rf_predictions_n100 <- predict(rf_model_n100, newdata = testHousingData)
print(rf_predictions_n100)
```

```{r}
#Calculating the performance metrics
rf_performance_n100 <- postResample(pred = rf_predictions_n100, obs = testHousingData$SALE_PRC)
print(rf_performance_n100)
```

```{r}
#For RandomForest = 200
rf_model_n200 <- randomForest(SALE_PRC ~., data = trainHousingData, ntree = 200)
print(rf_model_n200)
```

```{r}
#making predictions
rf_predictions_n200 <- predict(rf_model_n200, newdata = testHousingData)
print(rf_predictions_n200)
```

```{r}
#Calculating the performance metrics
rf_performance_n200 <- postResample(pred = rf_predictions_n200, obs = testHousingData$SALE_PRC)
print(rf_performance_n200)
```

```{r}
#For RandomForest = 500
rf_model_n500 <- randomForest(SALE_PRC ~., data = trainHousingData, ntree = 500)
print(rf_model_n500)
```

```{r}
#making predictions
rf_predictions_n500 <- predict(rf_model_n500, newdata = testHousingData)
print(rf_predictions_n500)
```

```{r}
#Calculating the performance metrics
rf_performance_n500 <- postResample(pred = rf_predictions_n500, obs = testHousingData$SALE_PRC)
print(rf_performance_n500)
```

```{r}
#Combine performance metrics into a data frame
performance_data <- data.frame(
  Model = c("Linear Regression", "SVR Linear", "SVR Radial", "SVR Polynomial", 
            "Decision Tree", "Random Forest (100)", "Random Forest (200)", "Random Forest (500)"),
  RMSE = c(lm_performance["RMSE"], svr_linearPerformance["RMSE"], svr_radialPerformance["RMSE"],
           svr_polynomialPerformance["RMSE"], dt_performance["RMSE"], rf_preformance_n100["RMSE"],
           rf_preformance_n200["RMSE"], rf_preformance_n500["RMSE"]),
  Rsquared = c(lm_performance["Rsquared"], svr_linearPerformance["Rsquared"], svr_radialPerformance["Rsquared"],
               svr_polynomialPerformance["Rsquared"], dt_performance["Rsquared"], rf_preformance_n100["Rsquared"],
               rf_preformance_n200["Rsquared"], rf_preformance_n500["Rsquared"]),
  MAE = c(lm_performance["MAE"], svr_linearPerformance["MAE"], svr_radialPerformance["MAE"],
          svr_polynomialPerformance["MAE"], dt_performance["MAE"], rf_preformance_n100["MAE"],
          rf_preformance_n200["MAE"],rf_preformance_n500["MAE"])
)
# Print the performance data
print(performance_data)
```

```{r}
#plot the graph of the Errors to view the performance
# Plot RMSE
ggplot(performance_data, aes(x = Model, y = RMSE, fill = Model)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Comparison of RMSE Across Models", x = "Model", y = "RMSE")

```

```{r}
# Plot R-squared
ggplot(performance_data, aes(x = Model, y = Rsquared, fill = Model)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Comparison of R-squared Across Models", x = "Model", y = "R-squared")

```

```{r}
# Plot MAE
ggplot(performance_data, aes(x = Model, y = MAE, fill = Model)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Comparison of MAE Across Models", x = "Model", y = "MAE")

```

```{r}
# Identify the model with the least RMSE
best_model <- performance_data[which.min(performance_data$RMSE), ]
cat("\nModel with the least RMSE:\n")
print(best_model)
```

```{r}
#Checking for accuracy on the RandomForest model with hyperparameters tuning techniques such as 
#Grid Search, Random Search and Bayesian Optimization techniques
#Train RandomForest using Random Search techniques
# Define the parameter space for Random Search
param_space <- list(
  mtry = sample(2:5, 10, replace = TRUE),  #Randomly sample 'mtry' values
  ntree = sample(c(100, 200, 500), 10, replace = TRUE)  #Randomly sample 'ntree' values
)

# Train the Random Forest model with Random Search
set.seed(123)
rf_model_random <- train(
  SALE_PRC ~ .,
  data = trainHousingData,
  method = "rf",
  trControl = trainControl(
    method = "cv",
    number = 5,
    search = "random"   #Use Random Search
  ),
  tuneLength = 10       #Number of random combinations to try
)

# Print the best parameters
print(rf_model_random$bestTune)
```

```{r}
# Evaluate the model on the test set
predictions_random <- predict(rf_model_random, newdata = testHousingData)
performance_random <- postResample(pred = predictions_random, obs = testHousingData$SALE_PRC)
print(performance_random)
```

```{r}
# Train the Random Forest model with Grid Search
set.seed(123)
rf_model <- train(
  SALE_PRC ~ .,                # Formula: Predict SALE_PRC using all other variables
  data = trainHousingData,
  method = "rf",               # Random Forest
  trControl = trainControl(
    method = "cv",             # Cross-validation
    number = 5                 # 5-fold cross-validation
  ),
  tuneGrid = tuneGrid          # Hyperparameter grid
)

# Print the best parameters
print(rf_model$bestTune)
```

```{r}
# Evaluate the model on the test set
predictions <- predict(rf_model, newdata = testHousingData)
performance <- postResample(pred = predictions, obs = testHousingData$SALE_PRC)
print(performance)
```

```{r}
#Comparing the performance metrics of the models train using grid search and random search tuning method
# Combine performance metrics into a data frame
results <- data.frame(
  Method = c("Grid Search", "Random Search"),
  RMSE = c(performance["RMSE"], performance_random["RMSE"]),
  Rsquared = c(performance["Rsquared"], performance_random["Rsquared"]),
  MAE = c(performance["MAE"], performance_random["MAE"])
)

# Print the results
print(results)
```

```{r}
#Plot the results
ggplot(results, aes(x = Method, y = RMSE, fill = Method)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(title = "Comparison of RMSE Across Tuning Methods", x = "Method", y = "RMSE")

```

```{r}
# Save the optimal trained model(Random Forest)
saveRDS(rf_model, "random_forest_model.rds")
```

```{r}
#Using the Optimal Tuning method(Grid Search) to predict price for the below input
#PARCELNO: 728980145245, LND_SQFOOT: 11247, TOTLVGAREA: 4552, SPECFEATVAL: 2105, RAIL_DIST:
#4871.9, OCEAN_DIST: 18507.2, WATER_DIST:375.8, CNTR_DIST: 43897.9, SUBCNTR_DI: 40115.7,
#HWY_DIST: 41917.1, age: 42, avno60plus: 0, structure_quality: 5, month_sold: 8

#The dataset is reduced to the number of column used to build the models
#LND_SQFOOT: 11247,TOTLVGAREA: 4552,SPECFEATVAL: 2105,HWY_DIST: 41917.1,structure_quality: 5

#Create a new data frame for the dataset
new_dataset <- data.frame(
  LND_SQFOOT = 11247,
  TOT_LVG_AREA = 4552,
  SPEC_FEAT_VAL = 2105,
  HWY_DIST = 41917.1,
  structure_quality = 5
)
# Predict the price using the Grid Search tuning method
predicted_price <- predict(rf_model, newdata = new_dataset)
print(predicted_price)

```




