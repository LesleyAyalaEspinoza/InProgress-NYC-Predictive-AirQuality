

#Code

data(airquality)
head(airquality)


#EDA

hist(airquality$Ozone, main="Distribution of Ozone Levels", xlab="Ozone Levels")
hist(airquality$Temp, main="Distribution of Temperature", xlab="Temperature")

hist(airquality$Ozone)

install.packages("ggplot2")

library(ggplot2)

ggplot(airquality, aes(x = factor(Month), y = Ozone)) +  # Use 'Month' as the categorical variable
  geom_boxplot(fill = "lightblue") +
  labs(title = "Ozone Distribution by Month", x = "Month", y = "Ozone Levels") +
  theme_minimal()

# Histogram for Temperature
ggplot(airquality, aes(x = Temp)) +
  geom_histogram(binwidth = 1, fill = "grey", color = "black") +
  labs(title = "Distribution of Temperature", x = "Temperature", y = "Frequency") +
  theme_minimal()

#Clean the data up , get rid of rows with empty values
na.omit(airquality)

#Linear Regression

# Remove rows with missing values
airquality_clean <- na.omit(airquality)


# Calculate IQR for Wind
Q1 <- quantile(airquality_clean$Wind, 0.25, na.rm = TRUE)
Q3 <- quantile(airquality_clean$Wind, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1

# Define lower and upper bounds
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

# Identify outliers
outliers_wind <- airquality_clean$Wind[airquality_clean$Wind < lower_bound | airquality_clean$Wind > upper_bound]
outliers_wind

upper_bound <- Q3 + 1.5 * IQR
# Remove outliers in Wind
airquality_no_outliers <- airquality_clean[!(airquality_clean$Wind < lower_bound | airquality_clean$Wind > upper_bound), ]


# Set seed for reproducibility
set.seed(123)

# Create an 90% sample for training
subset <- sample(nrow(airquality_no_outliers), nrow(airquality_no_outliers) * 0.9)

# Split the data
train_data <- airquality_no_outliers[subset, ]
test_data <- airquality_no_outliers[-subset, ]

# Fit the new linear regression model
model_no_outliers <- lm(Ozone ~ Temp + Wind + Solar.R, data = train_data)

# View the model summary
summary(model_no_outliers)

model_summary <- summary(model_no_outliers)
(model_summary$sigma)^2

AIC(model_no_outliers)
BIC(model_no_outliers)
# out of sample
# Predicted values for the test set
pi <- predict(object = model_no_outliers, newdata = test_data)

mean((pi - test_data$Ozone)^2)
model_summary$r.squared

#Variable Selection
# Model with all predictors (full model)
model_1 <- lm(Ozone ~ Temp + Solar.R + Wind, data = train_data)

# Model with a subset of predictors (reduced model, example: Temp and Solar.R only)
model_2 <- lm(Ozone ~ Temp + Solar.R, data = train_data)

# Summarize both models
summary(model_1)  # Full model
summary(model_2)  # Reduced model

# Compare models using AIC (Akaike Information Criterion)
AIC(model_1)
AIC(model_2)

plot(model_1)

#Regression Tree
install.packages('rpart')
install.packages('rpart.plot')

library(rpart)
library(rpart.plot)

airquality_rpart <- rpart(formula = Ozone ~ Temp + Solar.R + Wind, 
                          data = airquality_no_outliers)

airquality_rpart

prp(airquality_rpart,digits = 4, extra = 1)

#insample
airquality_pred_tree = predict(airquality_rpart, newdata = train_data)

#out of sample 
airquality_pred_tree_out_sample = predict(airquality_rpart, newdata = test_data)


 # Calculate in sample MSE for the regression tree model
MSE.tree <- mean((airquality_pred_tree_out_sample - test_data$Ozone)^2)

# 1. In-sample prediction for the regression tree (on training data)
airquality_pred_tree_in_sample = predict(airquality_rpart, newdata = train_data)

# 2. Calculate in-sample MSE
MSE.tree_in_sample <- mean((airquality_pred_tree_in_sample - train_data$Ozone)^2)

# 3. Out-of-sample prediction for the regression tree (on test data)
airquality_pred_tree_out_sample = predict(airquality_rpart, newdata = test_data)

# 4. Calculate out-of-sample MSE
MSE.tree_out_sample <- mean((airquality_pred_tree_out_sample - test_data$Ozone)^2)

# 5. Print both MSE values
print(paste("In-sample MSE:", MSE.tree_in_sample))
print(paste("Out-of-sample MSE:", MSE.tree_out_sample))

# random tree
install.packages("randomForest")
library(randomForest)
# Train a Random Forest model
random_forest_model <- randomForest(Ozone ~ Temp + Solar.R + Wind, 
                                    data = train_data, 
                                    ntree = 100, # Number of trees (you can adjust this)
                                    mtry = 2,    # Number of variables to consider for splitting at each node
                                    importance = TRUE)  # To assess the importance of each feature

# View the model summary
print(random_forest_model)

#in sample 
airquality_pred_rf_in_sample = predict(random_forest_model, newdata = train_data)

#out of sample prediction
airquality_pred_rf_out_sample = predict(random_forest_model, newdata = test_data)

# In-sample MSE
MSE_rf_in_sample <- mean((airquality_pred_rf_in_sample - train_data$Ozone)^2)

# Out-of-sample MSE
MSE_rf_out_sample <- mean((airquality_pred_rf_out_sample - test_data$Ozone)^2)

# Print MSE results
print(paste("In-sample MSE (Random Forest):", MSE_rf_in_sample))
print(paste("Out-of-sample MSE (Random Forest):", MSE_rf_out_sample))


