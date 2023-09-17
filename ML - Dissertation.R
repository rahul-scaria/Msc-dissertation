install.packages("caTools")
install.packages("Rtools")
install.packages("randomForest") 
install.packages("rpart")   
install.packages("rpart.plot")  
install.packages("e1071")  
install.packages("caret")  
install.packages('ROCR')
library(caTools)
library("Rtools")
library(randomForest)
library(rpart)
library(rpart.plot)
library(e1071)
library(caret)
library(randomForest)
library(caret)
library('ROCR')

############## Performing ML classification on heart failure dataset ################


#### A. MODELS WITHOUT CROSS VALIDATION ###################


#####1. Performing logistic regression ###################

set.seed(1) # for reproducibility of results
hf_data$DEATH_EVENT <- factor(hf_data$DEATH_EVENT)
split <- sample.split(hf_data$DEATH_EVENT, SplitRatio = 0.7) # 70% for training, 30% for testing
train_data <- subset(hf_data, split == TRUE)
test_data <- subset(hf_data, split == FALSE)


logit_model <- glm(DEATH_EVENT ~ ., data = train_data, family = binomial)
summary(logit_model)

predictions <- predict(logit_model, newdata = test_data, type = "response")


conf_matrix <- table(predictions > 0.5, test_data$DEATH_EVENT)
conf_matrix

# Calculate accuracy
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
accuracy

######## 2. Performing Decision trees #########################

decision_tree_model <- rpart(DEATH_EVENT ~ ., data = train_data, method = "class")

rpart.plot(decision_tree_model)

??rpart()

predictions <- predict(decision_tree_model, newdata = test_data, type = "class")


conf_matrix <- table(predictions, test_data$DEATH_EVENT)
conf_matrix

# Calculate accuracy
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
accuracy


######## 3. Performing Random forest ###############################

random_forest_model <- randomForest(DEATH_EVENT ~ ., data = train_data)

predictions <- predict(random_forest_model, newdata = test_data)

conf_matrix <- table(predictions, test_data$DEATH_EVENT)
conf_matrix

# Calculate accuracy
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
accuracy

######### 4. Performing SVM ##############################

# Assuming all the predictor variables are stored in a single data frame named 'train_data'
svm_model <- svm(DEATH_EVENT ~ ., data = train_data, kernel = "linear", probability = TRUE)

predictions <- predict(svm_model, newdata = test_data)

conf_matrix <- table(predictions, test_data$DEATH_EVENT)
conf_matrix

# Calculate accuracy
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
accuracy



####### B. MODELS WITH CROSS VALIDATION #######################


#### 1. Logistic Regression ####################

set.seed(123) 
split <- sample.split(hf_data$DEATH_EVENT, SplitRatio = 0.7) 
train_data <- subset(hf_data, split == TRUE)
test_data <- subset(hf_data, split == FALSE)


k <- 5

# Specify the control parameters for cross-validation
ctrl <- trainControl(method = "cv", number = k)

# Perform cross-validation with logistic regression (glm)
cv_model_lr <- train(DEATH_EVENT ~ ., data = train_data, method = "glm", family = "binomial", trControl = ctrl)

# Print cross-validation results
print(cv_model_lr)

# Access the overall cross-validation error
cv_error_lr <- cv_model_lr$results

cv_error_lr

#Accessing the coefficients

print(summary(cv_model_lr$finalModel))

coefficients_lr <- coef(cv_model_lr$finalModel)

print(coefficients_lr)

# Make predictions on the test data
predictions <- predict(cv_model_lr, newdata = test_data, type = "response")
print(predictions)

# Create a prediction object
prediction_obj <- prediction(as.numeric(predictions), test_data$DEATH_EVENT)


# Create confusion matrix
confusion <- confusionMatrix(predictions, test_data$DEATH_EVENT)

# Calculate performance metrics
accuracy <- confusion$overall["Accuracy"]
precision <- confusion$positivePredictiveValue
recall <- confusion$sensitivity
f1_score <- 2 * (precision * recall) / (precision + recall)

# Calculate AUC
auc_object_lr <- performance(prediction_obj, measure = "auc")
auc_lr <- auc_object_lr@y.values[[1]]

# Print the results
cat("Accuracy:", accuracy, "\n")
cat("Precision:", precision, "\n")
cat("Recall:", recall, "\n")
cat("F1 Score:", f1_score, "\n")
cat("AUC:", auc_lr, "\n")

print(significant_vars)

coefficients <- coef(cv_model_lr$finalModel)
p_values <- summary(cv_model_lr$finalModel)$coefficients[, 4]

# Combine coefficients and p-values into a data frame
coef_df <- data.frame(coefficients, p_values, row.names = rownames(summary(cv_model_lr$finalModel)$coefficients))

# Sort data frame by p-values
coef_df <- coef_df[order(coef_df$p_values), ]

# Create a horizontal bar chart
bar_chart <- ggplot(coef_df, aes(x = coefficients, y = rownames(coef_df))) +
  geom_bar(stat = "identity", fill = ifelse(coef_df$p_values < 0.05, "green", "red")) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "blue") +
  labs(x = "Coefficient Estimate", y = "Variables") +
  scale_fill_manual(values = c("green", "red"), guide = FALSE) +
  theme_minimal()

# Display the bar chart
print(bar_chart)


######2. Decision Trees #####################

# Perform cross-validation with Decision Tree (rpart)
cv_model_dt <-  train(DEATH_EVENT ~ ., data = train_data, method = "rpart", trControl = ctrl)

# Print cross-validation results
print(cv_model_dt)

# Access the overall cross-validation error
cv_error_dt <- cv_model_dt$results

(cv_error_dt)


########## 3. Random forest ########

# Perform cross-validation with Random Forest (rf)
cv_model_rf <-  train(DEATH_EVENT ~ ., data = train_data, method = "rf", trControl = ctrl)

# Print cross-validation results
print(cv_model_rf)

print(summary(cv_model_rf))

# Access the overall cross-validation error
cv_error_rf <- cv_model_rf$results$Accuracy

cv_error_rf

# Access variable importance information
var_importance <- varImp(cv_model_rf)

# Print the variable importance information for each fold
print(var_importance)

plot(var_importance)

# Set up the cross-validation control
ctrl <- trainControl(method = "cv", number = 5)

# Define a grid of hyperparameters to search
hyper_grid <- expand.grid(
  mtry = c(1:12)
  # Number of variables randomly sampled as candidates at each split
  # Complexity parameter for pruning
)

# Perform grid search
grid_search <- train(
  DEATH_EVENT ~ ., 
  data = train_data, 
  method = "rf", 
  trControl = ctrl, 
  tuneGrid = hyper_grid
)

plot(grid_search)

# Print the best hyperparameters and performance
print(grid_search)

# Extract the best hyperparameters
best_mtry <- grid_search$bestTune$mtry


cat("Best hyperparameters:\n")
cat("mtry:", best_mtry, "\n")

# Train the final Random Forest model with the best hyperparameters
best_rf_model <- randomForest(
  DEATH_EVENT ~ ., 
  data = train_data, 
  mtry = best_mtry
)

summary(best_rf_model)

# Print variable importance of the best model
print(importance(best_rf_model))

# Make predictions on test_data using the best model
predictions <- predict(best_rf_model, newdata = test_data)

# Calculate accuracy
accuracy <- mean(predictions == test_data$DEATH_EVENT)
cat("Best accuracy on test data:", accuracy, "\n")

varImpPlot(best_rf_model, main = "Variable Importance")

importance_data <- as.data.frame(importance(best_rf_model))

ggplot(importance_data, aes(x = reorder(rownames(importance_data), MeanDecreaseGini), y = MeanDecreaseGini)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "",
       x = "Predictors",
       y = "Mean Decrease Gini") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Create confusion matrix
confusion_rf <- confusionMatrix(predictions, test_data$DEATH_EVENT)

prediction_obj_rf <- prediction(as.numeric(predictions), test_data$DEATH_EVENT)

# Calculate AUC
auc_object_rf <- performance(prediction_obj_rf, measure = "auc")
auc_rf <- auc_object_rf@y.values[[1]]


######## 4. SVM ######################

# Perform cross-validation with SVM (svm)
cv_model_svm <-  train(DEATH_EVENT ~ ., data = train_data, method = "svmLinear", trControl = ctrl)

# Print cross-validation results
print(cv_model_svm)

# Access the overall cross-validation error
cv_error_svm <- cv_model_svm$results$Accuracy

cv_error_svm
