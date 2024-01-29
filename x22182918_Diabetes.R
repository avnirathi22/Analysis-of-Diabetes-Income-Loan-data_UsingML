#Installing necessary library
install.packages("tidyverse")
install.packages("randomForest")
install.packages("dplyr")
install.packages("Metrics")
install.packages("pROC")
install.packages("plotly")
# Load the necessary library
library(tidyverse)
library(dplyr)
library(randomForest)
library(readxl)
library(psych)
library(pROC)  # Load pROC before caret
library(caret)
library(Metrics)
library(ggplot2)
library(corrplot)
library(RColorBrewer)
library(class)
library(plotly)

#Load the dataset
file_path <- "diabetes_prediction_dataset 4.xlsx"
# Read the Excel file
diabetes_data <- read_excel(file_path)
head(diabetes_data)
str(diabetes_data)

#### Data cleaning ####
#checking for missing values
any(is.na(diabetes_data))
any(is.na(diabetes_data$diabetes))

# Checking for  duplicates values
duplicates <- diabetes_data[duplicated(diabetes_data), ]
if (nrow(duplicates) > 0) {
  print("Duplicate Rows:")
  print(duplicates)
} else {
  print("No duplicate rows found.")
}
# Removing duplicates from the dataset
diabetes_data <- diabetes_data %>% distinct()
# Print the dimensions after removing duplicates
data_dimensions <- dim(diabetes_data)
print(paste("Number of rows after removing duplicates:", data_dimensions[1]))
print(paste("Number of columns:", data_dimensions[2]))

#looking for Unique Values for each column
for (column in names(diabetes_data)) {
  unique_values <- length(unique(diabetes_data[[column]]))
  print(paste(column, unique_values, "unique values"))
}

# printing unique values for gender
unique_values <- unique(diabetes_data$gender)
print(unique_values)
# Removing rows where 'gender' is equal to 'Other'
diabetes_data <- diabetes_data[diabetes_data$gender != 'Other', ]

#checking unique values counts from smoking_history
smoking_history_counts <- table(diabetes_data$smoking_history)
print(smoking_history_counts)
# Creating a new column 'smoking_history' 
diabetes_data <- diabetes_data %>%
  mutate(smoking_history = case_when(
    smoking_history %in% 'No Info' ~ 0,
    smoking_history == c('never', 'ever') ~ 1,
    smoking_history %in% c('former', 'not current') ~ 2,
    TRUE ~ 3  # Default case
  ))
str(diabetes_data)

# Converting 'Male' to 1 and 'Female' to 0 and changed the datatype to factor
diabetes_data$gender <- factor(diabetes_data$gender, levels = c("Male", "Female"))
# Convert the factor to numeric
diabetes_data$gender <- as.numeric(diabetes_data$gender) - 1
head(diabetes_data)

describe(diabetes_data)

#### Converting variables into Factors ####
diabetes_data$diabetes <- as.factor(diabetes_data$diabetes)
diabetes_data$smoking_history <- as.factor(diabetes_data$smoking_history)
diabetes_data$heart_disease <- factor(diabetes_data$heart_disease)


#### Data Visualization ####

# Creating a histogram for 'age' variable with diabetes
ggplot(diabetes_data, aes(x = age, fill = diabetes)) +
  geom_histogram(color = 'black', binwidth = 2) +
  theme_bw()

# Creating a bar plot based on 'heart disease' variable with diabetes
bar_plot <- ggplot(diabetes_data, aes(x = heart_disease, fill = diabetes)) +
  geom_bar(color = 'black', position = "dodge") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
options(repr.plot.width = 10, repr.plot.height = 6)
print(bar_plot)

# Creating a bar plot based on 'smoking history' variable 
ggplot(diabetes_data, aes(x = smoking_history, fill = smoking_history, group = smoking_history)) +
  geom_bar(color = "black") +
  labs(title = "Smoking History Distribution", x = "Smoking History", y = "Count") +
  scale_fill_brewer(palette = "Set3")

# Creating a count plot based on 'hypertension' variable
count_plot <- ggplot(diabetes_data, aes(x = as.factor(hypertension), fill = as.factor(hypertension))) +
  geom_bar(color = 'black') +
  theme_minimal() +
  labs(title = "Count Plot for Hypertension", x = "Hypertension", y = "Count") +
  scale_fill_brewer(palette = "Set2") +
  guides(fill = FALSE)  # Remove the legend
options(repr.plot.width = 8, repr.plot.height = 6)
print(count_plot)

# Creating a count plot based on 'HbA1c_level' variable
count_plot <- ggplot(diabetes_data, aes(x = as.factor(HbA1c_level), fill = as.factor(HbA1c_level))) +
  geom_bar(color = 'black') +
  theme_minimal() +
  labs(title = "Count Plot for HbA1c_level", x = "HbA1c_level", y = "Count") +
  scale_fill_discrete() +
  guides(fill = FALSE)  # Remove the legend
options(repr.plot.width = 8, repr.plot.height = 6)
print(count_plot)

# Creating a count plot based on 'Blood Glucose Level' variable
count_plot <- ggplot(diabetes_data, aes(x = as.factor(blood_glucose_level), fill = as.factor(blood_glucose_level))) +
  geom_bar(color = 'black') +
  theme_minimal() +
  labs(title = "Count Plot for blood_glucose_level", x = "blood_glucose_level", y = "Count") +
  scale_fill_discrete() +
  guides(fill = FALSE)  # Remove the legend
options(repr.plot.width = 8, repr.plot.height = 6)
print(count_plot)

#Scatterplot of Age and BMI for diabetes
ggplot(diabetes_data, aes(x = age, y = bmi, color = factor(diabetes))) +
  geom_point() +
  labs(title = "Age vs BMI", x = "Age", y = "BMI", color = "Diabetes") +
  scale_color_manual(values = c("0" = "blue", "1" = "red"))  


# Select only numeric columns from the data frame
numeric_data <- diabetes_data[, sapply(diabetes_data, is.numeric)]
str(diabetes_data)
# Calculating correlation matrix
cor_matrix <- cor(numeric_data)
# Create heatmap
corrplot(cor_matrix, method = "color",
         tl.col = "black", tl.srt = 45, tl.cex = 0.7, addCoef.col = "black", number.cex = 0.7)

#creating a pie chart of Gender with their percentage
gender_counts <- table(diabetes_data$gender)
count_dict <- as.numeric(gender_counts)
names(count_dict) <- levels(gender_counts)
print(gender_counts)
print(count_dict)
# Calculate percentages
percentages <- round(count_dict / sum(count_dict) * 100, 2)
pie(count_dict, labels = paste(names(count_dict), ": ", percentages, "%", sep = ""), col = c("coral2", "skyblue"), main = "Gender Distribution")
# Adding a legend
legend("topright", legend = c("Female", "Male"), fill = c("coral2", "skyblue"))
title("Gender Distribution")

## 3-D pie chart for 'Diabetes' variable with percentage 
# Calculating class counts
class_counts <- table(diabetes_data$diabetes)
# Calculating percentages
percentages <- round(prop.table(class_counts) * 100, 1)
# Creating labels with category names and percentages
labels <- paste(c('not diabetes', 'diabetes'), "\n", percentages, "%")
pie_chart <- plot_ly(labels = ~labels, values = ~class_counts, type = 'pie', pull = c(0.2, 0), textinfo = 'label+percent')
pie_chart <- layout(pie_chart, title = 'Diabetes Distribution')
pie_chart



##################### Applying Random Forest Model ###################

# Taking diabetes as a Target variable 
target_variable <- "diabetes"

features <- diabetes_data %>% select(-{{ target_variable }})
target <- diabetes_data[[target_variable]]

#setting random seed as my student number 
set.seed(22182918)

# Splitting the data into training (80%) and testing (20%) sets
sample_indices <- createDataPartition(target, p = 0.8, list = FALSE)
train_data <- diabetes_data[sample_indices, ]
test_data <- diabetes_data[-sample_indices, ]
train_target <- target[sample_indices]
test_target <- target[-sample_indices]

train_data[[target_variable]] <- as.factor(train_data[[target_variable]])
test_data[[target_variable]] <- as.factor(test_data[[target_variable]])
# Train Random Forest model
rf_model <- randomForest(formula = formula(paste(target_variable, "~ .")), data = train_data, ntree = 100)
# Make predictions on the test set
rf_predictions <- predict(rf_model, newdata = test_data)

# Evaluate the model performance
conf_matrix_rf <- confusionMatrix(rf_predictions, test_data[[target_variable]])
print("Confusion Matrix (Random Forest):")
print(conf_matrix_rf)

accuracy_rf <- conf_matrix_rf$overall["Accuracy"]
precision_rf <- conf_matrix_rf$byClass["Pos Pred Value"]
recall_rf <- conf_matrix_rf$byClass["Sensitivity"]
f1_rf <- conf_matrix_rf$byClass["F1"]

cat("Accuracy (Random Forest):", round(accuracy_rf, 4), "\n")
cat("Precision (Random Forest):", round(precision_rf, 4), "\n")
cat("Recall (Random Forest):", round(recall_rf, 4), "\n")
cat("F1 Score (Random Forest):", round(f1_rf, 4), "\n")

# Calculate ROC AUC
rf_probs <- as.numeric(predict(rf_model, newdata = test_data, type = "response"))
library(pROC)
# Creating ROC curve for Random Forest
roc_curve_rf <- roc(test_data[[target_variable]], rf_probs)
cat("AUC (Random Forest):", round(roc_curve_rf$auc, 4), "\n")
# Plotting the ROC curve for Random Forest
plot(roc_curve_rf, main = "ROC Curve - Random Forest", col = "blue", lwd = 2)




#_________________________________________________________________________

################# Applying second model- KNN Model #########################

features <- diabetes_data %>% select(-diabetes)
target <- diabetes_data$diabetes

#setting random seed as my student number 
set.seed(22182918)

# Splitting the data into training (80%) and testing (20%) sets
sample_indices <- createDataPartition(target, p = 0.8, list = FALSE)
train_data <- features[sample_indices, ]
test_data <- features[-sample_indices, ]
train_target <- target[sample_indices]
test_target <- target[-sample_indices]

# Train KNN model
knn_model <- knn(train = train_data, test = test_data, cl = train_target, k = 3)
knn_pred <- as.factor(knn_model)
levels_to_use <- union(levels(knn_pred), levels(test_target))
knn_pred <- factor(knn_pred, levels = levels_to_use)
test_target <- factor(test_target, levels = levels_to_use)

# Evaluate the model performance
# Confusion matrix
conf_matrix <- confusionMatrix(knn_pred, test_target)
print("Confusion Matrix (K Nearest Neighbor):")
print(conf_matrix)

accKNN <- confusionMatrix(knn_pred, test_target)$overall["Accuracy"]
print("Confusion Matrix (K Nearest Neighbor):")
print(paste("Accuracy:", accKNN))

precision <- confusionMatrix(knn_pred, test_target)$byClass["Pos Pred Value"]
recall <- confusionMatrix(knn_pred, test_target)$byClass["Sensitivity"]
f1 <- confusionMatrix(knn_pred, test_target)$byClass["F1"]

print(paste("Precision:", precision))
print(paste("Recall:", recall))
print(paste("F1 Score:", f1))

# Calculate ROC AUC
knn_probs <- as.numeric(knn_model)
library(pROC)
# Creating ROC curve for K Nearest Neighbor
roc_curve_knn <- roc(test_target, knn_probs)
cat("AUC (K Nearest Neighbor):", round(roc_curve_knn$auc, 4), "\n")
# Plot ROC curve for K Nearest Neighbor
plot(roc_curve_knn, main = "ROC Curve - K Nearest Neighbor", col = "blue", lwd = 2)



