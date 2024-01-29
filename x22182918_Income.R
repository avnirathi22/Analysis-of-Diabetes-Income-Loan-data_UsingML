#Installing necessary library
install.packages("Amelia")
install.packages("gridExtra")

# Load the necessary library
library(dplyr)
library(Amelia)
library(ggplot2)
library(caTools)
library(tidyverse)
library(readxl)
library(gridExtra)
library(e1071)
library(pROC)
library(caret)

# Load the data
file_path <- "adult_sal.xlsx"
# Read the Excel file
salary_data <- read_excel(file_path)
head(salary_data)
str(salary_data)
column_names <- names(salary_data)
print(column_names)

##### Data Cleaning ######

#Removing 1st column from the dataset
salary_data <- select(salary_data,-...1)
head(salary_data)
#Dropping unwanted columns 
salary_data <- salary_data %>% select(-education)
head(salary_data)

#Combining Type of Employer variable into categories 
table(salary_data$type_employer)
salary_data$type_employer <- ifelse(salary_data$type_employer %in% c("Never-worked", "Without-pay"), "Unemployed", as.character(salary_data$type_employer))
salary_data$type_employer <- ifelse(salary_data$type_employer %in% c("Self-emp-inc", "Self-emp-not-inc"), "Self-emp", as.character(salary_data$type_employer))
salary_data$type_employer <- ifelse(salary_data$type_employer %in% c("Local-gov", "State-gov"), "SL-gov", as.character(salary_data$type_employer))
table(salary_data$type_employer)

#Combining marital variable into categories
table(salary_data$marital)
salary_data$marital <- ifelse(salary_data$marital %in% c("Separated", "Divorced", "Widowed"), "Not-Married", as.character(salary_data$marital))
salary_data$marital <- ifelse(salary_data$marital %in% c("Married-AF-spouse", "Married-civ-spouse", "Married-spouse-absent"), "Married", as.character(salary_data$marital))

# Combining country column into categories #
Asia <- c('China','Hong','India','Iran','Cambodia','Japan', 'Laos' ,
          'Philippines' ,'Vietnam' ,'Taiwan', 'Thailand')

North.America <- c('Canada','United-States','Puerto-Rico' )

Europe <- c('England' ,'France', 'Germany' ,'Greece','Holand-Netherlands','Hungary',
            'Ireland','Italy','Poland','Portugal','Scotland','Yugoslavia')

Latin.and.South.America <- c('Columbia','Cuba','Dominican-Republic','Ecuador',
                             'El-Salvador','Guatemala','Haiti','Honduras',
                             'Mexico','Nicaragua','Outlying-US(Guam-USVI-etc)','Peru',
                             'Jamaica','Trinadad&Tobago')
Other <- c('South')

salary_data$country <- case_when(
  salary_data$country %in% Asia ~ 'Asia',
  salary_data$country %in% North.America ~ 'North.America',
  salary_data$country %in% Europe ~ 'Europe',
  salary_data$country %in% Latin.and.South.America ~ 'Latin.and.South.America',
  TRUE ~ 'Other'
)
table(salary_data$country)
#Rename Country column to Region column
salary_data <- rename(salary_data, region = country)

##### Missing Values #####
salary_data[salary_data == '?'] <- NA


#### Converting variables into Factors ####
salary_data$type_employer <- sapply(salary_data$type_employer,factor)
salary_data$region <- sapply(salary_data$region,factor)
salary_data$marital <- sapply(salary_data$marital,factor)
salary_data$race <- sapply(salary_data$race,factor)
salary_data$occupation <- sapply(salary_data$occupation,factor)
salary_data$relationship <- sapply(salary_data$relationship,factor)
salary_data$sex <- sapply(salary_data$sex,factor)
salary_data$income <- sapply(salary_data$income,factor)

#### Drop Missing data ####
missmap(salary_data)
missmap(salary_data,y.at=c(1),y.labels = c(''),col=c('yellow','black'))

salary_data <- na.omit(salary_data)
missmap(salary_data,y.at=c(1),y.labels = c(''),col=c('yellow','black'))

#### Visualization of Distribution ####

# Creating a pie chart for Type Employer
pie_chart <- ggplot(salary_data, aes(x = 1, fill = type_employer)) +
  geom_bar(stat = "count", width = 1) +
  coord_polar(theta = "y") +
  theme_void()
# Combine plots side by side
grid.arrange(pie_chart)


# Creating a count plot for Education Number column
count_plot <- ggplot(salary_data, aes(y = as.factor(education_num), fill = as.factor(education_num))) +
  geom_bar() +
  theme_minimal() +
  labs(y = "Education Number", x = "Count") +
  guides(fill = FALSE)      # Removing the legend for the color scale
options(repr.plot.width=10, repr.plot.height=6)      #Setting the size of the plot
print(count_plot)

# Creating histogram of Ages on the basis of 'income' variable
ggplot(salary_data, aes(x = age)) +
  geom_histogram(aes(fill = income), color = 'black', binwidth = 1) +
  theme_bw()

# Creating a bar plot based on 'income' variable
bar_plot <- ggplot(salary_data, aes(x = region, fill = income)) +
  geom_bar(color = 'black', position = "dodge") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_fill_brewer(palette = "Set2")
print(bar_plot)

# Creating a histogram for 'hr_per_week' variable
hr_per_week_histogram <- ggplot(salary_data, aes(x = hr_per_week)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black") +
  labs(title = "Histogram of Hours Per Week", x = "Hours Per Week", y = "Count") +
  theme_bw()
print(hr_per_week_histogram)

# Creating a count plot for 'sex' variable
gender_count_plot <- ggplot(salary_data, aes(x = sex, fill = sex)) +
  geom_bar(color = "black") +
  labs(title = "Count Plot for Gender", x = "Sex", y = "Count") +
  scale_fill_manual(values = c("Male" = "blue", "Female" = "orange")) +  
  theme_minimal()
print(gender_count_plot)

# Creating a count plot for 'income' based on sex variable
salary_count_plot <- ggplot(salary_data, aes(x = income, fill = sex)) +
  geom_bar(position = "dodge", color = "black") +
  labs(title = "Data Income Values Count", x = "Income", y = "Count") +
  scale_fill_manual(values = c("Male" = "blue", "Female" = "orange")) +  
  theme_minimal() +
  theme(legend.title = element_blank()) +  
  guides(fill = guide_legend(title = "Sex"))  
print(salary_count_plot)

# Creating count plots for 'marital' variable
marital_count_plot <- ggplot(salary_data, aes(x = marital, fill = marital)) +
  geom_bar(color = "black") +
  labs(title = "Count Plot for Marital Status", x = "Marital Status", y = "Count") +
  theme_minimal()  +
  theme(legend.position = "none")# Format y-axis labels
print(marital_count_plot)

# Creating count plots for 'occupation' variable
occupation_count_plot <- ggplot(salary_data, aes(x = occupation, fill = occupation)) +
  geom_bar(color = "black") +
  labs(title = "Count Plot for Occupation", x = "Occupation", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "none")
print(occupation_count_plot)

# Creating count plots for 'relationship' variable
relationship_count_plot <- ggplot(salary_data, aes(x = relationship, fill = relationship)) +
  geom_bar(color = "black") +
  labs(title = "Count Plot for Relationship", x = "Relationship", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "none")
print(relationship_count_plot)


####### Data Preprocessing #######

# Converting 'Male' to 0 and 'Female' to 1
salary_data$sex <- as.numeric(salary_data$sex) - 1
salary_data$race <- as.numeric(salary_data$race) - 1

# Converting 'income' contains 0 for "<=50K" and 1 for ">50K"
salary_data$income <- ifelse(salary_data$income == "<=50K", 0, 1)
head(salary_data$income)
# Applying one-hot encoding on these columns
columns_to_encode <- c("type_employer", "marital", "occupation", "relationship", "region")
# Creating dummy variables
dummy_matrix <- dummyVars("~.", data = salary_data[, columns_to_encode])
salary_data_encoded <- as.data.frame(predict(dummy_matrix, newdata = salary_data))
# Concatenate the encoded variables to the original dataset
salary_data_encoded <- cbind(salary_data, salary_data_encoded)
# Removing the original categorical variables from the dataset
salary_data_encoded <- salary_data_encoded[, !(names(salary_data) %in% c("type_employer", "marital", "occupation", "relationship", "region"))]
head(salary_data_encoded)


# Applying Standardization to the numerical columns
numerical_columns <- c("age", "fnlwgt", "education_num", "capital_gain", "capital_loss", "hr_per_week")
salary_data_encoded[, numerical_columns] <- scale(salary_data_encoded[, numerical_columns])
head(salary_data_encoded)


###### Logistic Regression Model #######

# Splitting Data for modelling
#setting random seed as my student number 
set.seed(22182918)
sample <- sample.split(salary_data_encoded$income, SplitRatio = 0.70) 
train <- subset(salary_data_encoded, sample == TRUE)
test <- subset(salary_data_encoded, sample == FALSE)
# Train the logistic regression model
model <- glm(income ~ ., family = binomial(logit), data = train)
summary(model)

# Make predictions on the test data
test$predicted.income <- predict(model, newdata = test, type = "response")
# Converting to binary predictions
test$predicted.income_binary <- ifelse(test$predicted.income > 0.5, 1, 0)
test$predicted.income_binary <- factor(test$predicted.income_binary, levels = c("0", "1"))
test$income <- factor(test$income, levels = c("0", "1"))

# Evaluate the logistic regression model
conf_matrix_lr <- confusionMatrix(test$predicted.income_binary, test$income)
print("Confusion Matrix (Logistic Regression):")
print(conf_matrix_lr)

conf_matrix <- confusionMatrix(test$predicted.income_binary, test$income)
precision <- conf_matrix$byClass["Pos Pred Value"]
recall <- conf_matrix$byClass["Sensitivity"]
accuracy <- conf_matrix$overall["Accuracy"]
f1 <- conf_matrix$byClass["F1"]

print(paste("Precision:", precision))
print(paste("Recall:", recall))
print(paste("Accuracy:", accuracy))
print(paste("F1 Score:", f1))

library(pROC)
# Creating ROC curve for Logistic Regression 
roc_curve <- roc(as.numeric(test$income) - 1, test$predicted.income)
cat("AUC:", round(roc_curve$auc, 4), "\n")
# Plotting ROC curve for Logistics Regression 
plot(roc_curve, main = "ROC Curve - Logistic Regression", col = "blue", lwd = 2)


#------------------------------------------------------------------------------
  ############ MODEL 2 - Naive Bayes ###########


#setting random seed as my student number 
#set.seed(22182918)
# Splitting Data for modelling
#sample <- sample.split(salary_data_encoded$income, SplitRatio = 0.70)
#train <- subset(salary_data_encoded, sample == TRUE)
#test <- subset(salary_data_encoded, sample == FALSE)

# Applying the Naive Bayes Model
naive_bayes_model <- naiveBayes(income ~ ., data = train)
# Make predictions on the test set
naive_bayes_predictions <- predict(naive_bayes_model, newdata = test, type = "raw")
# Converting predicted values to factor according to levels
predicted_class <- as.factor(ifelse(naive_bayes_predictions[, "1"] > 0.5, "1", "0"))
actual_class <- as.factor(test$income)
predicted_class <- factor(predicted_class, levels = levels(actual_class))

# Evaluate the Naive Bayes model using confusionMatrix
conf_matrix_naive_bayes <- confusionMatrix(predicted_class, actual_class)
print("Confusion Matrix (Naive Bayes):")
print(conf_matrix_naive_bayes)

precision_naive_bayes <- conf_matrix_naive_bayes$byClass["Pos Pred Value"]
recall_naive_bayes <- conf_matrix_naive_bayes$byClass["Sensitivity"]
accuracy_naive_bayes <- conf_matrix_naive_bayes$overall["Accuracy"]
f1_naive_bayes <- conf_matrix_naive_bayes$byClass["F1"]

cat("Precision (Naive Bayes):", round(precision_naive_bayes, 4), "\n")
cat("Recall (Naive Bayes):", round(recall_naive_bayes, 4), "\n")
cat("Accuracy (Naive Bayes):", round(accuracy_naive_bayes, 4), "\n")
cat("F1 Score (Naive Bayes):", round(f1_naive_bayes, 4), "\n")

library(pROC)
# Converting predicted values to numeric
predicted_values_numeric <- as.numeric(naive_bayes_predictions[, "1"])
# Creating ROC curve for Naive Bayes
roc_curve_naive_bayes <- roc(test$income, predicted_values_numeric)
cat("AUC:", round(roc_curve_naive_bayes$auc, 4), "\n")
# Plotting ROC curve for Naive Bayes
plot(roc_curve_naive_bayes, main = "ROC Curve - Naive Bayes", col = "blue", lwd = 2)





