#Installing necessary library
#install.packages("pROC")
#install.packages("caret")
#install.packages("plotly")

# Load the necessary library
library(caTools)
library(e1071)
library(caret)
library(ggplot2)
library(corrplot)
library(plotly)
# Load the data
loan_data <- read.csv('loan_data.csv')
str(loan_data)
head(loan_data)

#### Data cleaning ####

#checking for missing values
any(is.na(loan_data))

# Checking for  duplicates values
duplicates <- loan_data[duplicated(loan_data), ]
if (nrow(duplicates) > 0) {
  print("Duplicate Rows:")
  print(duplicates)
} else {
  print("No duplicate rows found.")
}

#converting these variables into factor 
loan_data$credit.policy <- factor(loan_data$credit.policy)
loan_data$inq.last.6mths <- factor(loan_data$inq.last.6mths)
loan_data$delinq.2yrs <- factor(loan_data$delinq.2yrs)
loan_data$pub.rec <- factor(loan_data$pub.rec)
loan_data$not.fully.paid <- factor(loan_data$not.fully.paid)
str(loan_data)


##### Visulisation and EDA #######

# Creating a histogram for the 'int.rate' column 
hist(loan_data$int.rate, main = "Interest Rate Distribution", xlab = "Interest Rate", col = heat.colors(20))

# Creating a count plot for 'purpose' varibale 
pl <- ggplot(loan_data, aes(x = purpose, fill = not.fully.paid, group = not.fully.paid)) +
  geom_bar(position = "dodge") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
print(pl)

# Creating a histogram for 'Fico' variable based on 'not.fully.paid' variable
pl <- ggplot(loan_data, aes(x = fico)) +
  geom_histogram(aes(fill = not.fully.paid), color = 'black', bins = 40, alpha = 0.5) +
  scale_fill_manual(values = c('green', 'red')) +
  theme_bw()
print(pl)

#converting values into categories using custom binning method for ing.last.6mths column
custom_binning <- function(value) {
  if (value == 0) {
    return("bin1")
  } else if (value %in% c(1, 2)) {
    return("bin2")
  } else if (value %in% c(3, 4, 5)) {
    return("bin3")
  } else {
    return("bin4")
  }
}

loan_data$inq_bins <- sapply(loan_data$inq.last.6mths, custom_binning)
# To display the distribution of new labels with percentages
bin_counts <- table(loan_data$inq_bins)
percentages <- (bin_counts / sum(bin_counts)) * 100
cat("Custom Binning Distribution\n")
cat("Bin\tCounts\tPercentage\n")
for (i in seq_along(bin_counts)) {
  cat(paste(names(bin_counts)[i], "\t", bin_counts[i], "\t", sprintf("%.2f%%", percentages[i]), "\n"))
}
# creating a count plot for 'inq.last.6mths' variable 
ggplot(loan_data, aes(x = inq_bins, fill = inq_bins)) +
  geom_bar(color = "black") +
  labs(title = "Inq last 6mths Binning Distribution",
       x = "Bins",
       y = "Count") +
  scale_fill_brewer(palette = "Set3")

#converting values into categories using custom binning method for delinq.2yrs column 
# Converting 'delinq.2yrs' to numeric
loan_data$delinq.2yrs <- as.numeric(as.character(loan_data$delinq.2yrs))

loan_data$delinq_bins <- cut(loan_data$delinq.2yrs, breaks = c(-Inf, 0, 1, Inf),
                             labels = c("bin1", "bin2", "bin3"), include.lowest = TRUE)
unique_values_counts <- table(loan_data$delinq_bins)
percentages <- (unique_values_counts / sum(unique_values_counts)) * 100
cat("Feature: delinq.2yrs\n")
cat("Unique Values\tCounts\tPercentage\n")
for (i in seq_along(unique_values_counts)) {
  cat(paste(names(unique_values_counts)[i], "\t\t", unique_values_counts[i], "\t\t", sprintf("%.2f%%", percentages[i]), "\n"))
}
cat("\n")
# Creating a data frame for the pie chart 
pie_data <- data.frame(labels = names(unique_values_counts),
                       counts = as.numeric(unique_values_counts))
# To plot a pie chart for 'delinq.2yrs' variable 
plot_ly(data = pie_data, labels = ~labels, values = ~counts, type = "pie",
        textinfo = "percent+label", hole = 0.4) %>%
  layout(title = "Delinq bins Distribution")


#converting values into categories using custom binning method for pub.rec column  
loan_data$pub.rec_bins <- ifelse(loan_data$pub.rec == 0, "bin1", "bin2")
unique_values_counts <- table(loan_data$pub.rec_bins)
percentages <- (unique_values_counts / sum(unique_values_counts)) * 100
cat("Feature: pub.rec\n")
cat("Unique Values\tCounts\tPercentage\n")
for (i in seq_along(unique_values_counts)) {
  cat(paste(names(unique_values_counts)[i], "\t\t", unique_values_counts[i], "\t\t", sprintf("%.2f%%", percentages[i]), "\n"))
}
cat("\n")
# Creating a bar plot of 'pub.rec' variable 
ggplot(loan_data, aes(x = pub.rec_bins, fill = pub.rec_bins)) +
  geom_bar(color = "black") +
  labs(title = "Pub.rec Binning Distribution",
       x = "Bins",
       y = "Count") +
  scale_fill_manual(values = c("skyblue", "salmon"))


# Converting 'revol.util' to numeric
loan_data$revol.util <- as.numeric(loan_data$revol.util)
# Selecting data where 'revol.util' is greater than 100
records_over_100 <- loan_data[loan_data$revol.util > 100, ]
print("Records where revol.util is over 100%:")
print(records_over_100)
loan_data$revol.util <- as.numeric(loan_data$revol.util)
# To identify and replace values over 100 with 100
loan_data$revol.util[loan_data$revol.util > 100] <- 100
print("Updated dataset:")
print(loan_data)
str(loan_data)


# Extract numerical features from the dataset 
numerical_features <- loan_data[sapply(loan_data, is.numeric)]
# To plot histograms for all numerical variables
par(mfrow = c(3, 3)) 
for (col in colnames(numerical_features)) {
  hist(numerical_features[[col]], main = col, xlab = col, col = "skyblue", border = "black")
}
par(mfrow = c(1, 1))  
# Applying Logarithm transformation on 'revol.bal' variable 
loan_data$log_revol_bal <- log(loan_data$revol.bal + 1)  # Adding 1 to avoid log(0)
# Plot histogram for'revol.bal' variable 
hist(loan_data$log_revol_bal, main = "Log-transformed revol.bal", xlab = "Log(revol.bal)", col = "skyblue", border = "black")
str(loan_data)

# creating a scatter plot for 'int.rate' vs 'fico'
plot(loan_data$fico, loan_data$int.rate, col = ifelse(loan_data$not.fully.paid == 1, "red", "green"),
     main = "FICO vs. Interest Rate", xlab = "FICO", ylab = "Interest Rate")

# Calculating the correlation matrix for all numeric column
numeric_data <- loan_data[sapply(loan_data, is.numeric)]
cor_matrix <- cor(numeric_data)
corrplot(cor_matrix, method = "color",
         tl.col = "black", tl.srt = 45, tl.cex = 0.7, addCoef.col = "black", number.cex = 0.7)



##### Data Preprocessing #####

# Applying one-hot encoding on the 'purpose' column which is a categorical column
one_hot_encoded <- as.data.frame(model.matrix(~ purpose - 1, data = loan_data))
# Add the one-hot encoded column to the original dataset
loan_data <- cbind(loan_data, one_hot_encoded)
# Removing the original 'purpose' column
loan_data$purpose <- NULL
head(loan_data)
str(loan_data)

#Dropping unnecessary columns from the dataset
features_to_drop <- c('revol.bal', 'inq.last.6mths', 'delinq.2yrs', 'pub.rec')
loan_data <- loan_data[, !(names(loan_data) %in% features_to_drop)]
head(loan_data)
str(loan_data)

#applying label encoding on these 3 columns from the dataset
features_to_label_encode <- c('inq_bins', 'delinq_bins', 'pub.rec_bins')
for (feature in features_to_label_encode) {
  loan_data[[feature]] <- as.numeric(factor(loan_data[[feature]]))
}
head(loan_data)

# Applying Normalization on all numeric columns from the dataset
numerical_cols <- c("int.rate", "installment", "log.annual.inc", "dti", "fico", 
                    "days.with.cr.line", "revol.util", "inq_bins", "delinq_bins",
                    "pub.rec_bins", "log_revol_bal")
numerical_data <- loan_data[, numerical_cols]
normalized_data <- scale(numerical_data)
# Replacing the original numerical columns with the normalized ones
loan_data[, numerical_cols] <- normalized_data
head(loan_data)


###### Applying SVM model ######
#setting random seed as my student number 
set.seed(22182918)
# Splitting the data into training (70%) and testing (30%) sets
spl <- sample.split(loan_data$not.fully.paid, 0.7)

train <- subset(loan_data, spl == TRUE)
test <- subset(loan_data, spl == FALSE)

model <- svm(not.fully.paid ~ ., data = train)
summary(model)

# Removing 'not.fully.paid' column from the test dataset
test_data <- test[, -which(names(test) == "not.fully.paid")]

predicted.values <- predict(model, test_data)
table(predicted.values, test$not.fully.paid)

#using 'tune' function for doing hyperparameter tuning for the SVM model
tune.results <- tune(
  svm,
  train.x = not.fully.paid ~ .,
  data = train,
  kernel = 'radial',
  ranges = list(cost = c(1, 10), gamma = c(0.1, 1))
)

model <- svm(not.fully.paid ~ ., data = train, cost = 10, gamma = 0.1)
predicted.values <- predict(model, test_data)
table(predicted.values, test$not.fully.paid)

svm_predictions <- predict(model, test[, -which(names(test) == "not.fully.paid")])
# Convert predicted values to a factor
svm_predictions <- factor(svm_predictions, levels = levels(test$not.fully.paid))

# Calculating Confusion matrix
conf_matrix_svm <- confusionMatrix(svm_predictions, test$not.fully.paid)
print("Confusion Matrix (SVM):")
print(conf_matrix_svm)

# Calculating performance metrics
accuracy_svm <- conf_matrix_svm$overall["Accuracy"]
precision_svm <- conf_matrix_svm$byClass["Pos Pred Value"]
recall_svm <- conf_matrix_svm$byClass["Sensitivity"]
f1_svm <- conf_matrix_svm$byClass["F1"]

cat("Accuracy (SVM):", round(accuracy_svm, 4), "\n")
cat("Precision (SVM):", round(precision_svm, 4), "\n")
cat("Recall (SVM):", round(recall_svm, 4), "\n")
cat("F1 Score (SVM):", round(f1_svm, 4), "\n")

# Calculating ROC AUC
test_data <- test[, -which(names(test) == "not.fully.paid")]

roc_curve_svm <- pROC::roc(test$not.fully.paid, as.numeric(predict(model, newdata = test_data)))
roc_auc_svm <- pROC::auc(roc_curve_svm)
print(paste("ROC AUC Score (SVM):", round(roc_auc_svm, 4)))

# To Plot the ROC curve 
plot(roc_curve_svm, main = "ROC Curve for SVM", col = "red", lwd = 2)










