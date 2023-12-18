library(ggplot2)
library(corrplot)

# Can you create an explainable regression model to predict the probability of a patient dying based on their symptoms, current health status, and medical background?

# Load covid 19 data
covid <- na.omit(read.csv("Data/covid19.csv"))

# Remove "NA" values specified by 999
covid <- covid[!apply(covid == 999, 1, any),]

# Find correlations
correlations <- cor(covid)

# Plot the correlation matrix
corrplot(cor(covid), method = "number")

# Training data
covid_train <- covid[, c("dead", "patient_type", "pneumonia", "age", "diabetes")]

# Create a logistic regression model since being dead or not is a binary classification.
logistic_model <- glm(formula = dead ~ ., data = covid_train, family = binomial)

# Summary of the model.
model_summary <- summary(logistic_model)

# Get p-values
p_values <- model_summary$coefficients[, 4]

# Check if all features have p-value less than 0.05
all_significant <- all(p_values < 0.05)

# Print result
if (all_significant) {
  print("All features have a significance level of at least 5%.")
} else {
  print("Not all features have a significance level of at least 5%.")
}


# Identity the best threshold for the decision rule
thresholds <- seq(0.2, 0.8, 0.01)
accuracies <- sapply(thresholds, function(threshold) {
  predicted_values <- ifelse(predict(logistic_model, type = "response") > threshold, 1, 0)
  sum(predicted_values == covid_train$dead) / nrow(covid_train)
})

# Plot the threshold vs accuracy graph
df <- data.frame(thresholds, accuracies)
ggplot(df, aes(x = thresholds, y = accuracies)) +
  geom_line() +
  labs(title = "Threshold vs Accuracy", x = "Threshold", y = "Accuracy")

# Best threshold with the highest accuracy
best_threshold <- thresholds[which.max(accuracies)]


# Create a logistic regressor with the best threshold.
predicted_values <- ifelse(predict(logistic_model, type = "response") > best_threshold, 1, 0)
accuracy <- sum(predicted_values == covid_train$dead) / nrow(covid_train)

# Export the summary of the model to a CSV file
write.csv(summary(logistic_model)$coefficients, file = "model_summary.csv")

print(model_summary)
print(accuracy)
