library(ggplot2)

# 1. Correlations
correlations <- cor(iris[, 1:4])

# 2. Plot Sepal.Width against Sepal.Length
plot1 <- ggplot(iris, aes(x = Sepal.Width, y = Sepal.Length)) + geom_point()

# 3. Fit a linear model using Sepal.Width as predictor and Sepal.Length as response
model1 <- lm(Sepal.Length ~ Sepal.Width, data = iris)

# 4. Setosa correlations
correlations_setosa <- cor(subset(iris, Species == "setosa")[, 1:4])

# 5. Plot Sepal.Width against Sepal.Length, color by species
plot2 <- ggplot(iris, aes(x = Sepal.Width, y = Sepal.Length, color = Species)) + geom_point()

# 6. Fit second model using species and Sepal.Width as predictors and Sepal.Length as response
model2 <- lm(Sepal.Length ~ Sepal.Width + Species, data = iris)

# 7. Predict the sepal length of a setosa with a sepal width of 3.6 cm
newdata <- data.frame(Sepal.Width = 3.6, Species = "setosa")
prediction <- predict(model2, newdata)

# 8. Load the Pima Indian diabetes .csv into R
diabetes_data <- read.csv("a2_diabetes.csv")

# 9. Find a good logistic regression model
logistic_model <- glm(Outcome ~ ., data = diabetes_data, family = binomial)

# 10. Compute the accuracy
predicted_values <- ifelse(predict(logistic_model, type = "response") > 0.5, 1, 0)
actual_values <- diabetes_data$Outcome
accuracy <- sum(predicted_values == actual_values) / nrow(diabetes_data)
