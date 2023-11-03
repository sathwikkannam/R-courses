library(ggplot2)

# 1. Correlations
correlations <- cor(iris[, 1:4])
# Are the correlations positive or negative? Does this make sense to you?

# Output:
#              Sepal.Length Sepal.Width Petal.Length Petal.Width
# Sepal.Length    1.0000000  -0.1175698    0.8717538   0.8179411
# Sepal.Width    -0.1175698   1.0000000   -0.4284401  -0.3661259
# Petal.Length    0.8717538  -0.4284401    1.0000000   0.9628654
# Petal.Width     0.8179411  -0.3661259    0.9628654   1.0000000

# The correlations between Sepal.Length and Sepal.Width, Sepal.Length and Petal.Length, and Sepal.Length and Petal.Width are all positive,
# while the correlation between Sepal.Width and Petal.Length and Sepal.Width and Petal.Width are both negative.
# These correlations make sense because Sepal.Length and Petal.Length, as well as Sepal.Length and Petal.Width, are both positively correlated,
# Similarly, Sepal.Width and Petal.Length, as well as Sepal.Width and Petal.Width, are both negatively correlated,

# 2. Plot Sepal.Width against Sepal.Length
plot1 <- ggplot(iris, aes(x = Sepal.Width, y = Sepal.Length)) + geom_point()
# How does the plotted points match the correlations we computed before?

# The plot shows that as Sepal.Width increases, Sepal.Length decreases,
# which is consistent with the negative correlation coefficient of -0.1175698

# 3. Fit a linear model using Sepal.Width as predictor and Sepal.Length as response
model1 <- lm(Sepal.Length ~ Sepal.Width, data = iris)
# Does the estimated coefficient for sepal width match what have seen in tasks 1 and 2?

# Output:
# Coefficients:
# (Intercept)  Sepal.Width
#      6.5262      -0.2234

# The estimated coefficient for Sepal.Width in model1 is negative, which is consistent with the negative correlation coefficient we found in Task 1 and the negative relationship we observed in Task 2.
# However, the magnitude of the coefficient in model1 is larger than the correlation coefficient in Task 1, which suggests that the relationship between Sepal.Width and Sepal.Length may not be perfectly linear.

# 4. Setosa correlations
correlations_setosa <- cor(subset(iris, Species == "setosa")[, 1:4])
# Are these correlations positive or negative? How are they different from the overall correlations? Does this make sense?

# Output:
#              Sepal.Length Sepal.Width Petal.Length Petal.Width
# Sepal.Length    1.0000000   0.7425467    0.2671758   0.2780984
# Sepal.Width     0.7425467   1.0000000    0.1777000   0.2327520
# Petal.Length    0.2671758   0.1777000    1.0000000   0.3316300
# Petal.Width     0.2780984   0.2327520    0.3316300   1.0000000

# The correlations between Sepal.Length and Sepal.Width, Sepal.Length and Petal.Length, and Sepal.Length and Petal.Width are all positive,
# while the correlation between Sepal.Width and Petal.Length and Sepal.Width and Petal.Width are both negative.
# These correlations are different from the overall correlations we found earlier because they are specific to the setosa species,
# while the overall correlations were calculated using all the species in the dataset.

# 5. Plot Sepal.Width against Sepal.Length, color by species
plot2 <- ggplot(iris, aes(x = Sepal.Width, y = Sepal.Length, color = Species)) + geom_point()
# Does this match the correlations we computed for setosa flowers?

# It matches the correlations as Sepal.Width and Sepal.Length are positively correlated with 0.7425467, hence, as Sepal.Width increases
# Sepal.Length also increases. Which is what see in plot2 - a almost lineary correlation.

# 6. Fit second model using species and Sepal.Width as predictors and Sepal.Length as response
model2 <- lm(Sepal.Length ~ Sepal.Width + Species, data = iris)
# Does the estimated coefficient for Sepal.Width make sense? What happened when we added species to our model? How does it change the interpretation?

# Output:
# Coefficients:
#       (Intercept)        Sepal.Width  Speciesversicolor   Speciesvirginica
#            2.2514             0.8036             1.4587             1.9468

# Adding variable Species to the model allows us to estimate the effect of species on the relationship between Sepal.Width and Sepal.Length.
# The estimated coefficient for Sepal.Width in model2 is 0.8036, which means that for every one unit increase in Sepal.Width, we expect an increase of 0.8036 units in Sepal.Length.
# The coefficients for the versicolor and virginica species are both positive, which means that these species have a higher average Sepal.Length than the setosa species,
# Specifically, the estimated coefficient for versicolor is 1.4587, which means that on average, versicolor flowers have a Sepal.Length that is 1.4587 units higher than setosa flowers.
# Similarly, the estimated coefficient for virginica is 1.9468, which means that on average, virginica flowers have a Sepal.Length that is 1.9468 units higher than setosa flowers.
# By adding Specifies to the model, the coefficient represents the effect of Sepal.Width on Sepal.Length for the given species.
# Therefore allowing us to estimate the average difference in Sepal.Length between the species.

# 7. Predict the sepal length of a setosa with a sepal width of 3.6 cm
newdata <- data.frame(Sepal.Width = 3.6, Species = "setosa")
prediction <- predict(model2, newdata)
# Does the prediction appear reasonable?

# Output: 5.144212

# It is resonable as the prediction is within the range of Sepal.Length.
# From plot2 we can see that Sepal.Length at ~3.5 results in a Sepal.Length ~5.

# 8. Load the Pima Indian diabetes .csv into R
diabetes_data <- read.csv("a2_diabetes.csv")
diabetes_data_copy <- na.omit(diabetes_data)

# Correlations between Outcome and other features
correlations_outcome <- cor(diabetes_data_copy)

# Output:
#                          Outcome
# Pregnancies               0.2488099
# Glucose                   0.5437695
# BloodPressure             0.2162127
# SkinThickness             0.2965518
# Insulin                   0.3041094
# BMI                       0.2826541
# DiabetesPedigreeFunction  0.1481706
# Age                       0.3144350

# 9. Find a good logistic regression model
logistic_model <- glm(formula = Outcome ~ Glucose, data = diabetes_data_copy, family = binomial)

# 10. Compute the accuracy
predicted_values <- ifelse(predict(logistic_model, type = "response") > 0.5, 1, 0)
accuracy <- sum(predicted_values == (diabetes_data_copy)$Outcome) / nrow(diabetes_data_copy)

# Show the results of the logistic_model
print(summary(logistic_model))
print(accuracy)