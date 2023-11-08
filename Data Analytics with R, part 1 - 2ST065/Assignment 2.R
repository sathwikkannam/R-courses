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

# The correlations between Sepal.Length and Sepal.Width, Sepal.Width and Petal.Length, and Sepal.Wdith and Petal.Width are all negative.
# The rest are positively correalated.

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

# The estimated coefficient for Sepal.Width in model1 is negative,
# which is consistent with the negative correlation coefficient we found in Task 1 and the negative relationship we observed in Task 2.
# However, the magnitude of the coefficient in model1 is larger than the correlation coefficient in Task 1.

# 4. Setosa correlations
correlations_setosa <- cor(subset(iris, Species == "setosa")[, 1:4])
# Are these correlations positive or negative? How are they different from the overall correlations? Does this make sense?

# Output:
#              Sepal.Length Sepal.Width Petal.Length Petal.Width
# Sepal.Length    1.0000000   0.7425467    0.2671758   0.2780984
# Sepal.Width     0.7425467   1.0000000    0.1777000   0.2327520
# Petal.Length    0.2671758   0.1777000    1.0000000   0.3316300
# Petal.Width     0.2780984   0.2327520    0.3316300   1.0000000

# All features are positive correlated.
# In some cases, the correlations are almost linear (eg. Sepal.Length and Sepal.Width).
# These correlations are different from the overall correlations we found earlier because they are specific to the setosa species,
# while the overall correlations were calculated using all the species in the dataset.
# For example, Sepal.Length and Sepal.Width are strongly correlated in Setosa species. On the other hand, the overall correlation between
# Sepal.Length and Sepal.Width is very weak, and almost insignificant .

# 5. Plot Sepal.Width against Sepal.Length, color by species
plot2 <- ggplot(iris, aes(x = Sepal.Width, y = Sepal.Length, color = Species)) + geom_point()
# Does this match the correlations we computed for setosa flowers?

# It matches the correlations as Sepal.Width and Sepal.Length are positively correlated with 0.7425467, hence, as Sepal.Width increases
# Sepal.Length also increases, which is what we see in plot2 - a almost lineary correlation.

# 6. Fit second model using species and Sepal.Width as predictors and Sepal.Length as response
model2 <- lm(Sepal.Length ~ Sepal.Width + Species, data = iris)
# Does the estimated coefficient for Sepal.Width make sense? What happened when we added species to our model? How does it change the interpretation?

# Output:
# Coefficients:
#       (Intercept)        Sepal.Width  Speciesversicolor   Speciesvirginica
#            2.2514             0.8036             1.4587             1.9468

# The coeffient makes sense as it is similiar to what we found in the correlation above -- positive correlation.
# Adding Species to the model allows it to capture the effect of Species as well as Sepal.Width.
# For example, Sepal.Length is expected to increase by 0.8036 units for every unit increase in Sepal.Width,
# while isolating Sepal.Width and ignoring the effect (constant effect) of Speciesversicolor and Speciesvirginica.

# 7. Predict the sepal length of a setosa with a sepal width of 3.6 cm
newdata <- data.frame(Sepal.Width = 3.6, Species = "setosa")
prediction <- predict(model2, newdata)
# Does the prediction appear reasonable?

# Output: 5.144212

# It is resonable as the prediction is within the range of Sepal.Length.
# From plot2 we can see that Sepal.Length at ~3.5 results in a Sepal.Length ~5.

# 8. Load the Pima Indian diabetes .csv into R
diabetes_data <- read.csv("a2_diabetes.csv")
diabetes_data_copy <- na.omit(diabetes_data[, c("Outcome", "Glucose", "Pregnancies", "SkinThickness")])

# Correlations between Outcome and other features
correlations_outcome <- cor(diabetes_data_copy)[1, -1]

# Output:
#       Glucose   Pregnancies SkinThickness
#     0.5264784     0.2589347     0.2747623

# 9. Find a good logistic regression model
logistic_model <- glm(formula = Outcome ~ ., data = diabetes_data_copy, family = binomial)

# 10. Compute the accuracy
predicted_values <- ifelse(predict(logistic_model, type = "response") > 0.5, 1, 0)
accuracy <- sum(predicted_values == diabetes_data_copy$Outcome) / nrow(diabetes_data_copy)