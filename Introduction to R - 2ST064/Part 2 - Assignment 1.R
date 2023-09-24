#####
# Introduction to R - Assignment 1
#####

# Remember you can test your answers at https://statistics-umu.shinyapps.io/introduction-to-r/
# (testing is not the same as a submission, which should be in Canvas)

### Example of if/else combination with a for loop
# This is just an example to give you ideas you can use during the rest of this assignment
# Feel free to copy parts of the code and adjust them as needed
# This code takes a vector of random number and separates them into two vectors: odd_numbers and even_numbers

vector_numbers <- sample(1:1000, size = 50)
odd_numbers <- NULL
even_numbers <- NULL

for (index in seq_along(vector_numbers)) {
  # The modulus operator %% gives the remainder of a division. So 3 %% 2 = 1 but 4 %% 2 = 0
  if (vector_numbers[index] %% 2 == 0) {
    even_numbers <- c(even_numbers, vector_numbers[index])
  }else {
    odd_numbers <- c(odd_numbers, vector_numbers[index])
  }
}


### Start of the Assignment

# R has some built in datasets, which are frequently used for testing and studying purposes
# Among these, the dataset USArrests contains statistics about violent crime rates by us state
# We can add it to our environment by doing:

USArrests <- get(data("USArrests"))

# With the dataset added to our environment, we can have a look at it:

print(USArrests)

# And we can, for example, check what is the average muder rate in american states:

average_murder <- mean(USArrests$Murder)
print(average_murder)

# The code below prints the names of all the states in the dataset one by one, then adds it to a vector
total_rows <- nrow(USArrests)
state_names <- row.names(USArrests)
all_states = c()

for (row in 1:total_rows) {
  print(state_names[row])
  all_states = c(all_states, state_names[row])
}

# We can also create new datasets that are just a subset of the initial dataset
# For example, let's subset with only US states that border Mexico:

mexican_border <- USArrests[row.names(USArrests) %in% c('California', 'Arizona', 'New Mexico', 'Texas'),]
print(mexican_border)

# We can also select all states with a specific name. For example:
Utah <- USArrests[row.names(USArrests) == 'Utah',] #This is just one line, but it is an example

# Or with a specific UrbanPop. For example:
UrbanPop80 <- USArrests[USArrests$UrbanPop == 80,]


# Q1) Write your own loops (and/or modify one of the example loops above) so that we create two vectors:
# The first vector should be named below_average and should contain the names of 
# all states with a murder rate below the average (average_murder)
# The second vector should be names above_average and should contain the names of 
# all states with a murder rate above the average (average_murder)

below_average <- row.names(subset(USArrests, USArrests$Murder < average_murder))
above_average <- row.names(subset(USArrests, USArrests$Murder > average_murder))


# For Q2, we will be using the penguins dataset, from the package 'palmerpenguins'. 
# We can install the package and load it by:

#You need to run the install only once.  
# You should uncomment this install.packages line, run it once then comment it out again.

# install.packages('palmerpenguins') 
library(palmerpenguins)
penguins <- get(data("penguins"))

# 11 rows have incomplete information, we are interested only in the complete cases of the penguins
# Therefore, we do:

penguins <- penguins[complete.cases(penguins),]

# Q2) Using the above complete penguins dataset, separate it into two dataframes, one for male penguins and one for female
# Use the name 'male_penguins' and 'female_penguins' for the respective dataframes

male_penguins <- subset(penguins, penguins$sex == 'male')
female_penguins <- subset(penguins, penguins$sex == 'female')

# Q3) Do male or female penguins in the dataset have larger flippers for their body? 
# Calculate the average flipper size divided by body mass (flipper_length_mm / body_mass_g) 
# for both male and female penguins. Remember you can use the mean() function as in the example above
# Hint: You want flipper_length (in mm) / body_mass (g) for each penguin then take a mean


flipper_proportion_male <- mean(male_penguins$flipper_length_mm / male_penguins$body_mass_g)
flipper_proportion_female <- mean(female_penguins$flipper_length_mm / female_penguins$body_mass_g)

# Q4) On average, penguins have a bill (or beak) length that is more than twice of its depth.
# Due to genetic and species variation, this is not true for all penguins. 
# Some penguins just have "big mouths". Or maybe very short beaks.
# Create a dataframe that includes only penguins that have a bill length that is less twice its depth. 
# Do not include the cases that the bill length is equal to twice the depth.
# Hint: You want the penguins with bill_length (in mm) < 2 * bill_depth (in mm)


big_mouth <- subset.data.frame(penguins, penguins$bill_length_mm < 2 * penguins$bill_depth_mm)

# Q5) You might have noticed that all penguins with a "big mouth" (or a short beak) are all from the same species, "Adelie"
# What is the proportion of adelie penguins with a big mouth? Assign that number to the variable adelie_big_mouth_proportion
# Hint: You need the total of adelie penguins with a big mouth / total amount of adelie penguins. Do not multiply by 100 (we want the proportion, not the percentage).

adelie_big_mouth_proportion <- nrow(big_mouth) / nrow(subset(penguins, penguins$species == 'Adelie'))

# Q6) When measuring the penguins, there was a problem with the calibration of the scale in the Biscoe island.
# The scale used in Biscoe inflated all weights of penguins by 250g. 
# Create a corrected dataframe "corrected_penguins" with the adjusted weights of the Biscoe's penguins.
# Example: if a penguin in Biscoe was weighted as 3750, you should correct it to 3750 - 250 = 3500.
# Note: Do not alter the original penguins dataframe! You should alter only the corrected_penguins dataset

corrected_penguins <- penguins
biscoe_island_penguins <- corrected_penguins$island == 'Biscoe'
corrected_penguins$body_mass_g[biscoe_island_penguins] <- corrected_penguins$body_mass_g[biscoe_island_penguins] - 250

