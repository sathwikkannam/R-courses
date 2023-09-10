# Q1
USArrests <- get(data("USArrests"))
average_murder <- mean(USArrests$Murder)

below_average <- row.names(subset(USArrests, USArrests$Murder < average_murder))
above_average <- row.names(subset(USArrests, USArrests$Murder > average_murder))


# Q2
library(palmerpenguins)
penguins <- get(data("penguins"))

penguins <- penguins[complete.cases(penguins),]

male_penguins <- subset(penguins, penguins$sex == 'male')
female_penguins <- subset(penguins, penguins$sex == 'female')


# Q3
flipper_proportion_male <- mean(male_penguins$flipper_length_mm / male_penguins$body_mass_g)
flipper_proportion_female <- mean(female_penguins$flipper_length_mm / female_penguins$body_mass_g)


# Q4
big_mouth <- subset.data.frame(penguins, penguins$bill_length_mm < 2 * penguins$bill_depth_mm)


# Q5
adelie_big_mouth_proportion <- nrow(big_mouth) / nrow(subset(penguins, penguins$species == 'Adelie'))


# Q6
corrected_penguins <- penguins
biscoe_island_penguins <- corrected_penguins$island == 'Biscoe'
corrected_penguins$body_mass_g[biscoe_island_penguins] <- corrected_penguins$body_mass_g[biscoe_island_penguins] - 250