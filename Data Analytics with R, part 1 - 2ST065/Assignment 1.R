library(tidyverse)

data(msleep)

# 1. Convert into factors
msleep$genus <- as.factor(msleep$genus)
msleep$vore <- as.factor(msleep$vore)
msleep$order <- as.factor(msleep$order)
msleep$conservation <- as.factor(msleep$conservation)

# 2. Shortest sleep time
shortest_sleep <- min(msleep$sleep_total, na.rm = TRUE)
shortest_sleep_mammal <- msleep$name[which.min(msleep$sleep_total)]

# 3. Most missing
missing_values <- sapply(msleep, function(x) sum(is.na(x)))
most_missing <- names(missing_values)[which.max(missing_values)]
missing_values <- max(missing_values)

# 4. Correlations
correlations <- cor(msleep[sapply(msleep, is.numeric)], use = "pairwise.complete.obs")

# 5. Highest correlation
correlations_copy <- correlations
diag(correlations_copy) <- 0
highest_corr <- max(correlations_copy)

# 6. Sleep time distribution
sleep_histogram <- ggplot(msleep, aes(x = sleep_total)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  labs(x = "Total Sleep Time", y = "Count", title = "Distribution of Sleep Times")

# 7. Bar chart for food categories
food_barchart <- ggplot(msleep, aes(x = vore)) +
  geom_bar(fill = "blue", color = "black") +
  labs(x = "Food Category", y = "Count", title = "Number of Mammals in Each Food Category")

# 8. Grouped box plot for sleep time
sleep_boxplot <- ggplot(msleep, aes(x = vore, y = sleep_total)) +
  geom_boxplot(fill = "blue", color = "black") +
  labs(x = "Food Category", y = "Total Sleep Time", title = "Sleep Time by Food Category")

# 9. Longest average sleep time
highest_average <- max(tapply(msleep$sleep_total, msleep$vore, mean, na.rm = TRUE))

# 10. REM sleep vs. total sleep, colored by order
sleep_scatterplot <- ggplot(msleep, aes(x = sleep_total, y = sleep_rem, color = order)) +
  geom_point() +
  labs(x = "Total Sleep Time", y = "REM Sleep Time", title = "Total Sleep Time vs. REM Sleep Time")

# 11. REM sleep vs. total sleep for the order most common in the data
most_common_order <- names(which.max(table(msleep$order)))
sleep_scatterplot2 <- ggplot(msleep[msleep$order == most_common_order,], aes(x = sleep_total, y = sleep_rem)) +
  geom_point() +
  labs(x = "Total Sleep Time", y = "REM Sleep Time", title = paste("Total Sleep Time vs. REM Sleep Time for", most_common_order))