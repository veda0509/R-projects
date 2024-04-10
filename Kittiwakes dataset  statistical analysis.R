library(ggplot2)
library(knitr)

# Question 1

observation_data <- read.csv("Kittiwake_Observation_20601922.csv")
head(observation_data)
summary_stats <- summary(observation_data)
table_output <- kable(summary_stats)
print(table_output)

hist(observation_data$dawn, main = "Kittiwake Sighting Distribution at dawn", xlab = "Number of Sightings")
hist(observation_data$noon, main = "Kittiwake Sighting Distribution at noon", xlab = "Number of Sightings")
hist(observation_data$`mid.afternoon`, main = "Kittiwake Sighting Distribution at mid-afternoon", xlab = "Number of Sightings")
hist(observation_data$dusk, main = "Kittiwake Sighting Distribution at dusk", xlab = "Number of Sightings")

dawn_data <- observation_data$dawn

dawn_confidence_interval <- t.test(dawn_data, conf.level = 0.90)

print(dawn_confidence_interval)

hist(dawn_data, col = "lightblue", main = "Histogram of Dawn Observations",
     xlab = "Number of Kittiwakes", ylab = "Frequency")

abline(v = 34.5, col = "purple", lty = 2, lwd = 2)
abline(v = 31.17841, col = "green", lty = 2, lwd = 2)
abline(v = 37.82159, col = "green", lty = 2, lwd = 2)


# Question 2

historical_data <- read.csv("Kittiwake_Historical_20601922.csv")
chi_square_result <- chisq.test(historical_data[, -1])
print(chi_square_result)

model <- lm(Site.C ~ X, data = historical_data)
prediction <- predict(model, newdata = data.frame(X = 2006))
print (round(prediction))

plot(historical_data[, 1], historical_data[,4], 
     col = "blue", xlab = "Year", ylab = "Breeding Pairs",
     main = "Breeding Pairs at Site C Over Years")

points(2006, prediction, col = "red", pch = 16)

########### Question 3

measurement_data <- read.csv("Kittiwake_Measurement_20601922.csv")
head(measurement_data)
pairs(measurement_data[, 2:4], main = "Scatterplot Matrix for Measurement Data")

create_boxplot <- function(data, x_var, y_var, title) {
  ggplot(data, aes_string(x = x_var, y = y_var)) +
    geom_boxplot() +
    labs(title = title,
         x = x_var,
         y = y_var)
}

create_boxplot(measurement_data, "Sub.species", "Weight", "Weight by Sub-Species")
create_boxplot(measurement_data, "Sub.species", "Wingspan", "Wingspan by Sub-Species")
create_boxplot(measurement_data, "Sub.species", "Culmen", "Culmen Length by Sub-Species")

black_legged_data <- subset(measurement_data, Sub.species == "Black-legged")
red_legged_data <- subset(measurement_data, Sub.species == "Red-legged")

black_legged_cor <- cor.test(black_legged_data$Wingspan, black_legged_data$Culmen, method = "pearson")
red_legged_cor <- cor.test(red_legged_data$Wingspan, red_legged_data$Culmen, method = "pearson")

print(black_legged_cor)
print(red_legged_cor)


# p value less than 0.05 so we will reject null hypothesis and there is a relation between culmen and wing span for black legged

# p value greater than 0.05 for red legged, we fail to reject null hypothesis so there is not enough evidence to predict this relation

weight_relation_specie <- t.test(Weight ~ Sub.species, data = measurement_data)

print(weight_relation_specie)

# p value greater than 0.05 we conclude that there isn't enough evidence to suggest a significant difference in weights 
# between the two sub-species.

black_legged_wingspan <- subset(measurement_data, Sub.species == "Black-legged")$Wingspan
red_legged_wingspan <- subset(measurement_data, Sub.species == "Red-legged")$Wingspan

# Independent two-sample t-test
t_test_result_wingspan <- t.test(black_legged_wingspan, red_legged_wingspan)

print (t_test_result_wingspan)

# p value smaller

black_legged_culmen <- subset(measurement_data, Sub.species == "Black-legged")$Culmen
red_legged_culmen <- subset(measurement_data, Sub.species == "Red-legged")$Culmen

# Independent two-sample t-test
t_test_result_culmen <- t.test(black_legged_culmen, red_legged_culmen)

print (t_test_result_culmen)

# p value greater


# Conclusion:
# No difference in weight and culmen length but difference in wingspan

########## Question 4

location_data <- read.csv("Kittiwake_Location_20601922.csv")

linear_model_location <- lm(Breeding.pairs ~ Coast.direction + sandeel + Summer.temp + cliff.height, data = location_data)
summary(linear_model_location)
par(mfrow = c(2, 2))
plot(linear_model_location)

location_data$log_Breeding.pairs <- log(location_data$Breeding.pairs)
linear_model_log <- lm(log_Breeding.pairs ~ Coast.direction + sandeel + Summer.temp + cliff.height, data = location_data)
summary(linear_model_log)
par(mfrow = c(2, 2))
plot(linear_model_log)

AIC(linear_model_location)
AIC(linear_model_log)

# Logarithmic Model
# Model Fit:
#   The Multiple R-squared value is very high (0.9842), indicating excellent model fit.
# The Adjusted R-squared is also very high (0.9778), suggesting that the model explains a large portion of the variance in the log of the number of breeding pairs.
# The F-statistic p-value is extremely small, indicating a statistically significant model.
# Effect of Covariates:
#   Coast Direction: Similar to the original model, all coast direction categories have significant negative coefficients, indicating a negative association with the log of the number of breeding pairs.
# Sandeel: The significant positive coefficient suggests a positive relationship with the log of the number of breeding pairs.
# Summer Temperature: Again, not statistically significant.
# Cliff Height: The positive and significant coefficient indicates that higher cliffs are associated with higher numbers of breeding pairs, even when considering the logarithmic scale.

new_data <- data.frame(
  Summer.temp = 24.4,
  cliff.height = log(3.1),
  sandeel = 2.21,
  Coast.direction = "East"
)

predicted_log_breeding_pairs <- predict(linear_model_log, newdata = new_data, interval = "confidence", level = 0.80)

confidence_interval <- exp(predicted_log_breeding_pairs[, 2:3])
cat("98% Confidence Interval for the Number of Breeding Pairs:",
    round(confidence_interval[1], 2), "to", round(confidence_interval[2], 2))


