# Load required libraries
library(tidyverse)
library(caret)
library(randomForest)

# Read the car dataset into a data frame
car_data <- read.csv("otomoto_data.csv")

str(car_data)

# Data Cleaning
# Remove duplicate records
car_data <- distinct(car_data)

# Check the number of missing values in each column
missing_counts <- car_data %>%
  summarise_all(~ sum(is.na(.)))

# Print the missing value counts
print(missing_counts)

# Specify the columns where you want to drop rows with missing values
columns_to_drop_na <- c("Cena", 
                        "Rok_produkcji", 
                        "Przebieg_km", 
                        "Moc_km", 
                        "Drzwi",
                        "Miejsca",
                        "Pojemnosc_cm3")

summary(car_data)

# Drop variables with Cena > 100,000
car_data <- car_data %>%
  filter(Cena <= 100000)

# Plot histogram of Cena
ggplot(car_data, aes(x = Cena)) +
  geom_histogram(binwidth = 5000, fill = "steelblue", color = "white") +
  labs(x = "Cena", y = "Count") +
  ggtitle("Distribution of Car Prices")

# Drop rows with missing values in the specified columns
car_data <- car_data %>%
  drop_na(all_of(columns_to_drop_na))

# Impute missing values in "Spalanie miasto l" with the median
car_data <- car_data %>%
  mutate(Spalanie_miasto_l.100km = if_else(is.na(Spalanie_miasto_l.100km),
                                           median(Spalanie_miasto_l.100km, na.rm = TRUE),
                                           Spalanie_miasto_l.100km))

missing_counts <- car_data %>%
  summarise_all(~ sum(is.na(.)))
print(missing_counts)

# Feature Engineering
# Age of the car
current_year <- as.numeric(format(Sys.Date(), "%Y"))
car_data <- car_data %>%
  mutate(Age_of_car = current_year - Rok_produkcji)

# Engine power per unit weight
car_data <- car_data %>%
  mutate(Power_to_weight_ratio = Moc_km / Pojemnosc_cm3)

# Fuel efficiency
car_data <- car_data %>%
  mutate(Fuel_efficiency = Przebieg_km / Spalanie_miasto_l.100km)

# Polynomial features
car_data <- car_data %>%
  mutate(Pojemnosc_cm3_squared = Pojemnosc_cm3^2)

# Interaction terms
car_data <- car_data %>%
  mutate(Power_fuel_interaction = Moc_km * as.integer(Paliwo == "Benzyna"))

# Log transformations
car_data <- car_data %>%
  mutate(Log_Power = log(Moc_km + 1))

# Drop the "Marka" column
car_data <- car_data %>%
  select(-Marka)

# Print the updated dataset
print(car_data)

# Encode categorical variables and scale numerical variables
car_data <- car_data %>%
  mutate(across(where(is.character), as.factor),  # Encode all character columns as factors
         across(where(is.numeric), scale))         # Scale all numeric columns

# Print the updated dataset
print(car_data)

str(car_data)

# Split the data into training and testing sets
set.seed(42)  # For reproducibility
train_indices <- createDataPartition(car_data$Cena, p = 0.7, list = FALSE)
train_data <- car_data[train_indices, ]
test_data <- car_data[-train_indices, ]

# Train a linear regression model
lm_model <- train(Cena ~ ., data = train_data, method = "lm")

# Make predictions on the test data
test_data_scaled <- test_data %>% select(-Cena)  # Exclude the response variable
predictions_scaled <- predict(lm_model, newdata = test_data_scaled)

# Scaling parameters for Cena
scaled_center <- attr(car_data$Cena, "scaled:center")
scaled_scale <- attr(car_data$Cena, "scaled:scale")

# Unscaled predictions
unscaled_predictions <- predictions_scaled * scaled_scale + scaled_center

# Evaluate model performance
rmse <- RMSE(unscaled_predictions, test_data$Cena)
mae <- MAE(unscaled_predictions, test_data$Cena)
r_squared <- R2(unscaled_predictions, test_data$Cena)
mape <- mean(abs((test_data$Cena - unscaled_predictions) / test_data$Cena)) * 100

# Print evaluation metrics
print(paste("RMSE:", rmse))
print(paste("MAE:", mae))
print(paste("R-squared:", r_squared))
print(paste("MAPE:", mape))
