
car_data <- preprocess_car_data('otomoto_data.csv')

# Scale all numeric columns
car_data <- car_data %>%
  mutate(across(where(is.numeric), scale)) 

# Split the data into training and testing sets
set.seed(42)  # For reproducibility
train_indices <- createDataPartition(car_data$Cena, p = 0.7, list = FALSE)
train_data <- car_data[train_indices, ]
test_data <- car_data[-train_indices, ]

# Train a linear regression model
lm_model <- train(Cena ~ Rok_produkcji + 
                    Przebieg_km + 
                    Pojemnosc_cm3 + 
                    Moc_km + 
                    Paliwo + 
                    Skrzynia + 
                    Naped + 
                    Nadwozie + 
                    Pierwszy_wlasciciel + 
                    ASO + 
                    Bezwypadkowy, data = train_data, method = "lm")

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

saveRDS(lm_model, "lm_model.rds")

