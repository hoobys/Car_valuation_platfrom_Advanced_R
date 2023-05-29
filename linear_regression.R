
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
lm_model <- lm(Cena ~ Rok_produkcji + 
                    Przebieg_km + 
                    Pojemnosc_cm3 + 
                    Paliwo + 
                    Skrzynia + 
                    Naped + 
                    Nadwozie + 
                    Pierwszy_wlasciciel + 
                    ASO + 
                    Bezwypadkowy, data = train_data)

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
par <- price_accuracy_ratio(predictions, test_data$Cena)
smae <- segment_specific_mae(predictions, test_data$Cena)

# Print evaluation metrics
print(paste("RMSE:", rmse))
print(paste("MAE:", mae))
print(paste("R-squared:", r_squared))
print(paste("PAR:", par))
print(paste("Segment specific MAE:", smae))

saveRDS(lm_model, "lm_model.rds")

# Assumption checks for linear regression

# Residual analysis
residuals <- residuals(lm_model)

# Linearity and nonlinearity test (Breusch-Pagan test)
crPlots(lm_model)

# Residual plots
plot(lm_model, which = 1)  # Standardized residuals vs. fitted values
plot(lm_model, which = 2)  # Normal Q-Q plot of residuals
plot(lm_model, which = 3)  # Scale-location plot of residuals
plot(lm_model, which = 5)  # Cook's distance plot

resettest(lm_model)   # RESET test
car::ncvTest(lm_model)   # Non-constant Variance Test (Breusch-Pagan test)
car::vif(lm_model)   # Variance Inflation Factor (VIF)

