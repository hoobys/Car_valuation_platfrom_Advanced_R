# Split the data into training and testing sets
set.seed(42)  # For reproducibility
train_indices <- createDataPartition(car_data$Cena, p = 0.7, list = FALSE)
train_data <- car_data[train_indices, ]
test_data <- car_data[-train_indices, ]

train_data

# Column list
columns_list <- c("Rok_produkcji", "Przebieg_km", "Pojemnosc_cm3", "Moc_km",
                  "Paliwo", "Skrzynia", "Naped", "Nadwozie", "Pierwszy_wlasciciel",
                  "ASO", "Bezwypadkowy", "Wystawca")

# Preprocess the data for one-hot encoding
preprocess_data <- function(data, is_training = TRUE) {
  if (is_training) {
    data_matrix <- model.matrix(Cena ~ . - 1, data = data[, c("Cena", columns_list)])
  } else {
    data_matrix <- model.matrix(~ . - 1, data = data[, columns_list])
  }
  
  return(data_matrix)
}


train_data_processed <- preprocess_data(train_data)
test_data_processed <- preprocess_data(test_data)

train_data_processed

train_data_matrix <- xgb.DMatrix(data = as.matrix(train_data_processed), label = train_data$Cena)
test_data_matrix <- xgb.DMatrix(data = as.matrix(test_data_processed))

# Specify parameters for the xgboost model
params <- list(
  objective = "reg:squarederror",
  eta = 0.3,
  max_depth = 6,
  min_child_weight = 1,
  subsample = 1,
  colsample_bytree = 1
)

# Train an xgboost model
xgb_model <- xgb.train(
  params = params,
  data = train_data_matrix,
  nrounds = 100,
  nthread = 2
)

# Make predictions on the test data
predictions <- predict(xgb_model, newdata = test_data_matrix)

# Evaluate model performance
rmse <- RMSE(predictions, test_data$Cena)
mae <- MAE(predictions, test_data$Cena)
r_squared <- R2(predictions, test_data$Cena)

# Print evaluation metrics
print(paste("RMSE:", rmse))
print(paste("MAE:", mae))
print(paste("R-squared:", r_squared))

saveRDS(xgb_model, "xgb_model.rds")
