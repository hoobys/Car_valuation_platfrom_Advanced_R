require(xgboost)
Rcpp::sourceCpp("rcpp_metrics.cpp")

CarModel <- R6::R6Class("CarModel",
                        private = list(
                          .lm_model = NULL,
                          .xgb_model = NULL,
                          .preprocess_data = function(data, is_training = TRUE) {
                            columns_list <- c("Rok_produkcji", "Przebieg_km", "Pojemnosc_cm3", "Moc_km",
                                              "Paliwo", "Skrzynia", "Naped", "Nadwozie", "Pierwszy_wlasciciel",
                                              "ASO", "Bezwypadkowy", "Wystawca")
                            
                            if (is_training) {
                              data_matrix <- model.matrix(Cena ~ . - 1, data = data[, c("Cena", columns_list)])
                            } else {
                              data_matrix <- model.matrix(~ . - 1, data = data[, columns_list])
                            }
                            
                            return(data_matrix)
                          }
                        ),
                        public = list(
                          preprocess = function(file) {
                            car_data <- preprocess_car_data(file)
                            
                            set.seed(42)  # For reproducibility
                            train_indices <- createDataPartition(car_data$Cena, p = 0.7, list = FALSE)
                            train_data <- car_data[train_indices, ]
                            test_data <- car_data[-train_indices, ]
                            
                            list(train_data = train_data, test_data = test_data)
                          },
                          train = function(train_data, method = "lm") {
                            if (method == "lm") {
                              train_data <- train_data %>% mutate(across(where(is.numeric), scale)) 
                              private$.lm_model <- lm(Cena ~ Rok_produkcji + Przebieg_km + Pojemnosc_cm3 + 
                              Paliwo + Skrzynia + Naped + Nadwozie + Pierwszy_wlasciciel + ASO + Bezwypadkowy, data = train_data)
                            } else if (method == "xgb") {
                              train_data_processed <- private$.preprocess_data(train_data)
                              train_data_matrix <- xgb.DMatrix(data = as.matrix(train_data_processed), label = train_data$Cena)
                              
                              params <- list(
                                objective = "reg:squarederror",
                                eta = 0.3,
                                max_depth = 6,
                                min_child_weight = 1,
                                subsample = 1,
                                colsample_bytree = 1
                              )
                              
                              private$.xgb_model <- xgb.train(
                                params = params,
                                data = train_data_matrix,
                                nrounds = 100,
                                nthread = 2
                              )
                            }
                          },
                          predict = function(test_data, method = "lm") {
                            if (method == "lm") {
                              # Create a separate scaled version of the test data
                              test_data_scaled <- test_data %>% mutate(across(where(is.numeric), scale))
                              test_data_scaled <- test_data_scaled %>% select(-Cena)
                              
                              # Make predictions on the scaled test data
                              predictions_scaled <- predict(private$.lm_model, newdata = test_data_scaled)
                              
                              unscaled_predictions <- predictions_scaled * 24664.76 + 38225.88
                              
                              return(unscaled_predictions)
                            } else if (method == "xgb") {
                              test_data_processed <- private$.preprocess_data(test_data, is_training = FALSE)
                              test_data_matrix <- xgb.DMatrix(data = as.matrix(test_data_processed))
                              predictions <- predict(private$.xgb_model, newdata = test_data_matrix)
                              
                              return(predictions)
                            }
                          },
                          evaluate = function(predictions, test_data) {
                            rmse <- RMSE(predictions, test_data$Cena)
                            mae <- MAE(predictions, test_data$Cena)
                            r_squared <- R2(predictions, test_data$Cena)
                            par <- price_accuracy_ratio(predictions, test_data$Cena)
                            smae <- segment_specific_mae(predictions, test_data$Cena)
                            
                            list(
                              RMSE = rmse,
                              MAE = mae,
                              R_squared = r_squared,
                              PAR = par,
                              SMAE = smae
                            )
                          },
                          save_model = function(file, method = "lm") {
                            if (method == "lm") {
                              saveRDS(private$.lm_model, paste0(file, ".rds"))
                            } else if (method == "xgb") {
                              saveRDS(private$.xgb_model, paste0(file, ".rds"))
                            }
                          }
                        )
)

# Instantiate the CarModel class
carModel <- CarModel$new()

# Preprocess data
data <- carModel$preprocess('otomoto_data.csv')

######################################################

# Train linear model
carModel$train(data$train_data, method = "lm")

# Make predictions
lm_predictions <- carModel$predict(data$test_data, method = "lm")

# Evaluate linear model
lm_metrics <- carModel$evaluate(lm_predictions, data$test_data)
lm_metrics

# Save linear model
carModel$save_model("lm_model", method = "lm")

######################################################

# Train xgboost model
carModel$train(data$train_data, method = "xgb")

# Make predictions
xgb_predictions <- carModel$predict(data$test_data, method = "xgb")

# Evaluate xgboost model
xgb_metrics <- carModel$evaluate(xgb_predictions, data$test_data)
xgb_metrics

# Save xgboost model
carModel$save_model("xgb_model", method = "xgb")

