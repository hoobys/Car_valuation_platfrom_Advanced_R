require(tidyverse)
require(caret)

preprocess_car_data <- function(file_path) {
  
  # Read the car dataset into a data frame
  car_data <- read.csv(file_path)
  
  str(car_data)
  
  # Data Cleaning
  # Remove duplicate records
  car_data <- distinct(car_data)
  
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
  
  # Drop rows with missing values in the specified columns
  car_data <- car_data %>%
    drop_na(all_of(columns_to_drop_na))
  
  # Impute missing values in "Spalanie miasto l" with the median
  car_data <- car_data %>%
    mutate(Spalanie_miasto_l.100km = if_else(is.na(Spalanie_miasto_l.100km),
                                             median(Spalanie_miasto_l.100km, na.rm = TRUE),
                                             Spalanie_miasto_l.100km))
  
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
  
  # Encode all character columns as factors
  car_data <- car_data %>%
    mutate(across(where(is.character), as.factor))
  
  
  
  # Print the updated dataset
  print(car_data)
  
  return(car_data)
}
