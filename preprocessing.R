require(tidyverse)
require(caret)
require(ggplot2)
require(car)

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
  
  car_data <- car_data %>%
    filter(Cena >= 10000)
  
  # Drop variables with Rok produkcji < 1995
  car_data <- car_data %>%
    filter(Rok_produkcji >= 1995)
  
  car_data <- car_data %>%
    filter(Przebieg_km <= 200000)
  
  car_data <- car_data %>% 
    filter(Pojemnosc_cm3 <= 3000)
  
  car_data <- car_data %>% 
    filter(Moc_km <= 300) 

  # Drop rows with missing values in the specified columns
  car_data <- car_data %>%
    drop_na(all_of(columns_to_drop_na))
  
  # Print the updated dataset
  print(car_data)
  
  # Encode all character columns as factors
  car_data <- car_data %>%
    mutate(across(where(is.character), as.factor))
  
  
  
  # Print the updated dataset
  print(car_data)
  
  return(car_data)
}

create_box_plots <- function(data) {
  
  boxplot_data <- data.frame(
    Rok_produkcji = data$Rok_produkcji,
    Przebieg_km = data$Przebieg_km,
    Pojemnosc_cm3 = data$Pojemnosc_cm3,
    Moc_km = data$Moc_km,
    Paliwo = data$Paliwo,
    Skrzynia = data$Skrzynia,
    Naped = data$Naped,
    Nadwozie = data$Nadwozie,
    Pierwszy_wlasciciel = data$Pierwszy_wlasciciel,
    ASO = data$ASO,
    Bezwypadkowy = data$Bezwypadkowy,
    Wystawca = data$Wystawca
  )
  
  # Create box plots using ggplot2
  create_box_plots.Rok_produkcji <- function(data) {
    ggplot(data, aes(x = "", y = Rok_produkcji)) +
      geom_boxplot(fill = "darkred", color = "black") +
      labs(y = "", title = "Year of production") +
      theme_minimal()
  }
  
  create_box_plots.Przebieg_km <- function(data) {
    ggplot(data, aes(x = "", y = Przebieg_km)) +
      geom_boxplot(fill = "darkred", color = "black") +
      labs(y = "", title = "Box Plot of Mileage [km]") +
      theme_minimal()
  }
  
  create_box_plots.Pojemnosc_cm3 <- function(data) {
    ggplot(data, aes(x = "", y = Pojemnosc_cm3)) +
      geom_boxplot(fill = "darkred", color = "black") +
      labs(y = "", title = "Box Plot of Engine capacity [cm^3]") +
      theme_minimal()
  }
  
  create_box_plots.Moc_km <- function(data) {
    ggplot(data, aes(x = "", y = Moc_km)) +
      geom_boxplot(fill = "darkred", color = "black") +
      labs(y = "", title = "Box Plot of Power [km]") +
      theme_minimal()
  }
  
  # Call the respective box plot methods
  print(create_box_plots.Rok_produkcji(boxplot_data))
  print(create_box_plots.Przebieg_km(boxplot_data))
  print(create_box_plots.Pojemnosc_cm3(boxplot_data))
  print(create_box_plots.Moc_km(boxplot_data))

}

create_box_plots(car_data)
