require(tidyverse)
require(caret)
require(ggplot2)
require(car)

preprocess_car_data <- function(otomoto_data.csv) {
  
    if (!file.exists(otomoto_data.csv)) {
    stop("Input file does not exist.")
  }
  
  if (!grepl("\\.csv$", otomoto_data.csv)) {
    stop("Input file must be in CSV format.")
  }
  
  car_data <- tryCatch(
    {
      read.csv(otomoto_data.csv)
    },
    error = function(e) {
      stop("Error reading the car dataset:", conditionMessage(e))
    },
    warning = function(w) {
      stop("Warning reading the car dataset:", conditionMessage(w))
    }
  )
  
  required_columns <- c("Cena", "Rok_produkcji", "Przebieg_km", "Moc_km", "Drzwi", "Miejsca", "Pojemnosc_cm3")
  missing_columns <- setdiff(required_columns, colnames(car_data))
  if (length(missing_columns) > 0) {
    stop(paste("Missing columns:", paste(missing_columns, collapse = ", ")))
  }
  
  car_data <- distinct(car_data)
  
  car_data <- car_data %>%
    filter(Cena <= 100000)
  
  car_data <- car_data %>%
    filter(Cena >= 10000)
  
  car_data <- car_data %>%
    filter(Rok_produkcji >= 1995)
  
  car_data <- car_data %>%
    filter(Przebieg_km <= 200000)
  
  car_data <- car_data %>%
    filter(Pojemnosc_cm3 <= 3000)
  
  car_data <- car_data %>%
    filter(Moc_km <= 300)
  
  if (nrow(car_data) < 5000) {
    warning("Small sample size for modeling.")
  }
  
  
  car_data <- car_data %>%
    drop_na(all_of(required_columns))
  
  car_data <- car_data %>%
    mutate(across(where(is.character), as.factor))
  
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
  

  print(create_box_plots.Rok_produkcji(boxplot_data))
  print(create_box_plots.Przebieg_km(boxplot_data))
  print(create_box_plots.Pojemnosc_cm3(boxplot_data))
  print(create_box_plots.Moc_km(boxplot_data))
  
}

car_data <- preprocess_car_data('otomoto_data.csv')

create_box_plots(car_data)
