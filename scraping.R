library(rvest)
library(assertthat)

# This function scrapes car data from the provided URL
scrape_car_data <- function(url) {
  # Check if URL is valid
  assert_that(!is.null(url) && nzchar(url) && grepl("^http[s]?://", url),
              msg = "Invalid URL provided.")
  
  webpage <- read_html(url)
  
  # Extract all offer parameters items
  offer_params <- webpage %>%
    html_nodes(".offer-params__item")
  
  # If offer_params is empty, throw an error
  if(length(offer_params) == 0) {
    stop("No offer parameters found in the provided URL.")
  }
  
  car_data <- list()
  
  for(i in seq_along(offer_params)) {
    attr_name <- offer_params[[i]] %>%
      html_node(".offer-params__label") %>%
      html_text() %>%
      trimws()
    
    attr_value <- offer_params[[i]] %>%
      html_node(".offer-params__value") %>%
      html_text() %>%
      trimws()
    
    car_data[[attr_name]] <- attr_value
  }
  
  car_data <- as.data.frame(t(unlist(car_data)))
  
  # Assign 'Nie' to the specified attributes if they are not present
  specified_attributes <- c("Pierwszy właściciel (od nowości)", "Serwisowany w ASO", "Bezwypadkowy")
  for(attr in specified_attributes) {
    if(is.null(car_data[[attr]])) {
      car_data[[attr]] <- 'Nie'
    }
  }
  
  # Fix if 'Napęd' is missing in offer
  if(is.null(car_data[["Napęd"]])) {
    car_data[["Napęd"]] <- 'Na przednie koła'
  }
  

  # Convert numeric columns to numeric data type
  numeric_columns <- c("Rok produkcji", "Przebieg", "Pojemność skokowa", "Moc")
  for(col in numeric_columns) {
    car_data[[col]] <- as.numeric(gsub("\\D", "", car_data[[col]]))  # Remove non-digit characters and convert to numeric
  }
  
  # Column list
  columns_list <- c("Rok_produkcji", "Przebieg_km", "Pojemnosc_cm3", "Moc_km",
                    "Paliwo", "Skrzynia", "Naped", "Nadwozie", "Pierwszy_wlasciciel",
                    "ASO", "Bezwypadkowy", "Wystawca")
  
  # Column names mapping
  col_mapping <- c("Rok produkcji" = "Rok_produkcji", 
                   "Przebieg" = "Przebieg_km", 
                   "Pojemność skokowa" = "Pojemnosc_cm3", 
                   "Moc" = "Moc_km",
                   "Rodzaj paliwa" = "Paliwo",
                   "Skrzynia biegów" = "Skrzynia",
                   "Napęd" = "Naped",
                   "Typ nadwozia" = "Nadwozie",
                   "Pierwszy właściciel (od nowości)" = "Pierwszy_wlasciciel",
                   "Serwisowany w ASO" = "ASO",
                   "Bezwypadkowy" = "Bezwypadkowy",
                   "Oferta od" = "Wystawca"
  )
  
  
  # Rename the columns
  names(car_data) <- col_mapping[names(car_data)]

  # Select only the specified columns
  car_data <- car_data[, columns_list, drop = FALSE]
  
  # Encode all character columns as factors
  car_data <- car_data %>%
    mutate(across(where(is.character), as.factor))
  
  
  return(car_data)
}


# This function scrapes the car price from the provided URL
scrape_car_price <- function(url) {
  # Check if URL is valid
  assert_that(!is.null(url) && nzchar(url) && grepl("^http[s]?://", url),
              msg = "Invalid URL provided.")
  
  webpage <- read_html(url)
  
  car_price <- webpage %>%
    html_nodes(xpath = '/html/body/div[4]/main/div[1]/div[1]/div[1]/div/div[5]/div[2]/div[2]/div/span[1]') %>%
    html_text() %>%
    str_replace_all(" ", "") %>%
    str_replace_all("PLN", "") %>%
    as.numeric()
  
  # If car_price is NA or negative, throw an error
  if(is.na(car_price) || car_price < 0) {
    stop("Invalid car price found in the provided URL.")
  }
  
  return(car_price)
}

# This function uses the scraped data and the xgb and rf models to predict the car price
predict_car_price <- function(scraped_data, car_data, model) {
  # Check if scraped_data, car_data, and model are not null
  assert_that(!is.null(scraped_data), msg = "Scraped data cannot be null.")
  assert_that(!is.null(car_data), msg = "Car data cannot be null.")
  assert_that(!is.null(model), msg = "Model cannot be null.")
  
  # Prepare the data frame for prediction
  newdata <- data.frame(
    Rok_produkcji = scraped_data$Rok_produkcji,
    Przebieg_km = scraped_data$Przebieg_km,
    Pojemnosc_cm3 = scraped_data$Pojemnosc_cm3,
    Moc_km = scraped_data$Moc_km,
    Paliwo = factor(scraped_data$Paliwo, levels = levels(car_data$Paliwo)),
    Skrzynia = factor(scraped_data$Skrzynia, levels = levels(car_data$Skrzynia)),
    Naped = factor(scraped_data$Naped, levels = levels(car_data$Naped)),
    Nadwozie = factor(scraped_data$Nadwozie, levels = levels(car_data$Nadwozie)),
    Pierwszy_wlasciciel = factor(scraped_data$Pierwszy_wlasciciel, levels = levels(car_data$Pierwszy_wlasciciel)),
    ASO = factor(scraped_data$ASO, levels = levels(car_data$ASO)),
    Bezwypadkowy = factor(scraped_data$Bezwypadkowy, levels = levels(car_data$Bezwypadkowy)),
    Wystawca = factor(scraped_data$Wystawca, levels = levels(car_data$Wystawca))
  )
  
  # Make the prediction using the specified model
  prediction <- carModel$predict(newdata, method = model)

  return(prediction)
}
