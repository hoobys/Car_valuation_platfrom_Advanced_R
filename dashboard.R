library(shiny)
library(shinydashboard)
library(caret)
library(xgboost)

# Load the xgboost model if it's saved
xgb_model <- readRDS("xgb_model.rds")

# UI
ui <- dashboardPage(
  
  dashboardHeader(title = "Used Car Valuation Platform"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home"),
      menuItem("Predictions", tabName = "predictions"),
      menuItem("Scrape and Predict", tabName = "scrape_predict")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "home",
              div(style = "display: flex; flex-direction: column; justify-content: center; align-items: center;",
                  tags$img(src = "logo.png", height = 200),
                  h2("Welcome to the Used Car Valuation Platform Dashboard!"),
                  p("Select the features on the sidebar to make predictions.")
              )
      ),
      tabItem(tabName = "predictions",
              h2("Car Price Predictions"),
              fluidRow(
                column(width = 4, numericInput("Rok_produkcji", "Production Year", value = 2010, min = 1990, max = 2023)),
                column(width = 4, numericInput("Przebieg_km", "Mileage (km)", value = 50000, min = 0, max = 500000)),
                column(width = 4, numericInput("Pojemnosc_cm3", "Engine Size (cm3)", value = 2000, min = 800, max = 6000)),
                column(width = 4, numericInput("Moc_km", "Power (km)", value = 100, min = 50, max = 500)),
                column(width = 4, selectInput("Paliwo", "Fuel", choices = levels(car_data$Paliwo))),
                column(width = 4, selectInput("Skrzynia", "Transmission", choices = levels(car_data$Skrzynia))),
                column(width = 4, selectInput("Naped", "Drive", choices = levels(car_data$Naped))),
                column(width = 4, selectInput("Nadwozie", "Body", choices = levels(car_data$Nadwozie))),
                column(width = 4, selectInput("Pierwszy_wlasciciel", "First owner", choices = levels(car_data$Pierwszy_wlasciciel))),
                column(width = 4, selectInput("ASO", "ASO", choices = levels(car_data$ASO))),
                column(width = 4, selectInput("Bezwypadkowy", "Accident free", choices = levels(car_data$Bezwypadkowy))),
                column(width = 4, selectInput("Wystawca", "Exhibitor", choices = levels(car_data$Wystawca)))
              ),
              br(),
              actionButton("predict", "Predict"),
              br(),
              h3("Predicted Car Prices:"),
              div(style = "text-align: center; font-size: 24px; color: green", textOutput("lower_price_output")),
              div(style = "text-align: center; font-size: 36px;", textOutput("predicted_output")),
              div(style = "text-align: center; font-size: 24px; color: red", textOutput("higher_price_output"))      
    ),
    tabItem(tabName = "scrape_predict",
            h2("Scrape Car Data and Predict Price"),
            textInput("url", "Enter the URL of the car listing"),
            actionButton("scrape_predict", "Scrape Data and Predict"),
            br(),
            h3("Scraped Car Data:"),
            verbatimTextOutput("scraped_data_output"),
            h3("Predicted Car Price:"),
            htmlOutput("scraped_price_output")
    ))
  )
)

# Server
server <- function(input, output) {
  
  # Reactive expression for prediction
  predicted_prices <- eventReactive(input$predict, {
    newdata <- data.frame(
      Rok_produkcji = input$Rok_produkcji,
      Przebieg_km = input$Przebieg_km,
      Pojemnosc_cm3 = input$Pojemnosc_cm3,
      Moc_km = input$Moc_km,
      Paliwo = factor(input$Paliwo, levels = levels(car_data$Paliwo)),
      Skrzynia = factor(input$Skrzynia, levels = levels(car_data$Skrzynia)),
      Naped = factor(input$Naped, levels = levels(car_data$Naped)),
      Nadwozie = factor(input$Nadwozie, levels = levels(car_data$Nadwozie)),
      Pierwszy_wlasciciel = factor(input$Pierwszy_wlasciciel, levels = levels(car_data$Pierwszy_wlasciciel)),
      ASO = factor(input$ASO, levels = levels(car_data$ASO)),
      Bezwypadkowy = factor(input$Bezwypadkowy, levels = levels(car_data$Bezwypadkowy)),
      Wystawca = factor(input$Wystawca, levels = levels(car_data$Wystawca))
    )
    
    # Make the prediction using the xgboost model
    prediction <- carModel$predict(newdata, method = "xgb")
    
    return(prediction)
  })
  
  
  output$predicted_output <- renderText({
    predicted_price <- predicted_prices()
    paste0("PLN ", round(predicted_price, 2))
  })
  
  output$lower_price_output <- renderText({
    predicted_price <- predicted_prices()
    lower_price <- predicted_price * 0.95
    paste0("Lower confidence bound: PLN ", round(lower_price, 2))
  })
  
  output$higher_price_output <- renderText({
    predicted_price <- predicted_prices()
    higher_price <- predicted_price * 1.05
    paste0("Higher confidence bound: PLN ", round(higher_price, 2))
  })
  
  # Reactive expression for scraping and prediction
  scraped_and_predicted <- eventReactive(input$scrape_predict, {
    # Scrape data from the provided URL
    scraped_data <- scrape_car_data(input$url)
    
    # Original, scraped price
    original_price <- scrape_car_price(input$url)
    
    # Use the scraped data to predict the car price
    prediction <- predict_car_price(scraped_data, car_data, xgb_model)
    
    list(scraped_data = scraped_data, prediction = prediction, original_price = original_price)
  })
  
  output$scraped_data_output <- renderPrint({
    scraped_and_predicted()$scraped_data
  })
  
  output$scraped_price_output <- renderUI({
    scraped_price <- scraped_and_predicted()$original_price
    predicted_price <- scraped_and_predicted()$prediction
    price_diff <- predicted_price - scraped_price
    percentage_diff <- (price_diff / scraped_price) * 100
    
    color <- ifelse(price_diff > 0, "green", "red")
    
    HTML(
      paste0(
        "<div style='text-align: center; font-size: 24px;'>",
        "Listed Price: PLN ", round(scraped_price, 2),
        "</div>",
        "<div style='text-align: center; font-size: 36px;'>",
        "Predicted Price: PLN ", round(predicted_price, 2),
        "</div>",
        "<div style='text-align: center; font-size: 24px; color:", color, ";'>",
        "Difference: PLN ", round(price_diff, 2), " (", round(percentage_diff, 2), "%)",
        "</div>"
      )
    )
  })
}

shinyApp(ui = ui, server = server)
