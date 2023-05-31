library(shiny)
library(shinydashboard)
library(caret)
library(xgboost)

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
              p("As the site is in Polish, you have to choose the according features using their respective polish names."),
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
              fluidRow(
                column(width = 6,
                       h3("XGBoost Predicted Price:"),
                       div(style = "text-align: center; font-size: 24px; color: green", textOutput("xgb_lower_price_output")),
                       div(style = "text-align: center; font-size: 36px;", textOutput("xgb_predicted_output")),
                       div(style = "text-align: center; font-size: 24px; color: red", textOutput("xgb_higher_price_output"))),
                column(width = 6,
                       h3("Random Forest Predicted Price:"),
                       div(style = "text-align: center; font-size: 24px; color: green", textOutput("rf_lower_price_output")),
                       div(style = "text-align: center; font-size: 36px;", textOutput("rf_predicted_output")),
                       div(style = "text-align: center; font-size: 24px; color: red", textOutput("rf_higher_price_output")))
              ),
    ),
    tabItem(tabName = "scrape_predict",
            h2("Scrape OTOMOTO car data and compare prices"),
            textInput("url", "Enter the URL of the OTOMOTO car listing"),
            actionButton("scrape_predict", "Scrape Data and Predict"),
            br(),
            h3("Scraped car data:"),
            verbatimTextOutput("scraped_data_output"),
            h3("Predicted car price:"),
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
    
    # Make the prediction using the xgb and rf models
    prediction_xgb <- carModel$predict(newdata, method='xgb')
    prediction_rf <- carModel$predict(newdata, method='rf')
    
    return(list(
      xgb = prediction_xgb,
      rf = prediction_rf
    ))
  })
  
  #####################################################
  
  output$xgb_predicted_output <- renderText({
    predicted_price <- predicted_prices()$xgb
    paste0("PLN ", round(predicted_price, 2))
  })
  
  output$xgb_lower_price_output <- renderText({
    predicted_price <- predicted_prices()$xgb
    lower_price <- predicted_price * 0.95
    paste0("Lower confidence bound: PLN ", round(lower_price, 2))
  })
  
  output$xgb_higher_price_output <- renderText({
    predicted_price <- predicted_prices()$xgb
    higher_price <- predicted_price * 1.05
    paste0("Higher confidence bound: PLN ", round(higher_price, 2))
  })
  
  #####################################################
  
  output$rf_predicted_output <- renderText({
    predicted_price <- predicted_prices()$rf
    paste0("PLN ", round(predicted_price, 2))
  })
  
  output$rf_lower_price_output <- renderText({
    predicted_price <- predicted_prices()$rf
    lower_price <- predicted_price * 0.95
    paste0("Lower confidence bound: PLN ", round(lower_price, 2))
  })
  
  output$rf_higher_price_output <- renderText({
    predicted_price <- predicted_prices()$rf
    higher_price <- predicted_price * 1.05
    paste0("Higher confidence bound: PLN ", round(higher_price, 2))
  })
  
  # Reactive expression for scraping and prediction
  scraped_and_predicted <- eventReactive(input$scrape_predict, {
    scraped_data <- scrape_car_data(input$url)
    
    original_price <- scrape_car_price(input$url)
    
    prediction_xgb <- predict_car_price(scraped_data, car_data, 'xgb')
    prediction_rf <- predict_car_price(scraped_data, car_data, 'rf')
    
    list(scraped_data = scraped_data, 
         prediction_xgb = prediction_xgb, 
         prediction_rf = prediction_rf,
         original_price = original_price)
  })
  
  output$scraped_data_output <- renderPrint({
    scraped_and_predicted()$scraped_data
  })
  
  output$scraped_price_output <- renderUI({
    scraped_price <- scraped_and_predicted()$original_price
    predicted_xgb <- scraped_and_predicted()$prediction_xgb
    predicted_rf <- scraped_and_predicted()$prediction_rf
    
    price_diff_xgb <- predicted_xgb - scraped_price
    price_diff_rf <- predicted_rf - scraped_price
    
    percentage_diff_xgb <- (price_diff_xgb / scraped_price) * 100
    percentage_diff_rf <- (price_diff_rf / scraped_price) * 100
    
    color_xgb <- ifelse(price_diff_xgb > 0, "green", "red")
    color_rf <- ifelse(price_diff_rf > 0, "green", "red")
    
    HTML(
      paste0(
        "<div style='display: flex; flex-direction: row; justify-content: center;'>",
        
        "<div style='text-align: center; flex-basis: 50%;'>",
        "<div style='font-size: 24px;'>Listed Price: PLN ", round(scraped_price, 2), "</div>",
        "<div style='font-size: 36px;'>XGBoost Predicted Price: PLN ", round(predicted_xgb, 2), "</div>",
        "<div style='font-size: 24px; color:", color_xgb, ";'>Difference (XGBoost): PLN ", round(price_diff_xgb, 2), " (", round(percentage_diff_xgb, 2), "%)</div>",
        "</div>",
        
        "<div style='text-align: center; flex-basis: 50%;'>",
        "<div style='font-size: 24px;'>Listed Price: PLN ", round(scraped_price, 2), "</div>",
        "<div style='font-size: 36px;'>Random Forest Predicted Price: PLN ", round(predicted_rf, 2), "</div>",
        "<div style='font-size: 24px; color:", color_rf, ";'>Difference (Random Forest): PLN ", round(price_diff_rf, 2), " (", round(percentage_diff_rf, 2), "%)</div>",
        "</div>",
        
        "</div>"
      )
    )
  })
}

shinyApp(ui = ui, server = server)
