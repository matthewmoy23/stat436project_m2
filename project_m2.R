# Milestone 2
# Author: Matthew Moy
# Date: 2024-10-21

library(tidyverse)
library(ggplot2)
library(dplyr)
library(shiny)
library(quantmod)

getSymbols("AAPL", src = "yahoo", from = "2023-01-01", to = "2024-12-31", auto.assign = FALSE) -> stock_data
stock_data <- fortify.zoo(stock_data)
colnames(stock_data) <- c("Date", "Open", "High", "Low", "Close", "Volume", "Adjusted")

stock_data <- stock_data %>%
  mutate(Year = format(Date, "%Y"),
         Month = format(Date, "%m"),
         Day = as.numeric(format(Date, "%d")))

ui <- fluidPage(
  titlePanel("AAPL Monthly Candlestick Chart"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("year", "Select Year:", choices = c(2023, 2024), selected = 2024),
      selectInput("month", "Select Month:", 
                  choices = month.name, selected = "October")
    ),
    
    mainPanel(
      plotOutput("candlestickPlot")
    )
  )
)

server <- function(input, output, session) {
  
  monthly_data <- reactive({
    month_num <- match(input$month, month.name)
    
    stock_data %>%
      filter(Year == input$year, Month == sprintf("%02d", month_num))
  })
  
  output$candlestickPlot <- renderPlot({
    data <- monthly_data()
    
    ggplot(data, aes(x = Day)) +
      geom_segment(aes(x = Day, xend = Day, y = Low, yend = High), color = "black") +  # Wicks
      geom_rect(aes(xmin = Day - 0.3, xmax = Day + 0.3, ymin = pmin(Open, Close), ymax = pmax(Open, Close),
                    fill = Close > Open)) +  # Body
      scale_fill_manual(values = c("TRUE" = "green", "FALSE" = "red"), guide = "none") +
      labs(title = paste("AAPL Candlestick Chart for", input$month, input$year),
           x = "Day of Month", y = "Price") +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10)
      )
  })
}

shinyApp(ui = ui, server = server)

