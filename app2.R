# Load libraries
library(ggplot2)
library(dplyr)
library(lubridate)
library(readr)
library(shiny)
library(plotly)


# Read data
data = read_csv("https://uwmadison.box.com/shared/static/81h2znsto477hgtn99nycawhsy626bae.csv")


# Helper functions for filtering and plotting

# Update possible industries and companies when sector drop down is updated
updateSelections = function(selected_sectors, data, session, input) {
  industries = unique(data %>% filter(Sector %in% selected_sectors) %>% pull(Industry))
  updateSelectInput(session, "Industry", choices = sort(industries), selected = intersect(input$Industry, industries))
  companies = unique(data %>% filter(Sector %in% selected_sectors) %>% pull(Name))
  updateSelectInput(session, "Company", choices = sort(companies), selected = intersect(input$Company, companies))
}

# Update the sector and companies when industry drop down is updated
filterCompaniesByIndustry = function(selected_industries, data, session, input) {
  companies = unique(data %>% filter(Industry %in% selected_industries) %>% pull(Name))
  updateSelectInput(session, "Company", choices = sort(companies), selected = intersect(input$Company, companies))
  sectors = unique(data %>% filter(Industry %in% selected_industries) %>% pull(Sector))
  updateSelectInput(session, "Sector", choices = sort(sectors), selected = intersect(input$Sector, sectors))
}


# Before a company is selected, utilize the empty plot space to explain the app to the user
createEmptyPlot = function(data) {
  plot_ly() %>%
    layout(
      title = list(
        text = paste(
          "<span style='font-size: 18px;'>Welcome to the Investing Explorer!\n\n</span><br>",
          "<span style='font-size: 14px;'>Select one or more companies to start exploring.\n",
          "Filter companies by sector and industry to refine your options.\n",
          "Explore various price metrics and set a date range to analyze trends.\n",
          "View actual stock prices or the percentage change over time.\n",
          "Data covers ", 
          format(min(data$Date, na.rm = TRUE), "%B %d, %Y"), 
          " to ", 
          format(max(data$Date, na.rm = TRUE), "%B %d, %Y"),
          ".\n\n", 
          "Tip: Use the reset button anytime to clear your selections.</span>", sep = ""
        )
      ),
      xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
      yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
    )
}


# Create an interactive graph of stock prices based on companies, date range, and the selected price metric
generatePrices = function(filtered_data, min_date, max_date, metric) {
  plot_data = filtered_data %>%
    
    # change output format of text when hovering a line
    mutate(hover_text = case_when(
      metric == "Adjusted" ~ paste0("Name: ", Name, "<br>Date: ", format(Date, "%B %d, %Y"), "<br>Adjusted: $", format(round(Adjusted, 2), nsmall = 2)),
      metric %in% c("Open", "Close") ~ paste0("Name: ", Name, "<br>Date: ", format(Date, "%B %d, %Y"), "<br>Open: $", format(round(Open, 2), nsmall = 2), "<br>Close: $", format(round(Close, 2), nsmall = 2), "<br>Adjusted: $", format(round(Adjusted, 2), nsmall = 2)),
      metric %in% c("High", "Low") ~ paste0("Name: ", Name, "<br>Date: ", format(Date, "%B %d, %Y"), "<br>Low: $", format(round(Low, 2), nsmall = 2), "<br>High: $", format(round(High, 2), nsmall = 2), "<br>Adjusted: $", format(round(Adjusted, 2), nsmall = 2))
    ))
  
  # use plotly to show line graph with hover and company coloring
  plot_ly(data = plot_data, x = ~Date, 
          y = plot_data[[metric]], color = ~Name, 
          type = 'scatter', mode = 'lines',
          text = ~hover_text, 
          hoverinfo = 'text',
          key = ~Name) %>%
    layout(
      title = paste(metric, "Share Price from", format(min_date, "%B %d, %Y")),
      xaxis = list(title = "Date", range = c(min_date, max_date)),
      yaxis = list(title = paste(metric, "Price ($)"))
    )
}

# same plot as above, but instead of prices it is the percent change since the minimum date
generatePctChange = function(filtered_data, min_date, max_date, metric) {
  plot_data = filtered_data %>%
    group_by(Name) %>%
    mutate(Begin_Val = get(metric)[1],
           Pct_Change = (get(metric) - Begin_Val) / Begin_Val * 100) %>%
    ungroup()
  
  plot_ly(data = plot_data, x = ~Date, 
          y = ~Pct_Change, color = ~Name, 
          type = 'scatter', mode = 'lines', 
          text = ~paste("Name:", Name, "<br>Date:", format(Date, "%B %d, %Y"), "<br>Percent Change:", round(Pct_Change, 2), "%"),
          hoverinfo = 'text', key = ~Name) %>%
    layout(
      title = paste(metric, "Percentage Change from", format(min_date, "%B %d, %Y")),
      xaxis = list(title = "Date", range = c(min_date, max_date)),
      yaxis = list(title = paste(metric, "Percentage Change (%)"))
    )
}


# add click feature that gives company summary
createCompanySummary = function(event, input, data) {
  if (is.null(event) || length(input$Company) == 0) {
    return(NULL)
  }
  
  clicked_company = event$key  # Get the clicked company name
  # Check if the clicked company exists in the data
  company_info = data %>%
    filter(Name == clicked_company) %>%
    select(Name, Symbol, Sector, Industry, Headquarters, Founded, Date_added) %>%
    unique()

  if (nrow(company_info) == 0) {
    return(NULL)  # No company info found
  }
  
  date_added_formatted = format(as.Date(company_info$Date_added), "%B %d, %Y")
  
  tagList(
    h4(paste("Summary for:", company_info$Name)),
    p(paste("Symbol:", company_info$Symbol)),
    p(paste("Sector:", company_info$Sector)),
    p(paste("Industry:", company_info$Industry)),
    p(paste("Headquarters:", company_info$Headquarters)),
    p(paste("Founded:", company_info$Founded)),
    p(paste("Date Added to S&P 500:", date_added_formatted))
  )
}


# add reset button, to set all filters back to the starting point
resetFilters = function(session, data) {
  updateSelectInput(session, "Sector", selected = NULL)
  updateSelectInput(session, "Industry", selected = NULL)
  updateSelectInput(session, "Company", selected = NULL)
  updateSelectInput(session, "Crypto", selected = NULL)
  updateSelectInput(session, "Index", selected = NULL)
  updateSelectInput(session, "ETF", selected = NULL)
  updateSelectInput(session, "Mutual", selected = NULL)
  updateSelectInput(session, "Metric", selected = "Adjusted")
  
  updateDateRangeInput(session, "dateRange", 
                       start = min(data$Date, na.rm = TRUE),
                       end = max(data$Date, na.rm = TRUE))
  
  updateSelectInput(session, "Industry", choices = data %>% filter(Type == 'Stock') %>% distinct(Industry) %>% arrange(Industry) %>% pull(Industry))
  updateSelectInput(session, "Company", choices = data %>% filter(Type == 'Stock') %>% distinct(Name) %>% arrange(Name) %>% pull(Name))
  updateSelectInput(session, "Sector", choices = data %>% filter(Type == 'Stock') %>% distinct(Sector) %>% arrange(Sector) %>% pull(Sector))
  updateSelectInput(session, "Crypto", choices = data %>% filter(Type == 'Crypto') %>% distinct(Name) %>% arrange(Name) %>% pull(Name))
  updateSelectInput(session, "Index", choices = data %>% filter(Type == 'Index') %>% distinct(Name) %>% arrange(Name) %>% pull(Name))
  updateSelectInput(session, "ETF", choices  = data %>% filter(Type == 'ETF') %>% distinct(Name) %>% arrange(Name) %>% pull(Name))
  updateSelectInput(session, "Mutual", choices = data %>% filter(Type == 'Mutual Fund') %>% distinct(Name) %>% arrange(Name) %>% pull(Name))
}




# Define UI
ui = fluidPage(
  titlePanel("Investing Explorer"),
  
  # have filters in the side layout
  sidebarLayout(
    sidebarPanel(
      selectInput("Company", "Select Company", choices = data %>% filter(Type == 'Stock') %>% distinct(Name) %>% arrange(Name) %>% pull(Name), multiple = TRUE),
      selectInput("Sector", "Select Sector", choices = data %>% filter(Type == 'Stock') %>% distinct(Sector) %>% arrange(Sector) %>% pull(Sector), multiple = TRUE),
      selectInput("Industry", "Select Industry", choices = data %>% filter(Type == 'Stock') %>% distinct(Industry) %>% arrange(Industry) %>% pull(Industry), multiple = TRUE),
      selectInput("Crypto", 'Select Cryptocurrency', choices = data %>% filter(Type == 'Crypto') %>% distinct(Name) %>% arrange(Name) %>% pull(Name), multiple = TRUE),
      selectInput("Index", 'Select Index Fund', choices = data %>% filter(Type == 'Index') %>% distinct(Name) %>% arrange(Name) %>% pull(Name), multiple = TRUE),
      selectInput("ETF", 'Select ETF', choices = data %>% filter(Type == 'ETF') %>% distinct(Name) %>% arrange(Name) %>% pull(Name), multiple = TRUE),
      selectInput("Mutual", 'Select Mutual Fund', choices = data %>% filter(Type == 'Mutual Fund') %>% distinct(Name) %>% arrange(Name) %>% pull(Name), multiple = TRUE),
      dateRangeInput("dateRange", "Select Date Range", start = min(data$Date), end = max(data$Date),
                     min = min(data$Date), max = max(data$Date), 
                     startview = 'decade', format = "mm/dd/yyyy"),
      radioButtons("plotType", "Select Plot Type", 
                   choices = c("Price" = "price", "Percentage Change" = "pct_change"),
                   selected = "price"),
      selectInput("Metric", "Price Metric", 
                  choices = c("Open", "Close", "Low", "High", "Adjusted"), 
                  selected = 'Adjusted'),
      actionButton("reset", "Reset Filters")
    ),
    
    # have graphs in the main layout
    mainPanel(
      plotlyOutput("plot"),
      p("Click on a company's line in the chart to get more information about that company."),
      uiOutput("companySummary")
    )
  )
)




# Define server logic by utilizing the helper functions
server = function(input, output, session) {
  
  observeEvent(input$Sector, {
    req(input$Sector)
    updateSelections(input$Sector, data, session, input)
  })
  
  observeEvent(input$Industry, {
    req(input$Industry)
    filterCompaniesByIndustry(input$Industry, data, session, input)
  })
  
  # Choose when to output each plot based on company selection 
  output$plot = renderPlotly({
    if (length(input$Company) + 
        length(input$Crypto) + 
        length(input$Index) + 
        length(input$ETF) + 
        length(input$Mutual) == 0) {
      return(createEmptyPlot(data))
    }

    min_date = input$dateRange[1]
    max_date = input$dateRange[2]
    
    plot_data = data %>%
      filter((Name %in% input$Company) | (Name %in% input$Crypto) | (Name %in% input$Index) | (Name %in% input$ETF) | (Name %in% input$Mutual), Date >= min_date, Date <= max_date)
    
    if (input$plotType == "pct_change") {
      return(generatePctChange(plot_data, min_date, max_date, input$Metric))
    } else {
      return(generatePrices(plot_data, min_date, max_date, input$Metric))
    }
  })
  
  # Create a reactive expression for the company summary
  event_data_reactive = reactive({
    event_data("plotly_click")
  })
  
  output$companySummary = renderUI({
    event = event_data_reactive()
    createCompanySummary(event, input, data)
  })
  
  observeEvent(input$reset, {
    resetFilters(session, data)
  })
}





# Run the app
shinyApp(ui = ui, server = server)