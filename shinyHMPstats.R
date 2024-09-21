#Section 1: Loading in the Data File and R libraries. 

#Reading in the libraries

library("readxl")
library("dplyr")
library("tidyverse")
library("writexl")
library(ggplot2)
library(lubridate)
library(tidyquant)
library(stringr)
library(tidyr)
library(anytime)
library(shiny)
library(DT)

#Reading in the data file

data <- read_excel("C:/Users/amrigupt/Downloads/fourmonths2024ChangeEvents.xlsx")

data

# Convert EVENT_OCCUR_DATE to date format and from there you can get the month
data$EVENT_OCCUR_DATE <- as.POSIXct(data$EVENT_OCCUR_DATE, format="%m/%d/%Y %H:%M")
data$Month <- month(data$EVENT_OCCUR_DATE, label = TRUE) # Extracting month and using labels (Jan, Feb, etc.)

# Function to count EVENT_NAME occurrences for each of the categories in CATEGORY_NAME
count_events_by_category <- function(data, category_filter = NULL) {
    if (!is.null(category_filter)) {
        data <- subset(data, CATEGORY_NAME == category_filter)
    }
    
    event_counts <- data %>%
        group_by(CATEGORY_NAME, EVENT_NAME) %>%
        summarise(Event_Count = n(), .groups = 'drop')
    
    return(event_counts)
}

# Function to count number of events by month
count_events_by_month <- function(data, month_filter = NULL) {
    if (!is.null(month_filter)) {
        data <- subset(data, Month == month_filter)
    }
    
    event_counts <- data %>%
        group_by(Month, CATEGORY_NAME, EVENT_NAME) %>%
        summarise(Event_Count = n(), .groups = 'drop')
    
    return(event_counts)
}

# UI for the Shiny app
ui <- fluidPage(
    titlePanel("HMP Last Four Months Usage Statistics"),
    
    tabsetPanel(
        # Tab 1: Filter by CATEGORY_NAME and show counts of EVENT_NAME
        tabPanel("Filter by Category",
                 sidebarLayout(
                     sidebarPanel(
                         selectInput("category", "Select Category", 
                                     choices = unique(data$CATEGORY_NAME), 
                                     selected = unique(data$CATEGORY_NAME)[1],
                                     multiple = FALSE) # Single category selection
                     ),
                     
                     mainPanel(
                         DTOutput("eventTableCategory")
                     )
                 )
        ),
        
        # Tab 2: Filter by Month and show statistics for all categories
        tabPanel("Filter by Month",
                 sidebarLayout(
                     sidebarPanel(
                         selectInput("month", "Select Month", 
                                     choices = unique(data$Month), 
                                     selected = unique(data$Month)[1])
                     ),
                     
                     mainPanel(
                         DTOutput("eventTableMonth")
                     )
                 )
        )
    )
)

# Server logic for the Shiny app
server <- function(input, output) {
    # Tab 1: Filter by CATEGORY_NAME
    output$eventTableCategory <- renderDT({
        event_data_category <- count_events_by_category(data, category_filter = input$category)
        datatable(event_data_category, options = list(pageLength = 10))
    })
    
    # Tab 2: Filter by Month
    output$eventTableMonth <- renderDT({
        event_data_month <- count_events_by_month(data, month_filter = input$month)
        datatable(event_data_month, options = list(pageLength = 10))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)