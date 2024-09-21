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

# Load necessary libraries
library(shiny)
library(dplyr)
library(lubridate)
library(DT)
library(ggplot2)  



# Convert EVENT_OCCUR_DATE to date format and extract the month
data$EVENT_OCCUR_DATE <- as.POSIXct(data$EVENT_OCCUR_DATE, format="%m/%d/%Y %H:%M")
data$Month <- month(data$EVENT_OCCUR_DATE, label = TRUE) # Extracting month and using labels (Jan, Feb, etc.)

# Function to count EVENT_NAME occurrences for each CATEGORY_NAME
count_events_by_category <- function(data, category_filter = NULL) {
    if (!is.null(category_filter)) {
        data <- subset(data, CATEGORY_NAME == category_filter)
    }
    
    event_counts <- data %>%
        group_by(CATEGORY_NAME, EVENT_NAME) %>%
        summarise(Event_Count = n(), .groups = 'drop')
    
    return(event_counts)
}

# Function to count events by month
count_events_by_month <- function(data, month_filter = NULL) {
    if (!is.null(month_filter)) {
        data <- subset(data, Month == month_filter)
    }
    
    event_counts <- data %>%
        group_by(Month, CATEGORY_NAME, EVENT_NAME) %>%
        summarise(Event_Count = n(), .groups = 'drop')
    
    return(event_counts)
}

# Function to create a summary of events by month for graph
count_events_for_graph <- function(data, category_filter = NULL) {
    if (!is.null(category_filter)) {
        data <- subset(data, CATEGORY_NAME == category_filter)
    }
    
    event_counts <- data %>%
        group_by(Month, CATEGORY_NAME) %>%
        summarise(Event_Count = n(), .groups = 'drop')
    
    return(event_counts)
}

# Function to get CREATED_BY, UPDATED_BY, and EVENT_OCCUR_DATE by category and month, sorted in reverse chronological order
get_created_updated_by <- function(data, category_filter = NULL, month_filter = NULL) {
    if (!is.null(category_filter)) {
        data <- subset(data, CATEGORY_NAME == category_filter)
    }
    if (!is.null(month_filter)) {
        data <- subset(data, Month == month_filter)
    }
    
    # Sort the data by EVENT_OCCUR_DATE in reverse chronological order
    data <- data %>% arrange(desc(EVENT_OCCUR_DATE))
    
    return(data %>% select(CATEGORY_NAME, EVENT_NAME, CREATED_BY, UPDATED_BY, EVENT_OCCUR_DATE, Month))
}

# Function to get top 5 CREATED_BY users, excluding NA and "system" values, and filtered by selected month
get_top_5_created_by <- function(data, month_filter = NULL) {
    top_created <- data %>%
        filter(!is.na(CREATED_BY) & CREATED_BY != "system") %>%  # Exclude NA and "system"
        filter(is.null(month_filter) | Month == month_filter) %>%  # Filter by month if selected
        group_by(CREATED_BY) %>%
        summarise(Event_Count = n()) %>%
        arrange(desc(Event_Count)) %>%
        top_n(5, wt = Event_Count)
    
    return(top_created)
}

# Function to get top 5 UPDATED_BY users, excluding NA and "system" values, and filtered by selected month
get_top_5_updated_by <- function(data, month_filter = NULL) {
    top_updated <- data %>%
        filter(!is.na(UPDATED_BY) & UPDATED_BY != "system") %>%  # Exclude NA and "system"
        filter(is.null(month_filter) | Month == month_filter) %>%  # Filter by month if selected
        group_by(UPDATED_BY) %>%
        summarise(Event_Count = n()) %>%
        arrange(desc(Event_Count)) %>%
        top_n(5, wt = Event_Count)
    
    return(top_updated)
}

# UI for the Shiny app
ui <- fluidPage(
    titlePanel("Event Dashboard"),
    
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
                         h3("Statistics"),
                         textOutput("categoryStats"),
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
                         h3("Statistics"),
                         textOutput("monthStats"),
                         DTOutput("eventTableMonth")
                     )
                 )
        ),
        
        # Tab 3: Graph of events per month for each category with a category filter
        tabPanel("Monthly Event Graph",
                 sidebarLayout(
                     sidebarPanel(
                         selectInput("graphCategory", "Select Category for Graph", 
                                     choices = c("All", unique(data$CATEGORY_NAME)), 
                                     selected = "All") # Category selection for the graph
                     ),
                     
                     mainPanel(
                         h3("Statistics"),
                         textOutput("graphStats"),
                         plotOutput("eventGraph")
                     )
                 )
        ),
        
        # Tab 4: Filter by category and month to display CREATED_BY, UPDATED_BY, and EVENT_OCCUR_DATE, top 5 created_by and updated_by
        tabPanel("Created/Updated By",
                 sidebarLayout(
                     sidebarPanel(
                         selectInput("categoryCreatedUpdated", "Select Category", 
                                     choices = unique(data$CATEGORY_NAME), 
                                     selected = unique(data$CATEGORY_NAME)[1],
                                     multiple = FALSE), # Single category selection
                         selectInput("monthCreatedUpdated", "Select Month", 
                                     choices = unique(data$Month), 
                                     selected = unique(data$Month)[1]) # Single month selection
                     ),
                     
                     mainPanel(
                         h3("Statistics"),
                         textOutput("createdUpdatedStats"),
                         DTOutput("createdUpdatedTable"),
                         h3("Top 5 Created By"),
                         plotOutput("topCreatedByPlot"),
                         h3("Top 5 Updated By"),
                         plotOutput("topUpdatedByPlot")
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
    
    # Tab 1: Statistics for Filter by Category
    output$categoryStats <- renderText({
        filtered_data <- data %>% filter(CATEGORY_NAME == input$category)
        total_events <- nrow(filtered_data)
        unique_event_types <- n_distinct(filtered_data$EVENT_NAME)
        paste("Total Events:", total_events, "| Unique Event Types:", unique_event_types)
    })
    
    # Tab 2: Filter by Month
    output$eventTableMonth <- renderDT({
        event_data_month <- count_events_by_month(data, month_filter = input$month)
        datatable(event_data_month, options = list(pageLength = 10))
    })
    
    # Tab 2: Statistics for Filter by Month
    output$monthStats <- renderText({
        filtered_data <- data %>% filter(Month == input$month)
        total_events <- nrow(filtered_data)
        most_common_category <- filtered_data %>% 
            count(CATEGORY_NAME, sort = TRUE) %>% 
            slice(1) %>% 
            pull(CATEGORY_NAME)
        paste("Total Events:", total_events, "| Most Common Category:", most_common_category)
    })
    
    # Tab 3: Graph of events by month for each category with a category filter
    output$eventGraph <- renderPlot({
        if (input$graphCategory == "All") {
            event_data_graph <- count_events_for_graph(data)
        } else {
            event_data_graph <- count_events_for_graph(data, category_filter = input$graphCategory)
        }
        
        ggplot(event_data_graph, aes(x = Month, y = Event_Count, fill = CATEGORY_NAME)) +
            geom_bar(stat = "identity", position = "dodge") +
            labs(title = "Number of Events per Month by Category", x = "Month", y = "Number of Events") +
            theme_minimal()
    })
    
    # Tab 3: Statistics for Monthly Event Graph
    output$graphStats <- renderText({
        if (input$graphCategory == "All") {
            filtered_data <- data
        } else {
            filtered_data <- data %>% filter(CATEGORY_NAME == input$graphCategory)
        }
        total_events <- nrow(filtered_data)
        most_common_category <- filtered_data %>% 
            count(CATEGORY_NAME, sort = TRUE) %>% 
            slice(1) %>% 
            pull(CATEGORY_NAME)
        paste("Total Events:", total_events, "| Most Common Category:", most_common_category)
    })
    
    # Tab 4: Filter by CATEGORY_NAME and Month, show CREATED_BY, UPDATED_BY, and EVENT_OCCUR_DATE in reverse chronological order
    output$createdUpdatedTable <- renderDT({
        created_updated_data <- get_created_updated_by(data, category_filter = input$categoryCreatedUpdated, month_filter = input$monthCreatedUpdated)
        datatable(created_updated_data, options = list(pageLength = 10))
    })
    
    # Tab 4: Statistics for Created/Updated By
    output$createdUpdatedStats <- renderText({
        filtered_data <- data %>% filter(Month == input$monthCreatedUpdated)
        total_events <- nrow(filtered_data)
        top_creator <- filtered_data %>% 
            filter(!is.na(CREATED_BY) & CREATED_BY != "system") %>% 
            count(CREATED_BY, sort = TRUE) %>% 
            slice(1) %>% 
            pull(CREATED_BY)
        top_updater <- filtered_data %>% 
            filter(!is.na(UPDATED_BY) & UPDATED_BY != "system") %>% 
            count(UPDATED_BY, sort = TRUE) %>% 
            slice(1) %>% 
            pull(UPDATED_BY)
        paste("Total Events:", total_events, "| Top Creator:", top_creator, "| Top Updater:", top_updater)
    })
    
    # Top 5 Created By Plot
    output$topCreatedByPlot <- renderPlot({
        top_created_by <- get_top_5_created_by(data, month_filter = input$monthCreatedUpdated)
        ggplot(top_created_by, aes(x = reorder(CREATED_BY, Event_Count), y = Event_Count)) +
            geom_bar(stat = "identity", fill = "blue") +
            labs(title = "Top 5 Created By", x = "Created By", y = "Event Count") +
            coord_flip() +
            theme_minimal()
    })
    
    # Top 5 Updated By Plot
    output$topUpdatedByPlot <- renderPlot({
        top_updated_by <- get_top_5_updated_by(data, month_filter = input$monthCreatedUpdated)
        ggplot(top_updated_by, aes(x = reorder(UPDATED_BY, Event_Count), y = Event_Count)) +
            geom_bar(stat = "identity", fill = "green") +
            labs(title = "Top 5 Updated By", x = "Updated By", y = "Event Count") +
            coord_flip() +
            theme_minimal()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)



