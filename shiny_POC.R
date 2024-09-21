library(shiny)
library(dplyr)
library(ggplot2)

data <- read_excel("C:/Users/amrigupt/Downloads/chatgpt_time_series_test.xlsx")
head(data)


# Define UI for the Shiny app
ui <- fluidPage(
    titlePanel("Events Dashboard"),
    
    sidebarLayout(
        sidebarPanel(
            selectInput("category", "Choose a category:", choices = unique(data$CATEGORY_NAME), selected = unique(data$CATEGORY_NAME)[1]),
            selectInput("year", "Choose a year:", choices = unique(data$Year), selected = unique(data$Year)[1])
        ),
        
        mainPanel(
            plotOutput("eventPlot"),
            tableOutput("eventTable")
        )
    )
)

# Define server logic
server <- function(input, output) {
    filtered_data <- reactive({
        data %>%
            filter(CATEGORY_NAME == input$category & Year == input$year)
    })
    
    output$eventPlot <- renderPlot({
        ggplot(filtered_data(), aes(x = Month)) +
            geom_bar() +
            theme_minimal() +
            labs(title = paste("Number of Events per Month in", input$year, "for", input$category),
                 x = "Month", y = "Number of Events")
    })
    
    output$eventTable <- renderTable({
        filtered_data() %>%
            group_by(Month) %>%
            summarise(Events = n()) %>%
            arrange(Month)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

shiny::stopApp()