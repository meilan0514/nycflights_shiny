library(shiny)
library(tidyverse)
library(nycflights13)

flights

ui <- fluidPage(
    titlePanel("nycflights13"),
    sidebarLayout(
        sidebarPanel(
            sliderInput(inputId = "dep_delay",
                        label = "Departure delay (minutes)",
                        min = 0,
                        max = max(flights$dep_delay, na.rm = TRUE),
                        value = c(0, max(flights$dep_delay, na.rm = TRUE))),
            sliderInput(inputId = "arr_delay",
                        label = "Arrival delay (minutes)",
                        min = 0,
                        max = max(flights$arr_delay, na.rm = TRUE),
                        value = c(0, max(flights$arr_delay, na.rm = TRUE))),
            radioButtons(inputId = "origin",
                         label = "Airport",
                         choices = c("JFK", "LGA", "EWR")),
            selectInput(inputId = "carrier",
                        label = "Carrier",
                        choices = sort(unique(flights$carrier)),
                        selected = "AA", 
                        multiple = TRUE)
        ),
        mainPanel(plotOutput("hourlyPlot"),
                  tableOutput("flights_Table "))
    )
)

server <- function(input, output) {
    flights_filter <- reactive({
        flights <- flights
        
        # filter by carrier
        if(!is.null(input$carrier)) {
            flights <- filter(flights, carrier %in% input$carrier)
        }
        
       
        # filter by origin
        flights <- filter(flights, origin %in% input$origin)
        
        # filter by dep_delay
        flights <- filter(flights,
                          dep_delay >= input$dep_delay[[1]],
                          dep_delay <= input$dep_delay[[2]])
        
        # filter by dep_delay
        flights <- filter(flights,
                          arr_delay >= input$arr_delay[[1]],
                          arr_delay <= input$arr_delay[[2]])
    })
    
    output$jobTitle <- renderUI({
        flights <- flights
        
        # filter by origin
        if(!is.null(input$origin)) {
            flights <- filter(flights, origin %in% input$origin)
        }
        
        # filter by carrier
        flights <- filter(flights, carrier %in% input$carrier)
        
        # filter by dep_delay
        flights <- filter(flights,
                            dep_delay >= input$dep_delay[[1]],
                            dep_delay <= input$dep_delay[[2]])
        
        flights <- filter(flights,
                          arr_delay >= input$arr_delay[[1]],
                          arr_delay <= input$arr_delay[[2]])
        
        selectInput(inputId = "origin",
                    label = "Airport",
                    choices = sort(unique(flights$origin)),
                    multiple = TRUE)
    })
    
    output$hourlyPlot <- renderPlot({
        flights_filter() %>%
            mutate(
                date = make_date(year, month, day),
                wday = wday(date, label = TRUE)
            ) %>%
            ggplot(aes(x = hour, color = wday, y = ..density..)) +
            geom_freqpoly(binwidth = 1)+
            labs(title = "Flight frequency")+
            theme(plot.title = element_text(hjust = 0.5))
    })
    
    output$flights_Table <- renderTable({
        flights_filter() %>%
            count(origin)
    })
}

shinyApp(ui = ui, server = server)


