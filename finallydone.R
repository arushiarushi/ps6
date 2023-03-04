library(shiny)
library(tidyverse)

data <- read_delim("Fat_Supply_Quantity_Data.csv")
chosenSet <- data %>% 
select(Country, Vegetables, Confirmed, Deaths, Recovered)

ui <- fluidPage(
  navbarPage("My Application",
            tabPanel("Home", 
                     h1("Welcome to", 
                        strong("My Healthy COVID"),
                        " Data Set!"),
                     p("This application shows", 
                       em("covid recovery and confirmed rates for vegetarians"),
                       "."),
                     p("The data set shows", nrow(chosenSet), "rows of data. The plot display page will show a plot that presents potential correlation between vegetable product consumption and recovered COVID
                        cases. The table display page will present a table that shows vegetable product consumption and recovery based on country ."),
                     dataTableOutput("datasample")
            ),
            tabPanel("Plot Display",
                     sidebarLayout(
                       sidebarPanel(
                         sliderInput("n","Number of Confirmed Points:",
                                     min = 0,
                                     max = 100,
                                     value = 10),
                         radioButtons("colors", label = h3("Pick your color scheme:"),
                                      choices= list("Red" = "red", "Orange" = "orange")
                                      
                        ),
                        
                      ),
                      
                       mainPanel(
                         plotOutput("plot"),
                         textOutput("plotSummary")
                       )
                     )
            ),
            tabPanel("Table Display",
                     sidebarPanel(
                       # Dropdown menu for choosing country
                       selectInput(inputId = "Country",
                                   label = "Select country:",
                                   choices = unique(data$Country),
                       ),
                       
                       
                     ),
                     mainPanel(
                       tableOutput("table"),
                       textOutput("summary"),
                     )
                     
            )
  )
)

server <- function(input, output) {
  
output$datasample <- renderDataTable ({
  chosenSet %>% 
    sample_n(6)
})
  
  
  output$plot <- renderPlot({
    data %>% 
      filter(!is.na(Vegetables)) %>% 
      sample_n(input$n) %>% 
      ggplot(aes(Vegetables, Recovered, size = Recovered, color = Recovered)) + 
      geom_point()+
      labs(title = "COVID Recovered Cases vs. Vegetables Consumption")+
      scale_color_gradient(low = "pink", high = input$colors)

  })
  
  
  filteredData <- reactive({
    data %>%
      filter(Country == input$Country) %>%
      select(Country, Vegetables, Recovered)
  }) 
  
  
  output$table <- renderTable({
    filteredData()
    
  })
  
 output$summary <-renderText({
   if (!is.null(input$Country)) {
     paste("You selected", input$Country, "with a recovery rate of",round(filteredData()$Recovered*100, 2), "%.")
   } 
    
  })
  
 output$plotSummary <- renderText({
   paste("You selected", input$n, "values.")
   
 })
   

}

shinyApp(ui = ui, server = server)
