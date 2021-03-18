library(shiny)
library(shinydashboard)
library(tidyverse)
library(readr)
ui <- dashboardPage(skin = "red",
                    dashboardHeader(title = "Laboratorinis"),
                    dashboardSidebar(selectizeInput(inputId = "imones_kodas", label="Imones kodas",
                                                    choices= NULL, selected= NULL)),
dashboardBody(tabsetPanel(
  tabPanel("grafikas", plotOutput("plot")),
  tabPanel("lentele", tableOutput("table"))
    )
  )
)

server <- function(input, output, session){
  data <- read_csv("../data/lab_sodra.csv")
  data1 <- data %>%
    filter(ecoActCode == 702200)
  updateSelectizeInput(session, "imones_kodas", 
                       choices = data1$name, 
                       server = TRUE)
  output$table <- renderTable(
    data1 %>%
      filter(name == input$imones_kodas), digits = 0
  )
  
  output$plot <- renderPlot(
    data1 %>%
      filter(name == input$imones_kodas) %>%
      ggplot(aes(x = month, y = avgWage)) +
      theme_minimal() +
      theme(axis.text.x = element_blank()) +
      geom_point() +
      geom_line(colour = 'red')
  )
}
shinyApp(ui, server)