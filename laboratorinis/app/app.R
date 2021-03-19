library(shiny)
library(shinydashboard)
library(tidyverse)
library(readr)
ui <- dashboardPage(skin = "red",
                    dashboardHeader(title = "702200 Konsultacine verslo ir kito valdymo veikla"),
                    dashboardSidebar(selectizeInput(inputId = "imones_pavadinimas", label="Imones pavadinimas",
                                                    choices= NULL, selected= NULL)),
dashboardBody(tabsetPanel(
  tabPanel("Vidutinio atlyginimo grafikas", plotOutput("plot1")),
  tabPanel("Duomenu lentele", tableOutput("table")),
  tabPanel("Apdraustu darbuotoju grafikas", plotOutput("plot2")),
  tabPanel("Mokesciu grafikas", plotOutput("plot3"))
    )
  )
)

server <- function(input, output, session){
  data <- read_csv("../data/lab_sodra.csv")
  data1 <- data %>%
    filter(ecoActCode == 702200) %>%
    mutate(month_value=as.integer(substr(month, 5 ,7)))
  
  updateSelectizeInput(session, "imones_pavadinimas", 
                       choices = data1$name, 
                       server = TRUE)
  
  output$table <- renderTable(
    data1 %>%
      filter(name == input$imones_pavadinimas), digits = 0
  )
  
  output$plot1 <- renderPlot(
    data1 %>%
      filter(name == input$imones_pavadinimas) %>%
      ggplot(aes(x = month_value, y = avgWage)) +
      scale_x_continuous("month",breaks=1:12,limits=c(1,12)) +
      theme_minimal() +
      theme(axis.text.x = element_blank()) +
      geom_point() +
      geom_line(colour = 'red') +
      labs(x = "Month", y = "Euros")
  )
  
    output$plot2 <- renderPlot(
      data1 %>%
        filter(name == input$imones_pavadinimas) %>%
        ggplot(aes(x = month_value, y = numInsured)) +
        scale_x_continuous("month",breaks=1:12,limits=c(1,12)) +
        theme_minimal() +
        geom_point() +
        geom_line(colour = 'red') +
        labs(x = "Month", y = "Count")
  )
    
    output$plot3 <- renderPlot(
      data1 %>%
        filter(name == input$imones_pavadinimas) %>%
        ggplot(aes(x = month_value, y = tax)) +
        scale_x_continuous("month",breaks=1:12,limits=c(1,12)) +
        theme_minimal() +
        geom_point() +
        geom_line(colour = "red") +
        labs(x = "Month", y = "Euros")
    )
}
shinyApp(ui, server)