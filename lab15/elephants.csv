library(shiny)
library(shinydashboard)
library(ggplot2)

data <- read.csv("elephants.csv")

data$age <- as.numeric(data$age)
data$height <- as.numeric(data$height)

ui <- dashboardPage(
  dashboardHeader(title = "Elephant Data"),
  dashboardSidebar(
    selectInput("y", 
                "Select Variable",
                choices = c("age", "height"),
                selected = "age")
  ),
  dashboardBody(
    plotOutput("plot", width="500px", height="400px")
  )
)
server <- function(input, output, session) {
  
  output$plot <- renderPlot({
    
    elephants %>% 
      ggplot(aes(x = sex, 
                 y = .data[[input$y]], 
                 fill = sex)) +
      geom_boxplot(alpha = 0.7) +
      labs(
        title = paste("Range of", 
                      tools::toTitleCase(input$y), 
                      "by Sex"),
        x = "Sex",
        y = tools::toTitleCase(input$y)
      ) +
      theme_minimal(base_size = 14) +
      theme(legend.position = "none")
  })
}

shinyApp(ui, server)
