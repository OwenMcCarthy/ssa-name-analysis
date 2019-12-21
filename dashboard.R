library(shiny)
library(plotly)
source('~/Names/Code/functions.R')

ui <- fluidPage(
  
  headerPanel("Names!"),
  sidebarPanel(
    selectInput('Name', 'Name', choices = possibles, selected = "Name"),
    
  
  ),
  mainPanel(
    plotlyOutput('namePlot', height = "900px")
  )
)

server <- function(input, output) {

  #add reactive data information. Dataset = built in diamonds data
  
  output$namePlot <- renderPlotly({
    
    name.graph(names=input$Name,1990) %>% 
      layout(height = input$plotHeight, autosize=TRUE)
    
  })
  
}

shinyApp(ui, server)