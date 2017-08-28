library(shiny)
library(Rperform)

# Define server logic required to plot the data
shinyServer(function(input, output) {
   
  output$Plot_metric <- renderPlot({
    
  
    
    
    
    
      # generate the plot on input$bins from ui.R
   # plot the test with the specified number of commits
  #  hist(x, breaks = bins, col = 'darkgray', border = 'white')
    
  })
})
