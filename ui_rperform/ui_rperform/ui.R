library(shiny)
library(stringr)
library(Rperform)
library(dygraphs)

shinyUI(fluidPage
          (
                titlePanel(("Rperform: Performance analysis and visualization for R")),
  
  
                  sidebarLayout(
                    
                    sidebarPanel(
                          selectInput("Performance metrics", "Choose a metric:", 
                          choices = c("time", "memory", "testmetric")),
                          selectInput("Branch", "Choose type of branch:", 
                                      choices = c("within a branch", "With two branches")),
                          selectInput("Operating system (OS)", "Select the type of your OS",
                                      choices = c("Linux", "Windows", "Mac")),
  
                          numericInput("commits", "Number of commits to view:", 10)
                          
                          ,
                    actionButton("update", "Submit")),
                    
                  mainPanel(
                    #plotouput is the fucntion used to plot the output for a given metric. plot_metrics is name of the object given 
                    #save the output
                          plotOutput("Plot_metrics"))
                          
                              
                            )
              )

        )
