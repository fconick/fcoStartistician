#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

Map_draw <- readRDS('../Map_draw.RDS')
Languages <- paste0('`',colnames(Map_draw)[-c(1:16)],'`') 

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  title = "Toronto language distribution ",
  h1("Toronto language distribution divided by Wards"),
  
  
  
  # Sidebar with a slider input for number of bins 
  fluidRow(column(2, 
                  
      checkboxInput("Map", label = "Language Map", value = TRUE),
     
       selectInput("variable", "Select a Language:",
                  Languages, 
                  selected = '`English`'),
    
    
   radioButtons("color", "Choose a color:",
                c("red", 'green', 'blue', 'magenta', 'orange', 'transparent')),
   
   checkboxInput("Bike", label = "Plot bike rings", value = FALSE),
   checkboxInput("Cameras", label = "Plot traffic cameras", value = FALSE)
   
  ),
    # Show a plot of the generated distribution
    column(9,
      
      # Application title
     # titlePanel("Toronto Map divided by Wards"),
       plotOutput('Plot' )
    )
  )
))
