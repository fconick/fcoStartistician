#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
library(rgeos)
library(shiny)
library(shinythemes)
library(maptools)
library(rgdal)
options(scipen =999)
gpclibPermit()

Map_draw <- readRDS('Map_draw.RDS')
Attributes <- readRDS('Attributes.RDS')


# Define UI for application that draws a histogram
shinyUI(fluidPage(
  theme = shinytheme("united"),
  
  # tags$style(type="text/css",
  #            ".shiny-output-error { visibility: hidden; }",
  #            ".shiny-output-error:before { visibility: hidden; }"
  # ),
  
  title = "Title",
  
  br(),
  br(),
  
  
  sidebarPanel(
    
      
      tabsetPanel(type = "tabs",
                  
                  tabPanel(div(img(src='myImage.jpg', height =30, width =30), 'Census Data'),
                           br(),
                           
h6('This app shows the data form the Statistics Canada, 2011 National
   Household Survey summarized by Wards.'),
                           

# radioButtons("radio_year_select","Year", 
#              c("2001" = "2001", "2011" = "2011"), inline=T),

                           selectInput("attrib", "Select a Category:",
                                       names(Attributes), 
                                       selected = '`English`'), 
                           
                           uiOutput("subAttribute"),

                          radioButtons("color", "Choose a color:",
                                        c("red",'blue', 'magenta', 'green'), inline = T)
                           
                          ),
                  
                  tabPanel("Traffic data",  
                           
                           br(),
                           
                           h6('If you want to return to the Census Data tab uncheck the boxes'),
                           
                           checkboxInput("Bike", 
                                         label = "Bike rings",
                                         value = FALSE),
                           
                           checkboxInput("Cameras", 
                                          label = "Traffic cameras",
                                          value = FALSE)
                           )
      )#,
      

      
      #radioButtons("color1", "Choose a color:", c("red", 'green'))
      
      
    
    
  ),
  

  
  mainPanel(
      plotOutput('Plot', height = '500px' )
      )
    
  
 
           
))
