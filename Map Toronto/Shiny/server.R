#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
library(rgeos)
library(shiny)
library(ggplot2)
library(ggmap)
library(magrittr)
library(tidyverse)
library(sp)
library(maptools)
library(rgdal)
gpclibPermit()
Toronto <- readRDS('Toronto.RDS')
Attributes <- readRDS('Attributes.RDS')
cnames <- readRDS('cnames.RDS')

Bike_rings <- readRDS('Bike_rings.RDS')
Traffic_cameras <- readRDS('Traffic_cameras.RDS')

Map <- readRDS('Map.RDS')
Data_organized <- readRDS('Data_organized_clean.RDS')
load(file = 'Join_Map_Data.rda')

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  
  # 2.- Reactive selection for TEAMS
  output$subAttribute <- renderUI({
    selectInput("variable", h4("Attribute:"), 
                choices = Attributes[[input$attrib]])
  })
 # name_main_attribute <- paste0('`', input$variable1 , '`')
  
  
  Map_draw_reactive <- reactive({
    
    return(Join_Map_Data(Map, Data_organized, input$attrib))
  
    })
 
  
  output$Plot <- renderPlot({
    
    
    Map_draw <- Map_draw_reactive()
    
    ditch_the_axes <- theme(
      axis.text = element_blank(),
      axis.line = element_blank(),
      axis.ticks = element_blank(),
      panel.border = element_blank(),
      panel.grid = element_blank(),
      axis.title = element_blank()
    )
    

    
    #Plot the map
    ggmap(Toronto, extent = 'normal') +
      
    {if(input$Bike)geom_point(data = Bike_rings, 
                              aes(lon, lat, size = posts),
                              color = 'black', alpha = .7 )} +
      
    {if(input$Cameras) geom_point(data = Traffic_cameras, 
        aes(Longitude, Latitude),
        color = 'blue', size =1.5)} +      
      
      {if(!input$Cameras & !input$Bike) geom_polygon(data = Map_draw,
                   aes_string('long', 'lat', group = 'group', 
                              fill = paste0('`',input$variable,'`') ), 
                   colour = 'white', alpha =.7)} + 
      geom_text(data =cnames, aes(long, lat, label = New_Name),
                size =3, fontface = "bold", check_overlap = F) +
      coord_map() + 
      scale_fill_gradient(low = '#E8F3F8',
                          high = paste0('dark ', input$color)) +
      

      
      
      xlim(c(-79.64,-79.11)) + ylim(c(43.57,43.86)) + 
      labs(fill= paste0(input$variable, ' (%)'), size = 'No. of Bike\nPosts') +
      ditch_the_axes + theme(legend.position = c(.9, .2),
                             legend.background = element_rect(
                               fill ="transparent"))
    
  })
})
