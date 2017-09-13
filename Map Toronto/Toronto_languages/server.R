#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(ggmap)

Toronto <- readRDS('../Toronto.RDS')
Map_draw <- readRDS('../Map_draw.RDS')
cnames <- readRDS('../cnames.RDS')
Bike_rings <- readRDS('../Bike_rings.RDS')
Traffic_cameras <- readRDS('../Traffic_cameras.RDS')

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  output$distPlot <- renderPlot({
    
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2] 
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
    
  })
  
  output$Plot <- renderPlot({
    
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
                              color = 'black')} +
      
    {if(input$Cameras) geom_point(data = Traffic_cameras, 
        aes(Longitude, Latitude),
        color = 'light blue')} +      
      
      {if(input$Map) geom_polygon(data = Map_draw,
                   aes_string('long', 'lat', group = 'group', 
                              fill = input$variable), 
                   colour = 'gray', alpha =.7)} + 
      geom_text(data =cnames, aes(long, lat, label = New_Name),
                size =2, check_overlap = TRUE) +
      coord_map() + 
      scale_fill_gradient(low = 'white', high = input$color) +
      

      
      
      xlim(c(-79.64,-79.11)) + ylim(c(43.57,43.86)) + 
      labs(fill= paste0(input$variable, ' (%)'), size = 'Post Bikes') +
      ditch_the_axes
    
  })
})
