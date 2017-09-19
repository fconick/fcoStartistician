
library(rgdal)
library(readxl)
library(magrittr)
library(tidyverse)
library(tmap)
library(stringr)
library(ggmap)



#1.- Read Map data this has to be done only once

 #Map <- readOGR("Map Toronto/Ward 2","icitw_wgs84")
 # #Save the data as an R object
 # saveRDS(Map,'Map.RDS')

#1.- From now on this will be the way to load the map data 
Map <- readRDS('Map Toronto/Ward 2/Map.RDS')

#2.- Take a view of the data
 head(Map,1)

#3.-Deal with the statistical data
 DATA <- read_excel('Map Toronto/Ward Profiles - Census_2011.xlsx', 
                    range = 'A11:AX642')

 #Take a look at the rows
   head(DATA) 
   colnames(DATA)[1] <- 'Attributes'
   
   M.Data <- gather(DATA, Ward, value, -Attributes) %>% 
     filter(!is.na(value), !is.na(Attributes))
   M.Data$value %<>% as.numeric
 
  #Get the code name out of the Ward variable
   M.Data$SCODE_NAME <- str_extract(string = M.Data$Ward, '[0-9]+') %>% 
     as.numeric

#4.- Subset data to put in the map
   # Stats <- filter(M.Data,
   #                     Attributes =='Total population by age groups',
   #                   # Attributes == 'Tagalog (Pilipino, Filipino)',
   #                   # Attributes == 'Spanish',
   #                     !is.na(SCODE_NAME)) %>%
   #   select(-Ward, -Attributes)
   # 
   # colnames(Stats)[1] <- 'Variable'
   
#5.- Get the data ready to map it
   
   saveRDS(Map,'Map Toronto/Ward 2/MapInfo.RDS')
   Map <- readRDS('Map Toronto/Ward 2/MapInfo.RDS')
   #1.- From now on this will be the way to load the map data 
   Map <- readRDS('Map Toronto/Ward 2/Map.RDS')
   colnames(Map@data)[11] <- 'Population'
   
   #Join
   Map$SCODE_NAME %<>% as.character  %>% as.numeric
   Map@data %<>% left_join(Stats) 
   
   #Transform polygon data into a data.frame
   Map %<>% spTransform(CRS("+proj=longlat +datum=WGS84"))
   Map_draw <- fortify(Map, region ='SCODE_NAME')
   Map_draw$id %<>% as.numeric
   
   Data <- Map@data
    
   #Add the stats to the map data
   Map_draw %<>% left_join( Data, 
             by = c('id'='SCODE_NAME'))
   
   saveRDS(Map_draw, 'Map Toronto/Map_draw.RDS')
   
  #Aggregate data to get mean latitude and mean longitude for each state
   cnames <- aggregate(cbind(long, lat) ~ NAME, data=Map_draw,
                       FUN=function(x) mean(range(x)))
    
   cnames$New_Name <- gsub(pattern = " *\\(.*?\\) *",
                           x = cnames$NAME, replacement = '')
    
   cnames$New_Name <- gsub(pattern = "-",
                           x = cnames$New_Name, replacement = '\n')
   
   cnames$New_Name <- gsub(pattern = ' ',
                           x = cnames$New_Name, replacement = '\n')
   
   saveRDS(cnames, 'Map Toronto/cnames.RDS')
   
#6.- Plot the data
   
   #Download the map from goggle maps
   Toronto <- get_map(location = c(-79.43, 43.725), zoom=10)
   


   #saveRDS(Toronto, 'Map Toronto/Toronto.RDS')
   
   Toronto <- readRDS('Map Toronto/Toronto.RDS')

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
     geom_polygon(data = Map_draw,
                  aes(long, lat, group = group), 
                  colour = 'gray', alpha =.7) + 
     geom_text(data =cnames, aes(long, lat, label = New_Name),
               size =3, check_overlap = TRUE) +
     coord_map() + 
     scale_fill_gradient(low = 'white', high = 'dark blue') +
     
     xlim(c(-79.65,-79.10)) + ylim(c(43.55,43.88)) + 
     labs(fill=' ') +
     ditch_the_axes
   
   
   
  (Plot <-  ggplot() +
     geom_polygon(data = Map_draw,
                  aes(long, lat, group =group, fill = Variable),
                  color = 'gray', aes =.7) +
     coord_map() + 
     scale_fill_gradient(low = 'light blue', high = 'dark blue')+
     labs(fill ="Population") )
   
   ggsave('Map Toronto/Example2.png')
   
   Background <- get_map(location = c(rowMeans(Map@bbox)), 
                 maptype = 'terrain',
                 zoom =10)
   
   
   width <- diff(Map@bbox[1,])
   heigth <- diff(Map@bbox[2,])
   
   plot_area <- ggplot()+xlim( c(Map@bbox[1,1]-(.1*width),
                              Map@bbox[1,2]+.1*width)) +
                        ylim(c(Map@bbox[2,1]-(.1*heigth),
                               Map@bbox[2,2]+.1*heigth))
   
   (Plot <- ggmap(Background, extent = 'normal') +
     geom_polygon(data = Map_draw,
                  aes(long, lat, group =group, fill = Variable),
                  color = 'gray', alpha =.7) +
     coord_map() + 
     scale_fill_gradient(low = 'light blue', high = 'dark blue')+
     labs(fill ="Population")  +
     ditch_the_axes)
   
   ggsave('Map Toronto/Example3.png')
   
  (Plot + scale_x_continuous(expand = c(-.2,0)) +
    scale_y_continuous(expand = c(-.24,0)))
     #ylim(Map@bbox[2,])
  
  ggsave('Map Toronto/Example4.png')
   
