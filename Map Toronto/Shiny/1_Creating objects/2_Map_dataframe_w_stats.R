#------------------------------------------------------------------------
#                           Join stats with map shape
#------------------------------------------------------------------------

library(readxl)
library(magrittr)
library(rgdal)
library(tidyverse)


#1.- Read Map data this has to be done only once

Map <- readOGR("Map Toronto/Ward 2","icitw_wgs84")

Data_organized <- readRDS( 'Map Toronto/Shiny/Data_organized_clean.RDS')

#2.-Join the data

Map$SCODE_NAME %<>% as.character  %>% as.numeric

for(i in 1:length(Data_organized)){
  Aux <- Data_organized[[i]]
  colnames(Aux) <- colnames(Data_organized[[i]])
  Map@data %<>% left_join(Aux, by = 'SCODE_NAME')
}

#Transform polygon data into a data.frame
Map %<>% spTransform(CRS("+proj=longlat +datum=WGS84"))
Map_draw <- fortify(Map, region ='SCODE_NAME')
Map_draw$id %<>% as.numeric

Data <- Map@data

#Add the stats to the map data
Map_draw %<>% left_join( Data, 
                         by = c('id'='SCODE_NAME'))

saveRDS(Map_draw, 'Map Toronto/Shiny/Map_draw.RDS')
