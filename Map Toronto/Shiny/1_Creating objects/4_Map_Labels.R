#----------------------------------------------------------------------
#                   Labels for each war, making them much nicer
#----------------------------------------------------------------------

library(magrittr)
library(dplyr)
library(rgdal)

options(stringsAsFactors = F)
Map <- readOGR("Map Toronto/Ward 2","icitw_wgs84")

Data <- Map@data
Data$SCODE_NAME %<>% as.numeric
Map_figure <- fortify(Map, region ='SCODE_NAME')
Map_figure$id %<>% as.numeric

#Add the stats to the map data
Map_figure %<>% left_join( Data, 
                         by = c('id'='SCODE_NAME'))

#Aggregate data to get mean latitude and mean longitude for each state
cnames <- aggregate(cbind(long, lat) ~ NAME, data=Map_figure,
                    FUN=function(x) mean(range(x)))

cnames$New_Name <- gsub(pattern = " *\\(.*?\\) *",
                        x = cnames$NAME, replacement = '')

cnames$New_Name <- gsub(pattern = "-",
                        x = cnames$New_Name, replacement = '\n')

cnames$New_Name <- gsub(pattern = ' ',
                        x = cnames$New_Name, replacement = '\n')

cnames %<>% dplyr::group_by(New_Name) %>% 
  dplyr::summarise(
    long = mean(long),
    lat = mean(lat)
  )

cnames %<>% mutate(
  New_Name=replace(New_Name, New_Name=='Toronto\nCentre\nRosedale', 
                   'To. Centre\nRosedale'), 
  lat = replace(lat, New_Name == 'Toronto\nDanforth', 43.67878),
  long = replace(long, New_Name == 'To. Centre\nRosedale', -79.36746))
  

saveRDS(cnames, 'Map Toronto/Shiny/cnames.RDS')


