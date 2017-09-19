#------------------------------------------------------------------------
#                  Function to join the shapefile and the stats
#------------------------------------------------------------------------


Join_Map_Data <- function(Map = Map, Data_organized = Data_organized, Category ){
  
  Map$SCODE_NAME %<>% as.character  %>% as.numeric
  
  Aux<- Data_organized[[Category]] 
  colnames(Aux) <- colnames(Data_organized[[Category]])
  Map@data %<>% left_join(Aux, by = 'SCODE_NAME')
  
  
  #Transform polygon data into a data.frame
  Map %<>% spTransform(CRS("+proj=longlat +datum=WGS84"))
  Map_draw <- fortify(Map, region ='SCODE_NAME')
  Map_draw$id %<>% as.numeric
  
  Data <- Map@data
  
  #Add the stats to the map data
  Map_draw %<>% left_join( Data, 
                           by = c('id'='SCODE_NAME'))
  
  Map_draw
}

save(Join_Map_Data, file = 'Map Toronto/Shiny/Join_Map_Data.rda')


