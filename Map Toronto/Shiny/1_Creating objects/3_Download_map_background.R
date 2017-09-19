#----------------------------------------------------------------------
#                   Download the map from goggle maps
#----------------------------------------------------------------------

library(ggmap)

Toronto <- get_map(location = c(-79.43, 43.725), zoom=10, 
                   maptype = 'roadmap')

saveRDS(Toronto, 'Map Toronto/Shiny/Toronto.RDS')
