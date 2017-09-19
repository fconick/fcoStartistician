# Deal with bike rings data

library(readxl)
library(ggmap)

Bike_rings <- read_excel('Map Toronto/installs_2011.xls')

Bike_rings_geocode <- geocode(paste(Bike_rings$ADDRESS, 'Toronto',
                              sep = ' '))


Bike_rings %<>% cbind(Bike_rings_geocode) %>% 
  filter(!is.na(lon))

saveRDS(Bike_rings, 'Map Toronto/Bike_rings.RDS')


Traffic_cameras <- read.csv('Map Toronto/Cameras.csv')[1:3]

saveRDS(Traffic_cameras, 'Map Toronto/Traffic_cameras.RDS')
