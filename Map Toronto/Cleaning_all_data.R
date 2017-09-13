#------------------------------------------------------------------------------
#                             Manipulating data
#------------------------------------------------------------------------------

#1.- Read data and make it ready to do queries
library(readxl)
#Read data
DATA <- read_excel('Map Toronto/Ward Profiles - NHS_2011.xlsx', 
                   range = 'A11:AX642')

colnames(DATA)[1] <- 'Attributes'


#2.- Divide data by type and information
Start <- c(grep('otal', DATA$Attributes),nrow(DATA)+1)

Data_divided <- vector("list", length(Start)-1)
for(i in 1:(length(Start)-1)){
  Data_divided[[i]] <- DATA[Start[i]:(Start[i+1]-1),]
 names(Data_divided)[i] <- Data_divided[[i]][1,1]
 Data_divided[[i]][1,1] <- 'Total'
}

#Create a function to transform each data frame

transform_data <- function(DATA){
  
  DATA %<>% filter(!is.na(Toronto)) 
  DATA$Toronto %<>% as.numeric
  
  #Melt the data and make Wards as rows and data as columns
  M.Data <- DATA %>%   
    gather( Ward, value, -Attributes) %>% 
    filter(!is.na(value), !is.na(Attributes))
  
  M.Data$value %<>% as.numeric
  M.Data %<>% spread(value = value , key = Attributes)
  
  #.- Subset data to put in the map
  Exclude <- which(colnames(M.Data) %in% c("Ward", 'Total') )
  
  M.Data[,-c(Exclude)] <-   M.Data[,-Exclude] * 100 / M.Data$Total 
  
  #Get the code name out of the Ward variable
  M.Data$SCODE_NAME <- str_extract(string = M.Data$Ward, '[0-9]+') %>% 
    as.numeric
  
  Stats <- M.Data %>% select(-Ward) %>%
    filter(!is.na(SCODE_NAME))
  
  Stats
}


Data_organized <- lapply(Data_divided, transform_data)

saveRDS(Data_organized, 'Map Toronto/Data_organized.RDS')


#5.- Get the data ready to map it

#1.- From now on this will be the way to load the map data 
Map <- readRDS('Map Toronto/Ward 2/Map.RDS')

#Join
Map$SCODE_NAME %<>% as.character  %>% as.numeric

#Map@data %<>% left_join(Data_organized[[2]])#Join with what you are interested

for(i in 1:length(Data_organized)){
  Map@data %<>% left_join(Data_organized[[i]], by = 'SCODE_NAME')
}

#Transform polygon data into a data.frame
Map %<>% spTransform(CRS("+proj=longlat +datum=WGS84"))
Map_draw <- fortify(Map, region ='SCODE_NAME')
Map_draw$id %<>% as.numeric

Data <- Map@data

#Add the stats to the map data
Map_draw %<>% left_join( Data, 
                         by = c('id'='SCODE_NAME'))

saveRDS(Map_draw, 'Map Toronto/Map_draw.RDS')

#Get cnames
cnames <- readRDS('Map Toronto/cnames.RDS')

#6.- Plot the data

#Download the map from goggle maps
# Toronto <- get_map(location = c(-79.43, 43.725), zoom=10)
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
               aes(long, lat, group = group,fill = `$100,000 and over`), 
               colour = 'gray', alpha =.7) + 
  geom_text(data =cnames, aes(long, lat, label = New_Name),
            size =3, check_overlap = TRUE) +
  coord_map() + 
  scale_fill_gradient(low = 'white', high = 'dark blue') +
  
  xlim(c(-79.65,-79.10)) + ylim(c(43.55,43.88)) + 
  labs(fill=' ') +
  ditch_the_axes


