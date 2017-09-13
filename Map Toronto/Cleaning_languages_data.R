#------------------------------------------------------------------------------
#                             Manipulating data
#------------------------------------------------------------------------------

#1.- Read data and make it ready to do queries

#Read data
DATA <- read_excel('Map Toronto/Ward Profiles - Census_2011.xlsx', 
                   range = 'A11:AX642')

colnames(DATA)[1] <- 'Attributes'


#2.- Retreiv desired data (In this case LANGUAGES used at home)
Start <- which(DATA$Attributes==
                 'Detailed language spoken most often at home - Total population excluding institutional residents')

DATA <- DATA[Start:nrow(DATA),] %>% 
  filter(!is.na(`Ward 1`) & Toronto> 0 )

DATA[1,1] <- 'Total'
DATA$Toronto %<>% as.numeric

#Melt the data and make Wards as rows and data as columns
M.Data <- DATA %>%   filter(Toronto>1000) %>% 
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


saveRDS(Stats, 'Map Toronto/Stats.RDS')
