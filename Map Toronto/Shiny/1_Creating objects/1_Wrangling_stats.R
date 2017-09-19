#------------------------------------------------------------------------
#        Transform the data from Ward Profiles - NHS_2011.xlsx
#------------------------------------------------------------------------
library(stringr)
library(readxl)
library(magrittr)
library(tidyverse)


#1.- Read data
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
 transform_data <- function(DATA, percent =TRUE){
  
  DATA %<>% dplyr::filter(!is.na(Toronto)) 
  DATA$Toronto %<>% as.numeric
  
  #Melt the data and make Wards as rows and data as columns
  M.Data <- DATA %>%   
    tidyr::gather( Ward, value, -Attributes) %>% 
    dplyr::filter(!is.na(value), !is.na(Attributes))
  
  M.Data$value %<>% as.numeric
  M.Data %<>% tidyr::spread(value = value , key = Attributes)
  
  #.- Subset data to put in the map
  Exclude <- which(colnames(M.Data) %in% c("Ward", 'Total') )
  
  if (percent ==T){
    M.Data[,-c(Exclude)] <-   M.Data[,-Exclude] * 100 / M.Data$Total 
  }
  
  #Get the code name out of the Ward variable
  M.Data$SCODE_NAME <- str_extract(string = M.Data$Ward, '[0-9]+') %>% 
    as.numeric
  
  Stats <- M.Data %>% dplyr::select(-Ward) %>%
    dplyr::filter(!is.na(SCODE_NAME))
 
  Stats

}

#Apply the function to the list
Data_organized_part1 <- lapply(Data_divided[1:24], transform_data)

Data_organized_part2 <- map(Data_divided[25:29], transform_data, FALSE)


Data_organized <- c(Data_organized_part1, Data_organized_part2)

saveRDS(Data_organized, 'Map Toronto/Shiny/Data_organized.RDS')