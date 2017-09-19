#----------------------------------------------------------------------
#           Get the interesting attributes from  data_organized
#----------------------------------------------------------------------

library(magrittr)
library(dplyr)
library(stringr)

Data_organized <- readRDS('Map Toronto/Shiny/Data_organized_clean.RDS')

#MAke nicer names for the 
# names(Data_organized) %<>% str_replace_all('Total number of ', '') %>% 
#   str_replace_all(' (Total)', '') %>% 
#   str_replace_all('total ', '') %>% 
#   str_replace_all('Total ', '') %>% 
#   str_replace_all('- ', '') %>% 
#   str_to_title

#Create the columns with the attributes

Attributes <- lapply(Data_organized, function(x){
    colnames(x)[!colnames(x) %in% c("SCODE_NAME", 'Total')]
  })

empty_att <- which(lapply(Attributes, length)==0)

for(i in empty_att){
  Attributes[[i]] <- 'Total'
}

#Missing the details of categories
saveRDS(Attributes,'Map Toronto/Shiny/Attributes.RDS')
