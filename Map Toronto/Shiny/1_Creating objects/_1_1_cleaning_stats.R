#----------------------------------------------------------------------
#           Get the interesting attributes from  data_organized
#----------------------------------------------------------------------

library(magrittr)
library(dplyr)
library(stringr)

Data_organized <- readRDS('Map Toronto/Shiny/Data_organized.RDS')

#MAke nicer names for the 
names(Data_organized) %<>% str_replace_all('Total number of ', '') %>% 
  str_replace_all(' \\(total\\)', '') %>% 
  str_replace_all('total ', '') %>% 
  str_replace_all('Total ', '') %>% 
  str_replace_all('- ', '') %>% 
  str_to_title

#Delete uninformative list elements
#Data_organized[[13]] <- NULL
#Data_organized[[12]] <- NULL
Data_organized[[11]] <- NULL


#Modify each list element for more interesting data

#Householders info amount
Data_organized[[3]] %<>% 
  mutate(
    Average = (`1 household maintainer` + `2 household maintainers` * 2 +
      `3 or more household maintainers` * 3.05)/100
)  

#Householders info age
Data_organized[[4]] %<>%
  mutate(
    Median = (`25 to 34 years` * 30 + `35 to 44 years` * 40 +
                 `45 to 54 years` * 50 + `55 to 64 years` * 60 +
                `65 to 74 years` * 70 + `75 years and over` * 77 +
                `Under 25 years` * 22) / 100
  )  

#Imigration place of birth
Data_organized[[9]] %<>% select(-contains('Other'))
 
# Recent immigration place of birth
Data_organized[[10]] %<>% select(-contains('Other'))

#Employment
Data_organized[[13]] %<>% mutate(
  `Employment rate` = NULL,
  `Participation rate` = `Participation rate` *Total / 100 ,
  `Participation rate - Male`= `Participation rate - Male` * Total/100,
  `Participation rate - Female` = `Participation rate - Female`* Total /100,
  `Unemployment rate` = NULL
)

#Income
Data_organized[[18]] %<>% mutate(
  `Average household to..tal income $` = `Average household to..tal income $`*Total/100,
  `Median household to..tal income $` = `Median household to..tal income $`*Total /100
)

Data_organized[[19]] %<>% mutate(
  `Average household to.tal income $` = `Average household to.tal income $`*Total/100,
  `Median household to.tal income $` = `Median household to.tal income $`*Total /100
)

Data_organized[[20]] %<>% mutate(
  `% of tenant households spending 30% or more of household to.tal income on shelter costs` = `% of tenant households spending 30% or more of household to.tal income on shelter costs`*Total/100,
  `Average monthly shelter costs for rented dwellings ($)` = `Average monthly shelter costs for rented dwellings ($)`*Total /100
)


Data_organized[[21]] %<>% mutate(
  `% of owner households spending 30% or more of household to.tal income on shelter costs` = `% of owner households spending 30% or more of household to.tal income on shelter costs`*Total/100,
  `Average monthly shelter costs for owned dwellings ($)` = `Average monthly shelter costs for owned dwellings ($)`*Total /100
)

Data_organized[[22]] %<>% mutate(
  `Prevalence of low income in 2010 based on after-tax low-income measure %` = `Prevalence of low income in 2010 based on after-tax low-income measure %`*Total/100
)

Data_organized[[23]] %<>% mutate(
  `Median income $` = `Median income $`*Total/100,
  `Average income $` = `Average income $`*Total/100
)

##############              Not done ##########################
#Stuck at 20

#Get an example to  work for the front end
saveRDS(Data_organized,
        'Map Toronto/Shiny/Data_organized_clean.RDS')

