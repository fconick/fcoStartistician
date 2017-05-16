#=============================================================================
#                               Builidng the dataset
#=============================================================================

library(magrittr)
library(stringr)
library(tidyverse)
options(stringsAsFactors = F)

#1.- Get the dataset needed for the post and save it in the folder

LaLiga_stats <- read.csv('~/LIGA MX ESPN DATA/LaLiga/STATS.csv')
Premier_stats <- read.csv('~/LIGA MX ESPN DATA/PremierLeague/STATS.csv') 

LaLiga_teams <- read.csv('~/LIGA MX ESPN DATA/LaLiga/RESULTS.csv')
Premier_teams <- read.csv('~/LIGA MX ESPN DATA/PremierLeague/RESULTS.csv')

Teams <- rbind(LaLiga_teams, Premier_teams)

Teams %<>% gather(Home, Away, key= 'variable', value =  'Team') %>% 
  select(Date, id, variable, Team) %>% 
  mutate(Date = as.Date(Date))

#Add a column for each league
LaLiga_stats$League <- 'La Liga'
Premier_stats$League <- 'Premier League'

STATS <- rbind(LaLiga_stats, Premier_stats)%>% unique %>% 
  left_join(Teams, by = c('id', 'variable'))

#Recode the variables
STATS$Var[STATS$Var=='Posesion'] <- 'Posession'
STATS$Var[STATS$Var=='Tiros a gol'] <- 'Shots'
STATS$Var[STATS$Var=='Goles'] <- 'Goals'

#Spread for the Var's

STATS %<>% spread(key= Var, value = value) %>% 
   mutate(
    Posession = gsub("%",'', Posession) %>% as.numeric
  )
  
#Goals received
Attributes_Opp <- select(STATS, -Date, -League, -Team) %>% mutate(
  variable = ifelse(variable=='Home', 'Away', 'Home')
)

colnames(Attributes_Opp) [-c(1:2)]<- paste0(colnames(Attributes_Opp) [-c(1:2)],
                                            '_Opp')


STATS %<>% left_join(Attributes_Opp, by = c('id', 'variable')) %>% 
  select(-id, -Posession_Opp)

STATS %<>% mutate(variable = ifelse(variable=='Home', 'Away', 'Home')) 



#create a dir for the post

dir.create('Index_performance')

write.csv(STATS, 'Index_performance/DATABASE.csv', row.names=F)
