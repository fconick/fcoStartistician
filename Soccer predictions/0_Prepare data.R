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
LigaMX_stats <- read.csv('~/LIGA MX ESPN DATA/ESPNLigaMX_STATS.csv')

LaLiga_teams <- read.csv('~/LIGA MX ESPN DATA/LaLiga/RESULTS.csv')
Premier_teams <- read.csv('~/LIGA MX ESPN DATA/PremierLeague/RESULTS.csv')
LigaMX_teams <- read.csv('~/LIGA MX ESPN DATA/ESPNLigaMX_RESULTS.csv')


Teams <- rbind(LaLiga_teams, Premier_teams, LigaMX_teams)

Teams %<>% gather(Home, Away, key= 'variable', value =  'Team') %>% 
  select(Date, id, variable, Team) %>% 
  mutate(Date = as.Date(Date))

#Add a column for each league
LaLiga_stats$League <- 'La Liga'
Premier_stats$League <- 'Premier League'
LigaMX_stats$League <- 'Liga MX'


STATS <- rbind(LaLiga_stats, Premier_stats, LigaMX_stats)%>% unique %>% 
  left_join(Teams, by = c('id', 'variable'))
  

STATS %<>%  unite(Var_H_A, Var, variable) %>%  
  select(-Team) %>% 
  spread(key= Var_H_A, value = value) %>% 
  mutate(
    Posesion_Home = gsub("%",'', Posesion_Home) %>% as.numeric
  ) %>% select(-Posesion_Away)

STATS[,-c(1:3)] %<>% sapply(as.numeric)

write.csv(STATS, file = 'Soccer predictions/Data.csv', row.names = F)
