#=============================================================================
#                   Create an index according to performance
#=============================================================================

cbPalette <- c( "#E69F00", "#56B4E9", "#009E73", "#F0E442",
               "#0072B2", "#D55E00", "#CC79A7")


library(magrittr)
library(stringr)
library(tidyverse)
library(ggplot2)
options(stringsAsFactors = F)

STATS <- read.csv('Index_performance/DATABASE.csv')

STATS[,-c(1:4)] %<>% sapply(as.numeric)
STATS$Date %<>%  as.Date


STATS %<>% filter(variable == 'Home')

#Principal components
Princomp <- princomp(STATS[,-c(1:4)])

#Find number of components that explain most of the data
plot(Princomp)

#Biplot
biplot(Princomp)


#Loadings
(Loadings <- Princomp$loadings[,1:2] %>% round(2) %>% data.frame %>% 
  mutate(Attribute = rownames(.)) %>% 
  select(Attribute, everything()) %>% 
  arrange(Comp.1))


#Scores 1st component add it to DATA
STATS$PC1<- Princomp$scores[,1]

#Example of a team
Rayo <- filter(STATS, Team == 'Rayo Vallecano') %>% arrange(as.Date(Date))

#Plot time series of the Index by teams
ggplot(data= STATS, aes(x= Date, y = PC1, color = League)) + 
  geom_hline(yintercept= 0, colour ='red') +
  geom_line() + 
  facet_wrap(~Team) + 
  scale_color_manual(values=cbPalette)

#Box plot by teams
ggplot(data= STATS, aes(y = PC1, x = reorder(Team, PC1, FUN = median),  color = League)) + 
  geom_hline(yintercept= 0, colour ='red') +
  geom_boxplot() + 
  facet_wrap(~League, scales = 'free_y') + coord_flip() + 
  scale_color_manual(values=cbPalette)

#Index of the best teams from 2011 to 2016 with more than 40 matches in the DB
STATS  %>% 
  group_by(Team) %>% filter(n()>30) %>%   summarise(
  Index = mean(PC1)
) %>% arrange(desc(Index)) %>% data.frame


# Missing to add a moving average so the index can determine the "current INdex'
# of each Team
