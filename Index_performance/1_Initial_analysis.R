#=============================================================================
#                   Create an index according to performance
#=============================================================================

cbPalette <- c("#0072B2", "#D55E00")

library(zoo)
library(plyr)
library(magrittr)
library(stringr)
library(tidyverse)
library(ggplot2)
require(forecast)
options(stringsAsFactors = F)

theme_set(theme_minimal())

STATS <- read.csv('Index_performance/DATABASE.csv')

STATS[,-c(1:4)] %<>% sapply(as.numeric)
STATS$Date %<>%  as.Date


STATS %<>% filter(variable == 'Home')

#Principal components
Princomp <- princomp(STATS[,-c(1:4)], cor = T)

#Find number of components that explain most of the data
plot(Princomp)

#Biplot
#biplot(Princomp, col =c('white', 'red'), cex = .5)


#Loadings
(Loadings <- Princomp$loadings[,1:2] %>% round(2) %>% data.frame %>% 
    mutate(Attribute = rownames(.)) %>% 
    select(Attribute, everything()) %>% 
    arrange(Comp.2))


#Scores 2 component add it to DATA
STATS$PC1<- Princomp$scores[,1]
STATS$PC2<- Princomp$scores[,2]


#Plot time series of the Index by teams
ggplot(data= STATS, aes(x= Date, y = PC1, color = League)) + 
  geom_hline(yintercept= 0, colour ='red') +
  geom_line() + 
  facet_wrap(~Team) + 
  scale_color_manual(values=cbPalette)

#Box plot by teams
ggplot(data= STATS, aes(y = PC1, x = reorder(Team, PC1, FUN = median),  color = League)) + 
  geom_hline(yintercept= 0, colour ='red') +
  geom_boxplot(show.legend = F) + 
  facet_wrap(~League, scales = 'free_y') + coord_flip() + 
  scale_color_manual(values=cbPalette)

#Index of the best teams from 2011 to 2016 with more than 40 matches in the DB
STATS  %>% 
  group_by(Team) %>% filter(n()>30) %>%   summarise(
    Index.1 = mean(PC1),
    Index.2 = mean(PC2)
  ) %>% arrange(desc(Index.1)) %>% data.frame


#add a moving average so the index can determine the "current INdex'
# of each Team


STATS %<>% group_by(Team) %>%  filter(n()>30) %>%
  arrange(Date) %>% 
  mutate(
    M.A.PC1 = rollmeanr(PC1, 15, fill = NA ),
    M.A.PC2 = rollmeanr(PC2, 15, fill = NA )
  )

#Plot time series of the Index by teams
ggplot(data= STATS, aes(x= Date )) + 
  geom_hline(yintercept= 0, colour ='red', size = .1) +
  geom_line(aes(y = PC1), colour = 'gray', linetype= 2) + 
  geom_line(aes(y = M.A.PC1, color = League)) + 
  facet_wrap(~Team) + 
  scale_color_manual(values=cbPalette)  


#How good does the index explains the result??

#Average according to the scores
AVG_PC1 <- STATS %>% group_by(Dif = Goals - Goals_Opp) %>% 
  summarise(
    PC1.mean = mean(PC1),
    count = n()
  )

ggplot(STATS, aes(y = PC1, group = Goals - Goals_Opp, x = Goals - Goals_Opp)) +
  geom_boxplot()

ggplot(data = STATS, aes(y = PC2, x = PC1)) + 
  geom_point(aes(color = Goals - Goals_Opp))  +
  scale_colour_gradient(low = 'white', high = 'red')

ggplot(data = STATS, aes(y = Goals, x =  PC1)) + 
  geom_jitter() +
  geom_smooth()

lm(formula =   Goals - Goals_Opp~PC1, data= STATS) %>% summary

#No explica mucho

#Using Moving average get the latest MA of each team and plot it

Index.2016 <- STATS %>% group_by(Team) %>% 
  filter(Date == max(Date)) %>% 
  select(Team, M.A.PC1, M.A.PC2, Date, League) %>% filter(Date>= as.Date('2016-11-01'))

ggplot(Index.2016, aes(x = M.A.PC1, y = M.A.PC2, color = League)) +
  geom_point(show.legend = F) +
  geom_text(aes(label = Team), check_overlap = TRUE, nudge_y = 0.08,
            show.legend = F)


Fouls.cols <- grep('Faltas|Tarjetas',colnames(STATS), value = T)

#Why one league is divided by PC.2

Fouls.summary <- STATS %>% ddply(.(League), function(x){
  sapply(x[,c(Fouls.cols, 'PC2')], mean)
})
