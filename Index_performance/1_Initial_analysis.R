#=============================================================================
#                   Create an index according to performance
#=============================================================================

<<<<<<< HEAD
#Load libraries
libraries <- c('plyr','zoo', 'magrittr', 'stringr', 'tidyverse', 'ggplot2', 
               'forecast', 'lubridate')

lapply(libraries,require, character.only = TRUE )

#Define plotting and data importing specifications
=======
cbPalette <- c("#0072B2", "#D55E00")

library(zoo)
library(plyr)
library(magrittr)
library(stringr)
library(tidyverse)
library(ggplot2)
require(forecast)
>>>>>>> origin/master
options(stringsAsFactors = F)

theme_set(theme_minimal())
cbPalette <- c( "#D55E00", "#0072B2")

#Load data and prepare it for the analysis
STATS <- read.csv('Index_performance/DATABASE.csv')
#STATS <- read.csv('https://raw.githubusercontent.com/fconick


# Change class to several columns
STATS[,-c(1:4)] %<>% sapply(as.numeric)
STATS$Date %<>%  as.Date


#Subset the data 
STATS %<>% filter(variable == 'Home')

# 1.- START THE ANALYSIS ------------------------------------------------------

#Principal components
Princomp <- princomp(STATS[,-c(1:4)], cor = T )

#Find number of components that explain most of the data
plot(Princomp)

#Biplot
<<<<<<< HEAD
biplot(Princomp, col =c('white', 'red'), cex = 1)
=======
#biplot(Princomp, col =c('white', 'red'), cex = .5)
>>>>>>> origin/master


#Loadings
(Loadings <- Princomp$loadings[,1:2] %>% round(2) %>% data.frame %>% 
    mutate(Attribute = rownames(.)) %>% 
    select(Attribute, everything()) %>% 
    arrange(Comp.2))


#Scores 2 component add it to DATA
STATS$PC1<- -Princomp$scores[,1]
STATS$PC2<- Princomp$scores[,2]


#Plot time series of the Index by teams
ggplot(data= STATS, aes(x= Date, y = PC1, color = League)) + 
  geom_hline(yintercept= 0, colour ='red') +
  geom_line(show.legend = F) + 
  facet_wrap(~Team) + 
  scale_color_manual(values=cbPalette)
 
#Show 4 teams to put on the report

Teams.example <- c("Granada", 'Barcelona', 'Manchester City', "Stoke City")
Sample <- STATS %>% filter(Team %in% Teams.example)

ggplot(data= Sample, aes(x= Date, y = PC1, color = League)) + 
  geom_hline(yintercept= 0, colour ='red') +
  geom_line(show.legend = F)  + 
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
<<<<<<< HEAD
  ) %>% arrange(desc(Index.1)) %>%
  select(Team, Index.1) %>%
  data.frame %>% write.csv(row.names =F)
=======
  ) %>% arrange(desc(Index.1)) %>% data.frame
>>>>>>> origin/master


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
  geom_hline(yintercept= 0, colour ='black') +
  geom_line(aes(y = PC1), colour = 'gray', linetype= 2, size = .1) + 
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

<<<<<<< HEAD
ggplot(data = subset(STATS,abs(Goals-Goals_Opp)>3), aes(y = PC2, x = PC1)) + 
  geom_jitter(aes(color = Goals - Goals_Opp))  +
  scale_colour_gradient(low = 'green', high = 'red') +
  facet_wrap(~(Goals-Goals_Opp))
=======
ggplot(data = STATS, aes(y = PC2, x = PC1)) + 
  geom_point(aes(color = Goals - Goals_Opp))  +
  scale_colour_gradient(low = 'white', high = 'red')
>>>>>>> origin/master

ggplot(data = STATS, aes(y = Goals, x =  PC1)) + 
  geom_jitter() +
  geom_smooth()

lm(formula =   Goals - Goals_Opp~PC1, data= STATS) %>% summary

#No explica mucho

#Using Moving average get the latest MA of each team and plot it

Index.2016 <- STATS %>% group_by(Team) %>% 
  filter(Date == max(Date)) %>% 
  select(Team, M.A.PC1, M.A.PC2, Date, League) %>% 
  filter(Date>= as.Date('2016-11-01')) %>% arrange(M.A.PC1)

ggplot(Index.2016, aes(x = M.A.PC1, y = M.A.PC2, color = League)) +
  geom_point(show.legend = F) +
  geom_text(aes(label = Team), check_overlap = TRUE, nudge_y = 0.08,
            show.legend = F, size = 2.9) +
  scale_color_manual(values=cbPalette) +
  labs(x = 'PC1', y = 'PC2')


<<<<<<< HEAD
Fouls.cols <- grep('Fouls|Cards',colnames(STATS), value = T)

#Why one league is divided by PC.2

(Fouls.summary <- STATS %>% ddply(.(League), function(x){
  sapply(x[,c(Fouls.cols, 'PC2')], mean)
})) 

# Create the plot for the initail image

library(png)
library(grid)
img <- readPNG('Index_performance/Premier-League-Logo-shield.png')
Premier <- rasterGrob(img, interpolate=TRUE)

img <- readPNG('Index_performance/LaLiga.png')
LaLiga <- rasterGrob(img, interpolate=TRUE)


ggplot(Index.2016, aes(x = M.A.PC1, y = M.A.PC2, color = League)) +
  annotation_custom(LaLiga, xmin=.2, xmax=1.3, ymin=-1, ymax=0) +
  annotation_custom(Premier, xmin=-1.9, xmax=-.40, ymin=.1, ymax=1.1) +
  geom_point(show.legend = F) +
  geom_text(aes(label = Team), check_overlap = T, nudge_y = 0.08,
            show.legend = F, size =2.5)+
  scale_color_manual(values=cbPalette) +
  labs(x = 'General Performance', y = 'Fouls and Misconduct Scores', 
       title = 'Teams January 2017 scores') +
  theme(plot.title = element_text(hjust = 0.5))
=======
Fouls.cols <- grep('Faltas|Tarjetas',colnames(STATS), value = T)

#Why one league is divided by PC.2

Fouls.summary <- STATS %>% ddply(.(League), function(x){
  sapply(x[,c(Fouls.cols, 'PC2')], mean)
})
>>>>>>> origin/master
