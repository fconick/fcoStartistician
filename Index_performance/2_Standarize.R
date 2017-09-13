#=============================================================================
#                   Standarize fouls vars by league
#=============================================================================

#Load libraries
<<<<<<< HEAD
libraries <- c('plyr','zoo', 'magrittr', 'stringr', 'tidyverse', 'ggplot2',
               'forecast', 'lubridate')
=======
libraries <- c('plyr','zoo', 'magrittr', 'stringr', 'tidyverse', 'ggplot', 'forecast',
               'lubridate')
>>>>>>> origin/master

lapply(libraries,require, character.only = TRUE )

#Define plotting and data importing specifications
options(stringsAsFactors = F)

theme_set(theme_minimal())
cbPalette <- c("#0072B2", "#D55E00")

#Load data and prepare it for the analysis
STATS <- read.csv('Index_performance/DATABASE.csv')
#STATS <- read.csv('https://raw.githubusercontent.com/fconick/fcoStartistician/master/Index_performance/DATABASE.csv')

STATS[,-c(1:4)] %<>% sapply(as.numeric)
STATS$Date %<>%  as.Date


STATS %<>% filter(variable == 'Home')


<<<<<<< HEAD
Fouls.cols <- grep('Fouls|Cards',colnames(STATS), value = T)
=======
Fouls.cols <- grep('Faltas|Tarjetas',colnames(STATS), value = T)
>>>>>>> origin/master

#Why one league is divided by PC.2

Fouls.summary <- STATS %>% ddply(.(League), function(x){
  sapply(x[,Fouls.cols], mean)
})

#Tweak scale function
scale.data.frame <- function(x){
  scale(x) %>% c
}

#Standarize  all variables related with fouls  
STATS %<>% group_by(League) %>% 
<<<<<<< HEAD
  mutate_each(funs(scale.data.frame), contains("Fouls"), contains('Cards'))
=======
  mutate_each(funs(scale.data.frame), contains("Faltas"), contains('Tarje'))
>>>>>>> origin/master

#Principal components
Princomp <- princomp(STATS[,-c(1:4)], cor = T)

#Find number of components that explain most of the data
plot(Princomp)

#Biplot
biplot(Princomp)


#Rerun the princomp
(Loadings <- Princomp$loadings[,1:2] %>% round(2) %>% data.frame %>% 
    mutate(Attribute = rownames(.)) %>% 
    select(Attribute, everything()) %>% 
    arrange(Comp.2))


#Scores 2 component add it to DATA
<<<<<<< HEAD
STATS$PC1<- -Princomp$scores[,1]
STATS$PC2<- -Princomp$scores[,2]
=======
STATS$PC1<- Princomp$scores[,1]
STATS$PC2<- Princomp$scores[,2]
>>>>>>> origin/master

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


Index.2016 <- STATS %>% group_by(Team) %>% 
  filter(Date == max(Date)) %>% 
  select(Team, M.A.PC1, M.A.PC2, Date, League) %>% filter(Date>= as.Date('2016-11-01'))

<<<<<<< HEAD

ggplot(Index.2016, aes(x = M.A.PC1, y = M.A.PC2, color = League)) +
  geom_hline(aes(yintercept=mean(M.A.PC2)), colour = 'black', linetype = 2) +
  geom_vline(aes(xintercept=mean(M.A.PC1)), colour = 'black', linetype = 2) +
  geom_point(show.legend = F) +
  geom_text(aes(label = Team), check_overlap = T, nudge_y = 0.08,
            show.legend = F)+
  scale_color_manual(values=cbPalette) +
  labs(x = 'General Performance', y = 'Fouls and Misconduct Scores', 
       title = 'Teams January 2017 Index') +
  theme(plot.title = element_text(hjust = 0.5))#+
  #geom_smooth(method = 'gam', se = T, alpha = .2)

ggsave('Index_performance/Index_Teams_div.png')
=======
ggplot(Index.2016, aes(x = M.A.PC1, y = M.A.PC2, color = League)) +
  geom_point(show.legend = F) +
  geom_text(aes(label = Team), check_overlap = T, nudge_y = 0.08,
            show.legend = F, size =2.5)+
  scale_color_manual(values=cbPalette) +
  labs(x = 'General Performance', y = 'Fouls and Misconduct Scores', 
       title = 'Teams January 2017 scores') +
  theme(plot.title = element_text(hjust = 0.5))#+
  #geom_smooth(method = 'gam', se = T, alpha = .2)

>>>>>>> origin/master

