#==============================================================================
#                         ANALISYS FOR THE BLOG
#==============================================================================

options(stringsAsFactors = F)
library(magrittr)
library(plyr)
library(ggplot2)
library(stringr)
library(reshape2)

theme_set(theme_minimal())

#1.- Load the data ------------------------------------------------------------
DATA <- read.csv('Possesion_post/Dataset.csv')

#2.- CLean the data for the analysis ------------------------------------------

#Remove the percentage symbol
DATA$value %<>% str_replace(pattern = '%',replacement = '') %>% 
  as.numeric

#Recode for better visulization
DATA$variable[DATA$variable=='Home'] <- 'H'
DATA$variable[DATA$variable=='Away'] <- 'A'

#Arrange the data in a nice format
DATA %<>% dcast( formula = id~ Var + variable  , value.var = 'value') 

#3.- Descriptive analysis -----------------------------------------------------

#Number of matches
nrow(DATA)

# Descriptive analysis of possesion

summary(DATA$Posession_H)
sd(DATA$Posession_H)


ggplot(data=DATA, aes(x= Posession_H)) +
  geom_histogram(aes (y=..count../sum(..count..)), fill='white',
                 colour= "#E69F00", bins = 14, size = 1) +
  labs(y ='Proportion', x = 'Posession (%) \n HOME TEAM')

#4.- Relationship with the goals scored----------------------------------------

DATA$Result <- DATA$Goals_H - DATA$Goals_A

Correlation <- cor(DATA[,-c(1,4)]) %>% round(3) %>% melt

ggplot(data =Correlation, aes(x =Var1, y = Var2, fill=value)) + 
  geom_tile()+
    scale_fill_gradient2(low = "black", high = "black", mid = "white") +
  labs(x='', y='') +
  theme(legend.title = element_text(colour = 'white'))

cor_Pos <- cor(DATA[,-c(1,4)])[,'Posession_H'] %>% round(3) %>% data.frame
colnames(cor_Pos) <- 'Correlation'
cor_Pos$Variable <- row.names(cor_Pos)
cor_Pos %<>% arrange(desc(abs(Correlation))) 
cor_Pos[-1,]


cor(DATA$Posession_H, DATA$Shots_H - DATA$Shots_A)
cor(DATA$Posession_H, (DATA$Shots_H)/(DATA$Shots_H + DATA$Shots_A))

#5.- Visualization between shots on goals and posession ------------------


Pos_vs_Shots <- ddply(DATA[,c('Posession_H', 'Shots_H')], 
      .(Posession_H, `Shots_H`), nrow ) 

ggplot(data=Pos_vs_Shots, aes(x = Posession_H, y = Shots_H )) +
  geom_point(aes(alpha = V1)) + geom_smooth(color = "#E69F00") +
  labs(x='Posession (%)', y = 'Shots on goal', alpha= 'No. Obs')+
  ggtitle( 'Home team \n Posession vs Shots')+
  theme(plot.title = element_text(hjust = 0.5)) 
    
#6.- Visualization between Posession and Goal difference ----------------------


Pos_vs_ShotDif <- ddply(DATA[,c('Posession_H', 'Shots_H', 'Shots_A')], 
                      .(Posession_H, (Shots_H - Shots_A)), nrow ) 

colnames(Pos_vs_ShotDif)[2] <- 'Shot_Dif'

ggplot(data=Pos_vs_ShotDif, aes(x = Posession_H, y = Shot_Dif )) +
  geom_hline(yintercept=0.5, col = 'black', alpha = .05, size = 5)+
  geom_point(aes(alpha = V1)) + geom_smooth(color = "#E69F00") +
  labs(x='Posession (%)', y = 'Shots on goal \n difference', alpha= 'No. Obs')+
  ggtitle( 'Home team \n Posession vs Shot Difference')+ 
  theme(plot.title = element_text(hjust = 0.5)) 


#7.- GLM Posession vs Shot difference

DATA$Pos_simp <- DATA$Posession_H - 50

lm(data=DATA, formula = (Shots_H - Shots_A)~ Pos_simp) %>% summary


#Analysis by Win tie or lose
DATA$WTL <- 'Win'
DATA$WTL[DATA$Result==0] <- 'Tie'
DATA$WTL[DATA$Result<0] <- 'Lose'

Pos_vs_Shots_Res <- ddply(DATA[,c('Posession_H', 'Shots_H','Shots_A', 'WTL')], 
                      .(Posession_H, (Shots_H - Shots_A), WTL), nrow) 

colnames(Pos_vs_Shots_Res)[2] <- 'Shot_Dif'

ggplot(data=DATA, aes(x = Posession_H, y = Shots_H - Shots_A )) +
  geom_point(data = Pos_vs_ShotDif, aes(x = Posession_H, y = Shot_Dif, alpha=V1)) + 
  geom_smooth(aes(group=WTL, , color = WTL)) +
  labs(x='Posession (%)', y = 'Shots on goal \n difference', color= 'Result')+
  ggtitle( 'Home team \n Posession vs Shot Difference')+ 
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values=c( "#E69F00", "#56B4E9", "#009E73"))
