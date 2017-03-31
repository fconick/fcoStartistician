#==============================================================================
#                         ANALISYS FOR THE BLOG
#==============================================================================

#1.- Calculating each probability --------------------------------

#Chances of choosing North Carrolina or Gonzaga for the final 4

s1_0 <- 3/38
s1_12 <- 15/38
s1_3<- 4/38
s1_4 <- 1/38


Seed1_FF <- (1/4 * s1_12 + 2/4 * s1_12 + 3/4 * s1_3 + s1_4)


Seed_notN1_FF <- (1 - Seed1_FF)/10


#Chances of winning the Championship prior the event

C_S1_1No1 <- s1_12 * 1/4 * (1/4 *(7/10)^3 + 1/3 * (7^2 *3^2/10^3) +
                              1/2 *(7 *3^3/10^3) + (3/10)^3)
                            
C_S1_2No1 <- s1_12 *1/2 *( (7/10)^2 * 1/4 + (7*3*2/10^2 * 1/3) + 
                             (3/10)^2 * 1/2)

C_S1_3No1 <- s1_3 * 3/4 * ( 7/10 * 1/4 +3/10 *1/3 )

C_S1_4No1 <- s1_4 * 1/4


No1_C_Prob <- C_S1_1No1 + C_S1_2No1 + C_S1_3No1 + C_S1_4No1

                      

#Chances of choosing Oregon and South Carolina for the Final Four
Seed_not_N1_FF <- a.07 * 1/10 + a.39*3/4*1/10 + 1/2 *a.39 * 1/10 + 1/4*a.1 *1/10


#Chances of a not N1 seed to win the tournament

NotNo1_C_Prob <- (1 -  4 * No1_C_Prob) / 28

#2.- Ordering the data for the figures --------------------------

data <- data.frame(Teams = c('South Carolina', 'Gonzaga', 'Oregon', 'North Carolina'))
data$FF_Random <- 1/16
data$FF_People <- c( .006 ,.346 ,.11 ,.43) 
data$FF_Rules <- c(Seed_notN1_FF,Seed1_FF,Seed_notN1_FF,Seed1_FF)
data$C_Random <- 1/64
data$C_People <- c( .001, .085 , .016 , .152)
data$C_Rules <- c(NotNo1_C_Prob, No1_C_Prob, NotNo1_C_Prob , No1_C_Prob)

library(plyr)
library(stringr)

DATA <- melt(data, id.vars = 'Teams')
DATA[,4:5] <- str_split_fixed(string = DATA$variable, '_',2)
DATA <- DATA[,c(1,3,4,5)] 
DATA <-  dcast(DATA, Teams +V4 +V5 ~., )

DATA$Stage <- 1
DATA$Stage[DATA$V4=='C'] <- 2

#3.- Using ggplot to show the data ------------------------------

library(ggplot2)

theme_set(theme_minimal())
ggplot(DATA, aes(x=Stage, y =. , colour = V5)) +
  geom_point() + facet_wrap(~Teams, scales = 'free_y') +
    geom_line() + scale_x_discrete('')+
  labs(x ="", y = 'Probabilty', color= 'Strategy')+
  ggtitle( 'Chances of choosing a team according to a strategy')+ 
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(values=c( "#E69F00", "#56B4E9", "#009E73"))


Proba_FF <- (sapply(data[,2:4], prod) *100) %>% data.frame

Proba_FF$Strategies <- c("Random", ' People', "Rules")

ggplot(data= Proba_FF, aes(x =Strategies, y = ., fill = Strategies)) +
  geom_bar(stat = 'identity') +
  scale_fill_manual(values=c( "#E69F00", "#56B4E9", "#009E73"))+
  theme(legend.position = 'none') +
  labs(x ="", y = 'Chances of success (%)')+
  ggtitle( 'Chances of choosing perfect your Final Four 2017 teams' )+
  theme(plot.title = element_text(hjust = 0.5)) 
  

