options(stringsAsFactor = F)
options(warn = F)
library(plyr)
library(dplyr)
library(magrittr)
library(ggplot2)
library(stringr)
library(gridExtra)
library(zoo)
theme_set(theme_minimal())

#1.- LOAD THE DATA AND CLEAN IT ------------------------------------------------

#Women -

ranking <- read.csv("PSA Rankings/Womens April.csv") 
colnames(ranking) <- c("Rank", "Points", "Avg")
ranking <- ranking[-grep('Flag', ranking$Rank),]
ranking %<>% .[-c(1:2),]

RANK <- list(NULL)
for(i in 1:(dim(ranking)[1]/3) ){
  
  #print(i)
  RANK[[i]] <- ranking[c((1 + (3 * (i-1) )):(3 + (3 * (i-1) ))), ]
  
}

RANK_WOMEN <- ldply(RANK, function(x){
  y <- as.character(x[1,1])
  y[2]<- as.character(x[2,1])
  y[3]<- as.numeric(as.character(x[3,1]) )
  y[4]<- as.character(x[3,2]) %>% str_replace_all(',', '') 
  y[5]<- as.character(x[3,3]) %>% str_replace_all(',', '') 
  y
})

colnames(RANK_WOMEN) <- c("Ranking", "Name", "Tournaments", "Points", "Avg")


# Men - 


ranking <- read.csv("PSA Rankings/Mens April.csv") 
colnames(ranking) <- c("Rank", "Points", "Avg")
ranking <- ranking[-grep('Flag', ranking$Rank),]
ranking %<>% .[-c(1:2),]

RANK <- list(NULL)
for(i in 1:(dim(ranking)[1]/3) ){
  
  #print(i)
  RANK[[i]] <- ranking[c((1 + (3 * (i-1) )):(3 + (3 * (i-1) ))), ]
  
}

RANK_MEN <- ldply(RANK, function(x){
  y <- as.character(x[1,1])
  y[2]<- as.character(x[2,1])
  y[3]<- as.numeric(as.character(x[3,1]) )
  y[4]<- as.character(x[3,2]) %>% str_replace_all(',', '') 
  y[5]<- as.character(x[3,3]) %>% str_replace_all(',', '') 
  y
})

colnames(RANK_MEN) <- c("Ranking", "Name", "Tournaments", "Points", "Avg")

#Bind both ranks

RANK_MEN$GENDER <- 'MEN'
RANK_WOMEN$GENDER <- 'WOMEN'

RANK <- rbind(RANK_MEN, RANK_WOMEN)

#2.- DESCRIPTIVE ANALYSIS -----------------------------------------------------


Summary <- RANK %>% ddply(.(GENDER), function(x){
  c(Average = sum(as.numeric(x$Avg)), 
  Points = sum(as.numeric(x$Points)), No.Players = length(x$Avg),
  Active.Players = nrow(x[x$Points>0,]), 
  Inactive.Pct = (nrow(x[x$Points==0,])/nrow(x)*100) %>% round(2)) 
})


#Check the relationship between points and average points
Summary$Tr.points <- Summary$Points/Summary$Average




#3.- Analysis using Average as the value of each observation

RANK[,-c(2,6)] %<>% sapply(as.numeric)


RANK %<>% ddply(.(GENDER), function(x){
  x$Avg.Pct <- (x$Avg/sum(x$Avg)) 
  y <- arrange(x, desc(Ranking))
  y$Pct.Cum <- cumsum(y$Avg.Pct)%>%  round(4)
  y
})

RANK.Active <- subset(RANK, Points>0)
R.points <- subset(RANK.Active, Ranking%in%c(1:5,10,15, 20,28, 50,100,150,200,250,300,400))

A1 <- ggplot(RANK.Active, aes(y=Pct.Cum*100,  x = Ranking, color = GENDER)) + 
  geom_line() +
 geom_point( data=R.points, aes(y=Pct.Cum*100,  x = Ranking, color = GENDER)) +
  scale_colour_manual(values=c( '#0072B2', "#D55E00")) + 
  scale_x_log10(breaks = c(2, 10, 5,50, 20,100,200,400)) +
  labs(y = '') + 
  ggtitle("Logarithmic Transformation") +
  theme(plot.title=element_text(hjust=.5))

A2 <- ggplot(RANK.Active, aes(y=Pct.Cum*100,  x = Ranking, color = GENDER)) + 
  geom_line() +
   geom_point( data=R.points, aes(y=Pct.Cum*100,  x = Ranking, color = GENDER)) +
  scale_colour_manual(values=c( '#0072B2', "#D55E00")) + 
  #scale_x_log10(breaks = c(2, 10, 5,50, 20,100,200,400)) +
  labs(y = 'Effort (%)', title = '') + 
  #ggtitle("Normal view") + 
  theme(plot.title=element_text(hjust=.5), legend.position = 'none')

grid.arrange(A2, A1, ncol =2)

#Comparison vector 

Comparison <- data.frame(V1 = seq(0,1, by=.0001) %>% round(4) %>%  as.character)

MEN <- subset(RANK.Active, GENDER=='MEN', select = c('Pct.Cum','Ranking')) %>% 
  ddply(.(Ranking), function(x){(mean(x$Pct.Cum) %>% round(4)) %>% as.character})

WOMEN <- subset(RANK.Active, GENDER=='WOMEN', select = c('Pct.Cum','Ranking')) %>% 
  ddply(.(Ranking), function(x){(mean(x$Pct.Cum) %>% round(4)) %>% as.character})
  
C1 <- join(Comparison, MEN)

colnames(C1) <- c('V1', 'Men')

C1 <- join(C1,WOMEN, match ='first') 
       
colnames(C1) <- c('V1', 'Men', 'Women')    

C1 %<>% apply(2,as.numeric) %>% data.frame %>% arrange(desc(V1))


#Get the table for the comparison

D1 <- subset(C1, !is.na(Men) | !is.na(Women))


D1$Men %<>% na.approx(D1$V1) 
D1$Women %<>% na.approx(D1$V1)



D1$V2 <- D1$Women
D1$V3 <- 472/217*D1$Women - 255/217

R1 <- ggplot(data=D1, aes(x= Women, y = Men)) +
  geom_line(aes(color = 'Effort (%)'), size = 1) +# geom_point() +
  geom_line( linetype =1, aes(colour = 'Naive', y =V2), size =1) +
  geom_line( linetype =2, aes(colour = 'Active Players', y =V3), size = .5) +
scale_colour_manual(name="Matching tecnique",
                    values=c(`Effort (%)` ="#56B4E9", Naive="#E69F00",
                             `Active Players`="BLACK"))+
  scale_x_continuous(limits = c(1,260), breaks = c(1,100,200))+
  scale_y_continuous(limits = c(1,470), breaks = c(1,100,200,300,400)) +
  ggtitle('Equivalent Rankings') +
  theme(plot.title=element_text(hjust=.5), legend.position = 'none')


R2 <- ggplot(data=D1, aes(x= Women, y = Men)) +
  geom_line(aes(color = 'Effort (%)'), size = 1.1) +# geom_point() +
  geom_line( linetype =1, aes(colour = 'Naive', y =V2),size = 1) +
  geom_line( linetype =2, aes(colour = 'Active Players', y =V3), size = 1) +
  scale_colour_manual(name="Matching method",
                      values=c(`Effort (%)` ="#56B4E9", Naive="#E69F00",
                               `Active Players`="BLACK"))+
  scale_x_continuous(limits = c(1,13), breaks = c(1,3,5,7,9,11))+
  scale_y_continuous(limits = c(1,13), breaks = c(1,3,5,7,9,11)) +
  ggtitle(' Zoom  Top 10') +
  labs (x='')+
  theme(plot.title=element_text(hjust=.5))
  

grid.arrange(R1, R2, ncol =2)

#Format the table
# 

D1 %<>% subset(V1>0, select= c('Women', 'Men',  'V3', 'V2'))

colnames(D1) <- c('Women', 'Effort (%)', 'Active Players', 'Naive')

D.complete <- D1

D1 %<>%subset( Women%in%c(1:8,10,20,50,100) | `Effort (%)`%in%c(50,100, 200)) 

D1%<>% sapply(round) %>% data.frame

G1 <- ggplot(RANK, aes( x=Ranking, y =Tournaments, group = GENDER, color = GENDER)) +
  geom_point( alpha=.4) +geom_smooth(method = 'loess') +
  scale_colour_manual(values=c( '#0072B2', "#D55E00")) + 
  facet_wrap(~GENDER, scales = 'free_x') + 
  theme(legend.position = 'none')


G2 <- ggplot(RANK, aes( x=Ranking, y =Tournaments, group = GENDER, color = GENDER)) +
  geom_point(alpha =.3) +geom_smooth(alpha =0.6, method = 'loess') +
  scale_colour_manual(values=c( '#0072B2', "#D55E00")) + 
  theme(legend.position = 'none')

grid.arrange(G1, G2)

#Get every ranking observation

R.men <- subset(RANK_MEN, Tournaments > 0 , select = c('Ranking', 'Tournaments')) 
colnames(R.men) <- c('Effort (%)', 'Tournaments')

#Use D.Complete to add the Rank associated by effort
R.men %<>% join(D.complete[,c(1,2)], by = "Effort (%)") 

R.men$GENDER <- 'MEN'
R.men %<>% .[,2:4] 
colnames(R.men) <- c('Tournaments', 'Ranking', 'GENDER')

R.men %<>% subset(!is.na(Ranking)) 


R.women <- subset(RANK_WOMEN, select = c('Ranking', 'Tournaments')) 
R.women$GENDER <- 'WOMEN'

Rank.obs.st <- rbind(R.women, R.men) 
Rank.obs.st$Ranking %<>% as.numeric 
Rank.obs.st$Tournaments %<>% as.numeric 


T1 <- ggplot(data = Rank.obs.st, aes(x = Ranking, y = Tournaments, color =GENDER)) +
  geom_point() +geom_smooth(method = 'gam') +
  scale_colour_manual(values=c( '#0072B2', "#D55E00")) + 
  labs( y = "No. Tournaments", x = 'Ranking', 
        title = 'Tournaments played according to the ranking') +
  scale_x_continuous(breaks = c(0,100, 215), labels = c('1', '', 'Last')) +
  #scale_y_continuous(breaks = c(0,12.1, 13.9), ) +
  theme(plot.title=element_text(hjust=.5))


T1
#Now lets get the lm formula to see the relationship

Rank.obs.st$Rank.Inv <- max(Rank.obs.st$Ranking) - Rank.obs.st$Ranking  

Obs.W <- subset(Rank.obs.st, GENDER == 'WOMEN')
Obs.M <- subset(Rank.obs.st, GENDER == 'MEN')

W.c <- lm(Tournaments~ 0 + Rank.Inv, Obs.W)$coefficients
M.c <- lm(Tournaments~ 0 + Rank.Inv, Obs.M)$coefficients

Tournamnet.Rel <- 1 - W.c/M.c

#

library(png)
library(grid)
img <- readPNG('https://upload.wikimedia.org/wikipedia/en/8/8e/Logo_of_PSA_2015.jpg')
g <- rasterGrob(img, interpolate=TRUE)

qplot(1:10, 1:10, geom="blank") +
  annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) +
  geom_point()
