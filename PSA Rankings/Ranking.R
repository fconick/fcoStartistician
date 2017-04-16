options(stringsAsFactor = F)
options(warn = F)
library(plyr)
library(magrittr)
library(ggplot2)

#1.- LOAD THE DATA AND CLEAN IT ------------------------------------------------

#Women -

ranking <- read.delim("C:/Users/fco_n/Desktop/ranking.csv", sep='\t')
ranking[1,2:3] <- ""
ranking <- ranking[-grep('Flag', ranking$Rank),]

RANK <- list(NULL)
for(i in 1:(dim(ranking)[1]/3) ){
  
  #print(i)
  RANK[[i]] <- ranking[c((1 + (3 * (i-1) )):(3 + (3 * (i-1) ))), ]
  
}

RANK_WOMEN <- ldply(RANK, function(x){
  y <- as.numeric(as.character(x[1,1]))
  y[2]<- as.character(x[2,1])
  y[3]<- as.numeric(as.character(x[3,1]) )
  y[4]<- as.numeric(as.character(x[3,2]))
  y[5]<- as.numeric(as.character(x[3,3]))
  y
})

colnames(RANK_WOMEN) <- c("Ranking", "Name", "Tournaments", "Points", "Avg")


#Men 

ranking <- read.delim("C:/Users/fco_n/Desktop/ranking_men.csv", sep='\t')
ranking[1,2:3] <- ""
ranking <- ranking[-grep('Flag', ranking$Rank),]

RANK <- list(NULL)
for(i in 1:(dim(ranking)[1]/3) ){
  
  #print(i)
  RANK[[i]] <- ranking[c((1 + (3 * (i-1) )):(3 + (3 * (i-1) ))), ]
  
}


RANK_MEN <- ldply(RANK, function(x){
  y <- as.numeric(as.character(x[1,1]))
  y[2]<- as.character(x[2,1])
  y[3]<- as.numeric(as.character(x[3,1]) )
  y[4]<- as.numeric(as.character(x[3,2]))
  y[5]<- as.numeric(as.character(x[3,3]))
  y
})

colnames(RANK_MEN) <- c("Ranking", "Name", "Tournaments", "Points", "Avg")

#Bind both databases
RANK_WOMEN$Gender <-  "WOMEN" 
RANK_MEN$Gender <-  "MEN" 

RANK <- rbind(RANK_MEN, RANK_WOMEN)
RANK[c(1,3:5)] <- sapply(RANK[c(1,3:5)], as.numeric) 

#Quick summary
dlply(RANK, .(Gender),summary)


#2.- Standarize the points ------------------------------------------------------

qplot(x=RANK$Tournaments, y=RANK$Ranking, color=RANK$Gender)

RANK <-  subset(RANK,  Points!=0)

Range <- ddply(RANK, .(Tournaments, Gender), function(x){
  quantile(x$Ranking, c(0,.25,.5,.75,1))
})

colnames(Range)[3:7] <- c('Min', 'Per', 'Ranking', 'Ter', 'Max')
  
theme_set(theme_bw())

plot.1 <- ggplot(data=Range, aes(x=Tournaments, y=Ranking, colour =Gender)) +
  geom_errorbar(aes(ymin=Per, ymax = Ter)) +
 geom_point() # +
# geom_point(aes(y=Min)) +
 # geom_point(aes(y=Max)) 


eqq <- ddply(Range, .(Tournaments), function(x){
  Men <- x$Ranking[1] 
  Women <- x$Ranking[2]
  c(Men = Men, Women=Women)
})


plot.f <- ggplot(data=eqq, aes(x=Men, y = Women)) + 
  geom_smooth(method='loess', na.rm = T)

plot.g <- ggplot(data=Range, aes(x=Tournaments, y=Ranking, colour =Gender))+
  geom_smooth(alpha=.1, size =.5, method='loess')   


#Get rid of the inactive players (tournamnets < 2 and Points =0)
RANK_Clean <- subset(RANK, Tournaments>1 & Points!=0)

RANK_Clean <- ddply(RANK_Clean, .(Gender), function(x){
  x$ST_Avg_Points <- (x$Avg / sum(x$Avg)) %>% round (5) *100 
  x
}) 

#Find equivalents

Rank_Women <- 10

Who <- subset(RANK_Clean, Gender=='WOMEN' & Ranking==Rank_Women)

Interval <- c(Who$ST_Avg_Points - .1 * Who$ST_Avg_Points , 
              Who$ST_Avg_Points + .1 * Who$ST_Avg_Points )

Who_Men <- subset(RANK_Clean, Gender=='MEN' & ST_Avg_Points>=Interval[1] &
                    ST_Avg_Points<=Interval[2] )


Who
Who_Men


Who$ST_Avg_Points


#Delete the garbage
RANK <- subset(RANK, Tournaments>4 & Points>0)



library(ggplot2)

ggplot(data=RANK, aes(y = Avg, x=Points))+
  geom_point(aes(colour=Gender)) 

ggplot(data=RANK, aes(y = Ranking, x=Tournaments))+
  geom_point(aes(colour=Gender)) 



RANK.Top <- subset(RANK, Ranking < 10 | Ranking %in% seq(30,100,10) )

plot.r <- ggplot(data=RANK.Top, aes(y = Avg  , x=Tournaments))+
  geom_point(aes(colour=Gender)) +
  geom_label(aes(label=Ranking, colour = Gender), 
                 label.padding = unit(0.15, "lines"), label.r = unit(0.15, "lines"),
                 label.size = .05)


#Standarize points 


Quantiles <- ddply(RANK, .(Gender), function(x){
  #x <- x[-1,]
  y <-  quantile(x$Avg, probs = seq(0,1,.02))
  y
})

Quantiles <- t(Quantiles[,-1]) %>% data.frame

ggplot(data=Quantiles, aes(x=X1, y=X2)) +geom_point()

Quantiles$MEN <- as.numeric(as.character(Quantiles$X1))
Quantiles$WOMEN <- as.numeric(Quantiles$X2)

Quantiles <- arrange(Quantiles, desc(MEN))
Quantiles$Q <- 1:nrow(Quantiles)


Q <- 49
Eq <- NULL
for(Q in 2:50){

Sup <- Quantiles[which(Quantiles$Q==Q-1),'WOMEN'] 
Infe <-  Quantiles[which(Quantiles$Q==Q),'WOMEN'] 
W <- subset(RANK, Gender=="WOMEN" & Avg <=Sup & Avg>= Infe)$Ranking[1]

SupM <- Quantiles[which(Quantiles$Q==Q-1),'MEN'] 
InfeM <-  Quantiles[which(Quantiles$Q==Q),'MEN'] 
M <- subset(RANK, Gender=="MEN" & Avg <=SupM & Avg>= InfeM)$Ranking[1]

Eq <- rbind(Eq,c(W,M))
}


Eq <- data.frame(Eq)
colnames(Eq) <- c('Women', "Men")
ggplot(Eq, aes(x=Men, y=Women)) + geom_smooth(method = 'glm' )

ggplot(data=Quantiles, aes(x=MEN, y=WOMEN)) + geom_text(aes(label=Q))



#Extra

RANK$Round <- floor(RANK$Ranking/100) 

prop.table(table(RANK$Round, RANK$Tournaments, RANK$Gender),margin = c(1,3))
