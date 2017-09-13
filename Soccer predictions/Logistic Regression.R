#==============================================================================
#                         Logistic Regression
#==============================================================================

options(stringsAsFactors = F)
libs <- c("magrittr", "plyr", "ggplot2", "stringr", "lubridate","reshape2",
          "dplyr", 'glmnet', 'tidyr')
lapply(libs, require, character.only=T)

theme_set(theme_minimal())



#1.- Load the data ------------------------------------------------------------
DATA <- read.csv('Soccer predictions/Data.csv')

#Prepare the data
DATA %<>% mutate(
  Diferencia = Goles_Home - Goles_Away,
  Resultado_Binario = ifelse(Diferencia>0, 'Home', 'No_Home'), 
  Res_Binario_num = ifelse(Diferencia>0, 1, 0) %>% as.numeric,
  League_Esp = ifelse(League=='La Liga',1,0) %>% as.numeric
) 


#2.- Adjust the model to all the data

DATA_MODEL <-  DATA %>% select(-id, - Date, -Resultado_Binario,
                               -contains('Goles'), -Diferencia,
                               #-contains('Faltas'),
                              -contains('Amarillas_Away'),
                              -Faltas_Away,
                             -League)


model <- glm(Res_Binario_num ~ 0 +  . , family = binomial(link = 'logit'),
             data = DATA_MODEL )

summary(model)

#3.- Iterate the test and train 

size_train <- 3000

Perc.lm <- NULL
Coeffs <- model$coefficients
for(k in 1:400){
  
  sample_rows <- sample(x = 1:nrow(DATA_MODEL), size = size_train, replace = F)
  train <- DATA_MODEL[sample_rows,]
  test <- DATA_MODEL[-sample_rows,]
  
  model <- glm(Res_Binario_num ~ 0 + ., family = binomial(link = 'logit'),
               data = train)
  
  
  accuracy <- predict(model, test, type = 'response') %>% data.frame
  accuracy$Prediction <- 1
  accuracy$Prediction[accuracy$.<.5] <- 0
  accuracy$class <- test$Res_Binario_num
  
  #Overall accuracy
  
  Coeffs %<>%  rbind(model$coefficients)
  
  Overall <- mean(accuracy$Prediction==accuracy$class) %>% round(5) * 100
  Perc.lm %<>% c(Overall)
  cat(k, ' ')
}

hist(Perc.lm)
boxplot(Perc.lm)


#4.- Checking coefficients and feature selection

Coeffs %<>% data.frame

Coeffs.plot1 <- gather(Coeffs)

ggplot(data = Coeffs.plot1, aes(x =key, y = value, group = key)) +
  geom_hline(yintercept = 0,colour ='red', linetype =2)+
  geom_boxplot() + coord_flip() 

summary(Perc.lm)
