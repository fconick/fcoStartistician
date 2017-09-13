#==============================================================================
#                         Tree Classifier
#==============================================================================

options(stringsAsFactors = F)
libs <- c("magrittr", "plyr", "ggplot2", "stringr", "lubridate","reshape2",
          "dplyr", 'glmnet', 'rpart', 'randomForest')
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
                               -contains('Goles'), -Diferencia)



size_train <- 3000

Perc.lm <- NULL
Coeffs <- model$coefficients
for(k in 1:400){
  
  sample_rows <- sample(x = 1:nrow(DATA_MODEL), size = size_train, replace = F)
  train <- DATA_MODEL[sample_rows,]
  test <- DATA_MODEL[-sample_rows,]
  
  model <- rpart(Res_Binario_num ~ . , data = DATA_MODEL, method = 'class')
  
  accuracy <- predict(model, test, type = 'class') %>% data.frame
  accuracy$class <- test$Res_Binario_num
  
  #Overall accuracy
  
  Coeffs %<>%  rbind(model$coefficients)
  
  Overall <- mean(accuracy$.==accuracy$class) %>% round(5) * 100
  Perc.lm %<>% c(Overall)
  cat(k, ' ')
}

hist(Perc.lm)
boxplot(Perc.lm)
summary(Perc.lm)


# -------------------------RANDOM FOREST -------------------------

DATA_MODEL$Res_Binario_num %<>% as.factor 


size_train <- 3000

  
  sample_rows <- sample(x = 1:nrow(DATA_MODEL), size = size_train, replace = F)
  train <- DATA_MODEL[sample_rows,]
  test <- DATA_MODEL[-sample_rows,]
  
  model <- randomForest(Res_Binario_num ~ . , data = train %>% select(-League)
                        , ntree=5000, do.trace = T)
  
  accuracy <- predict(model, test %>% select(-Res_Binario_num), type = 'class') %>% data.frame
  accuracy$class <- test$Res_Binario_num
  
  #Overall accuracy
  Overall <- mean(accuracy$.==accuracy$class) %>% round(5) * 100
  Overall


