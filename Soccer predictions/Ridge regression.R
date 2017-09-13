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


#Add the  class variable
DATA %<>% mutate(
  Diferencia = Goles_Home - Goles_Away,
  Resultado_Binario = ifelse(Diferencia>0, 'Home', 'No_Home'), 
  Res_Binario_num = ifelse(Diferencia>0, 1, 0) %>% as.numeric,
  League_Esp = ifelse(League=='La Liga',1,0) %>% as.numeric
) 

DATA_MODEL <-  DATA %>% select(-id, - Date, -Resultado_Binario, 
                               -contains('Goles'), -Diferencia, 
                               -contains('Amarillas_Away'), -League)

#2.- Adjust the model

y <- DATA_MODEL$Res_Binario_num %>% as.matrix
x <- DATA_MODEL %>% select(-Res_Binario_num) %>% as.matrix 
model <- cv.glmnet( x  , y, alpha = 0, family = 'binomial', intercept = FALSE)
lambda <- model$lambda.min
#lambda <- .5
Coeffs <- coef(model, s = lambda )[1:nrow(coef(model, s = lambda ))] %>% 
  data.frame

rownames(Coeffs) <- c('Intercept',colnames(x))
Coeffs %<>% t %>% data.frame 


size_train <- 3000

#Ridge and lasso regression

 
Percentage1 <- NULL
for(k in 1:100){
  
  sample_rows <- sample(x = 1:nrow(DATA_MODEL), size = size_train, replace = F)
  train <- DATA_MODEL[sample_rows,]
  test <- DATA_MODEL[-sample_rows,]
  
  y <- train$Res_Binario_num %>% as.matrix
  x <- train %>% select(-Res_Binario_num) %>% as.matrix 
  model <- cv.glmnet( x  , y, alpha = 0, family = 'binomial', 
                      intercept = FALSE)
  lambda <- model$lambda.min
  
  accuracy <- predict(model, s = lambda, newx = test %>%
                        select(-Res_Binario_num) %>% as.matrix, 
                      type ='response') %>% data.frame
  accuracy$Prediction <- 1
  accuracy$Prediction[accuracy$X1<.5] <- 0
  accuracy$class <- test$Res_Binario_num
  
  
  #Overall accuracy
  
  Overall <- mean(accuracy$Prediction==accuracy$class) %>% round(5) * 100
  Percentage1 %<>% c(Overall)
  
  Coeffs.i <- coef(model, s = lambda )[1:nrow(coef(model, s = lambda ))]
  
  Coeffs %<>%  rbind(Coeffs.i)

  print(Overall)
}

summary(Percentage1)

Coeffs %<>% data.frame

Coeffs.plot <- gather(Coeffs)

ggplot(data = Coeffs.plot, aes(x =key, y = value, group = key)) +
  geom_hline(yintercept = 0,colour ='red', linetype =2)+
  geom_boxplot() + coord_flip() 


