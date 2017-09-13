#==============================================================================
#                                    KNN
#==============================================================================

options(stringsAsFactors = F)
libs <- c("magrittr", "plyr", "ggplot2", "stringr", "lubridate","reshape2",
          "dplyr", 'glmnet', 'tidyr', 'class')
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

## Convert the dependent var to factor. Normalize the numeric variables  

DATA_MODEL$Res_Binario_num %<>% factor

num.vars <- sapply(DATA_MODEL, is.numeric)
DATA_MODEL[num.vars] <- lapply(DATA_MODEL[num.vars], scale)

#Subset for the independent variables
DATA_MODEL.subset <- select(DATA_MODEL,-Res_Binario_num)


#3.- Iterate the test and train 

size_train <- 3000

Perc.lm <- NULL
for(k in 1:100){

  sample_rows <- sample(x = 1:nrow(DATA_MODEL.subset), size = size_train, replace = F)
  train <- DATA_MODEL.subset[sample_rows,]
  test <- DATA_MODEL.subset[-sample_rows,]
  
  train_cl <- DATA_MODEL$Res_Binario_num[sample_rows]
  test_cl <- DATA_MODEL$Res_Binario_num[-sample_rows] 
  
  model <- knn(train = train, test = test, cl =  train_cl, k = 100 )

  Overall <- 100 * sum(model == test_cl)/length(model)
  Perc.lm %<>% c(Overall)
  cat(k, ' ')
}

hist(Perc.lm)
boxplot(Perc.lm)
summary(Perc.lm)
