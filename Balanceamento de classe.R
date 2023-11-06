
##Balanceamento de classe

# carrega as bibliotecas
pacman::p_load(ade4, arules, car, caret, corrplot, data.table, dplyr, e1071, forcats, funModeling, ggplot2, mlbench, mltools, randomForest, rattle, tidyverse)

# leitura da base de dados pessoas vacinadas contra COVID 2023
Vacina_Covid23 <- read.csv("covid23.csv", stringsAsFactors = T)

# Treino e Teste: Pré-processamento
particaoCovid23 = createDataPartition(Vacina_Covid23$sexo, p=.7, list = F) # cria a partição 70-30
treinoCovid23 = Vacina_Covid23[particaoCovid23, ] # treino
testeCovid23 = Vacina_Covid23[-particaoCovid23, ] # - treino = teste

table(treinoCovid23$sexo)

# down / under
treinoCovid23_down <- downSample(x = treinoCovid23[, -ncol(treinoCovid23)], y = treinoCovid23$sexo)
table(treinoCovid23_down)   

# up
treinoCovid23_up <- upSample(x = treinoCovid23[, -ncol(treinoCovid23)], y = treinoCovid23$sexo)
table(treinoCovid23_up$sexo)  
 