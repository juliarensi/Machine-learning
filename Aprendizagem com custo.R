

##Aprendizagem custo

# carrega as bibliotecas
pacman::p_load(ade4, arules, car, caret, corrplot, data.table, dplyr, e1071, forcats, funModeling, ggplot2, mlbench, mltools, randomForest, rattle, tidyverse)

# leitura da base de dados pessoas vacinadas contra COVID 2023
Vacina_Covid23 <- read.csv("covid23.csv", stringsAsFactors = T)
view(Vacina_Covid23)

# Treino e Teste: Pré-processamento
particaoCovid23 = createDataPartition(Vacina_Covid23$sexo, p=.7, list = F) # cria a partição 70-30
treinoCovid23 = Vacina_Covid23[particaoCovid23, ] # treino
testeCovid23 = Vacina_Covid23[-particaoCovid23, ] # - treino = teste

table(treinoCovid23$sexo)

# Validação Cruzada: Pré-processamento
# Controle de treinamento
train.control <- trainControl(method = "cv", number = 10, verboseIter = T) # controle de treino

#Criando matriz de custo
matrizCusto <- matrix(c(0, 1000, 1, 0), ncol = 2)
matrizCusto

rownames(matrizCusto) <- levels(treinoCovid23$sexo)
colnames(matrizCusto) <- levels(treinoCovid23$sexo)
matrizCusto

COVID_RF_CLASS <- randomForest(sexo ~ faixa_etaria + raca_cor + municipio, data = treinoCovid23, method = "cforest", parms = list(loss = matrizCusto))
COVID_RF_CLASS

#Função nativa do caret
COVID_C5_CLASS <- train(sexo ~ faixa_etaria + raca_cor + municipio, data = treinoCovid23, method = "C5.0Cost", trControl = train.control)
CIVID_C5_CLASS

predicaoCOVID_RF_CLASS = predict(COVID_RF_CLASS, testeCovid23) # criar predição
cmCOVID_RF_CLASS <- confusionMatrix(predicaoCOVIDM_RF_CLASS, testeCovid23$sexo)
cmCOVID_RF_CLASS

predicaoCOVID_C5_CLASS = predict(COVID_C5_CLASS, testeCovid23) # criar predição
cmCOVID_C5_CLASS <- confusionMatrix(predicaoCOVID_C5_CLASS, testeCovid23$sexo)
cmCOVID_C5_CLASS