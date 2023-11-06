
# Matriz de Confusão: matriz que relaciona as classes observadas (também chamadas de referência) e as classes preditas. Para melhor interpretação, oferece várias estatísticas auxiliares. Vamos ver as principais delas
# Accuracy (Acc) = Acuidade, ou performance geral do modelo - total de acertos, sem considerar nenhuma penalidade ou ajuste
# No Information Rate (NIR) = proporção da classe mais frequente - indica o quão a classe mais frequente está presente nos dados. É um valor de referência para compararmos com a acuidade, uma vez que o modelo poderia ganhar performance artificialmente aprendendo a sempre "chutar" na classe mais frequente. É oferecido também um teste de hipótese para verificar a hipótese de que a Acc é maior que o NIR. 
# Kappa = coeficiente kappa de Cohen - em geral, mede a concordância de duas classificações. No caso de ML, tem a ver com a estimativa de acuidade controlada pela possibilidade de classificação aleatória. Assim, permite saber se o modelo é bom, mesmo considerando a chance de "sortear" o resultado. 

#Chamando os pacotes
pacman::p_load(ade4, arules, car, caret, corrplot, data.table, dplyr, e1071, forcats, funModeling, ggplot2, mlbench, mltools, randomForest, rattle, tidyverse)

# leitura da base de dados
carros <- mtcars
view(mtcars)

# Discretização - Transforma um número em fator (classe) #
carros$cyl <- discretize(carros$cyl, method = "frequency", breaks = 2, labels = c("baixo", "alto"))

# Treino e Teste: Pré-processamento
particaoCarros = createDataPartition(carros$cyl, p=.7, list = F) # cria a partição 70-30
treinoCarros = carros[particaoCarros, ] # treino 
testeCarros = carros[-particaoCarros, ] # - treino = teste

# Validação Cruzada: Pré-processamento
# Controle de treinamento
train.control <- trainControl(method = "cv", number = 10, verboseIter = T) # controle de treino

## Árvore de Decisão
CARROS_RPART_CLASS <- train(cyl ~ mpg + disp + drat + wt + qsec, data = treinoCarros, method = "rpart", trControl = train.control)

summary(CARROS_RPART_CLASS)
fancyRpartPlot(CARROS_RPART_CLASS$finalModel) # desenho da árvore
plot(varImp(CARROS_RPART_CLASS)) # importância das variáveis

# Bagging com Floresta Aleatória - paralelo sem transmissão da aprendizagem
CARROS_RF_CLASS <- train(cyl ~ mpg + disp + drat + wt + qsec, data = treinoCarros, method = "cforest", trControl = train.control)

plot(CARROS_RF_CLASS) # evolução do modelo
plot(varImp(CARROS_RF_CLASS)) # plot de importância

## Matriz de confusão
predicaoCarros_RF_CLASS = predict(CARROS_RF_CLASS, testeCarros) # criar predição
cmCARROS_RF_CLASS <- confusionMatrix(predicaoCarros_RF_CLASS, testeCarros$cyl)
cmCARROS_RF_CLASS
cmCARROS_RF_CLASS$table

# Expected Accuracy (AccE) = Acuidade Esperada = estimativa de acuidade "esperada", ou seja, uma acuidade mínima que poderia ser conseguida simplesmente "chutando" a classe de forma aleatória. 

gtBaixa <- cmCARROS_RF_CLASS$table[1]+cmCARROS_RF_CLASS$table[2]
gtAlta <- cmCARROS_RF_CLASS$table[3]+cmCARROS_RF_CLASS$table[4]

pdBaixa <- cmCARROS_RF_CLASS$table[1]+cmCARROS_RF_CLASS$table[3]
pdAlta <- cmCARROS_RF_CLASS$table[2]+cmCARROS_RF_CLASS$table[4]

gtTotal <- gtAlta + gtBaixa

estAcc <- (gtBaixa*pdBaixa/gtTotal^2)+(gtAlta*pdAlta/gtTotal^2)
estAcc

