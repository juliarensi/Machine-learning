
# carrega as bibliotecas
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

## Máquina de Vetor se Suporte (SVM) - separa dados de CYL alto e baixo
CARROS_SVM_CLASS <- train(cyl ~ mpg + disp + drat + wt + qsec, data = treinoCarros, method = "svmLinear", trControl = train.control)
CARROS_SVM_CLASS # sumário da máquina de vetor de suporte
plot(varImp(CARROS_SVM_CLASS))

# criar a máquina de vetor de suporte
svmCarros = svm(cyl ~ mpg + disp + drat + wt + qsec, data = treinoCarros, cost = 10, scale = F)
svmCarros
plot(svmCarros, treinoCarros, disp ~ mpg)

## Árvore de Decisão
CARROS_RPART_CLASS <- train(cyl ~ mpg + disp + drat + wt + qsec, data = treinoCarros, method = "rpart", trControl = train.control)

summary(CARROS_RPART_CLASS)
fancyRpartPlot(CARROS_RPART_CLASS$finalModel) # desenho da árvore
plot(varImp(CARROS_RPART_CLASS)) # importância das variáveis

# Bagging com Floresta Aleatória - paralelo sem transmissão da aprendizagem
CARROS_RF_CLASS <- train(cyl ~ mpg + disp + drat + wt + qsec, data = treinoCarros, method = "cforest", trControl = train.control)

plot(CARROS_RF_CLASS) # evolução do modelo
plot(varImp(CARROS_RF_CLASS)) # plot de importância

# Boosting com Boosted Generalized Linear Model
CARROS_ADA_CLASS <- train(cyl ~ mpg + disp + drat + wt + qsec, data = treinoCarros, method = "glmboost", trControl = train.control)

plot(CARROS_ADA_CLASS) # evolução do modelo
print(CARROS_ADA_CLASS) # modelo
summary(CARROS_ADA_CLASS) # sumário

melhor_modelo <- resamples(list(SVM = CARROS_SVM_CLASS, RPART = CARROS_RPART_CLASS, RF = CARROS_RF_CLASS, ADABOOST = CARROS_ADA_CLASS))
melhor_modelo

summary(melhor_modelo)

#Até esse ponto estamos apenas avaliando a performance da aprendizagem da máquina (Experiência/dados de treino). Os próximos códigos vamos usar os dados de teste 

predVals <- extractPrediction(list(SVM = CARROS_SVM_CLASS, RPART = CARROS_RPART_CLASS, RF = CARROS_RF_CLASS, ADABOOST = CARROS_ADA_CLASS), testX = testeCarros[, c(1, 3:7)], testY = testeCarros$cyl) 

plotObsVsPred(predVals)
