
# pacotes
pacman::p_load(
  caret, ggplot2, plotly, rattle
)

#Carregando base
data(iris)

set.seed(3)

# Pré-processamento
particaoIris = createDataPartition(1:nrow(iris), p=0.7) # cria a partição 70-30
treinoIris = iris[particaoIris$Resample1, ] # treino
testeIris = iris[-particaoIris$Resample1, ] # - treino = teste

# Controle de treinamento
train.control <- trainControl(method = "cv", number = 50, verboseIter = T) # controle de treino

# Mineração e predição com Árvores de Decisão
## Árvore de Decisão
IRIS_RPART <- train(
  Species ~ .,
  data = treinoIris, 
  method = "rpart", 
  trControl = train.control,
  tuneGrid = expand.grid(cp = c(0.00362, runif(19, 0, 0.25)))
)

plot(ENEM_RPART)

fancyRpartPlot(ENEM_RPART$finalModel) # desenho da árvore

plot(varImp(IRIS_RPART)) # importância das variáveis

predicaoTree = predict(IRIS_RPART, newdata = testeIris)

postResample(testeIris[ , 7], predicaoTree) # teste de performance da Árvore Condicional

base_avaliacao <- data.frame(
  Observado = testeIris[ , 16],
  Predição = predicaoTree)

predicao_arvore <- base_avaliacao %>% 
  ggplot(aes(x=Observado, y=Predição)) + 
  geom_point() + # cria os pontos
  geom_smooth() # cria a curva de associação
ggplotly(predicao_arvore)