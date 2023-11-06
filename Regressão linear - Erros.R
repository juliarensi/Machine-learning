
# carrega as bibliotecas
pacman::p_load(ade4, car, caret, corrplot, data.table, doParallel, dplyr, fastDummies, forcats, ggplot2, mlbench, mltools, randomForest, rattle, tidyverse)

# leitura da base de dados
ENEM_ESCOLA_2019 <- read.csv2('https://raw.githubusercontent.com/hugoavmedeiros/etl_com_r/master/bases_tratadas/ENEM_ESCOLA_2019.csv', stringsAsFactors = T) # carregando a base já tratada para o ambiente do R

ENEM_ESCOLA_2019 <- ENEM_ESCOLA_2019 %>% dummy_cols()

names(ENEM_ESCOLA_2019)[17:28] <- c(
  'EREM', 'ETE', 'Federal', 'Privada', 'Regular', 'Rural', 'Urbana', 'ICG2', 'ICG3', 'ICG4', 'ICG5', 'ICG6'
)

corrplot(cor(ENEM_ESCOLA_2019[ , c(6:28)])) # correlação entre as variáveis

# Treino e Teste: Pré-processamento
particaoENEM = createDataPartition(ENEM_ESCOLA_2019$nota, p=.7, list = F) # cria a partição 70-30
treinoENEM = ENEM_ESCOLA_2019[particaoENEM, ] # treino
testeENEM = ENEM_ESCOLA_2019[-particaoENEM, ] # - treino = teste

# Validação Cruzada: Pré-processamento
# Controle de treinamento
train.control <- trainControl(method = "cv", number = 100, verboseIter = T) # controle de treino

ENEM_formula <- nota ~ TDI_03 + MHA_03 + REP_EM + EREM + ETE + Federal + Privada + Regular +  Rural + Urbana

registerDoParallel(cores = detectCores() - 1)

# Treinamentos
## Regressão Linear penalizada
ENEM_LM <- train(
  ENEM_formula, 
  data = treinoENEM, 
  method = "glmnet", 
  trControl = train.control, tuneLength = 20)

plot(ENEM_LM)
summary(ENEM_LM) # sumário do modelo linear
plot(varImp(ENEM_LM))

coeficientes <- coef(ENEM_LM$finalModel, ENEM_LM$bestTune$lambda)

## Árvore de Decisão
ENEM_RPART <- train(
  ENEM_formula, 
  data = treinoENEM, 
  method = "rpart", 
  trControl = train.control, tuneLength = 20)

plot(ENEM_RPART)
summary(ENEM_RPART)
fancyRpartPlot(ENEM_RPART$finalModel) # desenho da árvore
plot(varImp(ENEM_RPART)) # importância das variáveis

# Bagging com Floresta Aleatória
ENEM_RF <- train(
  ENEM_formula, 
  data = treinoENEM, 
  method = "cforest", 
  trControl = train.control, tuneLength = 20)

plot(ENEM_RF) # evolução do modelo
plot(varImp(ENEM_RF)) # plot de importância

# Boosting com Boosted Generalized Linear Model
ENEM_GLMB <- train(
  ENEM_formula, 
  data = treinoENEM, 
  method = "glmboost", 
  trControl = train.control, tuneLength = 20)

plot(ENEM_GLMB) # evolução do modelo
print(ENEM_GLMB) # modelo
summary(ENEM_GLMB) # sumário

# Boosting com Boosted Generalized Linear Model
ENEM_ADA_CLASS <- train(nota ~ EREM + ETE + Federal + Privada + Regular + TDI_03 + MHA_03, data = treinoENEM, method = "glmboost", trControl = train.control)

plot(ENEM_ADA_CLASS) # evolução do modelo
print(ENEM_ADA_CLASS) # modelo
summary(ENEM_ADA_CLASS) # sumário

melhor_modelo <- resamples(list(LM = ENEM_LM, RPART = ENEM_RPART, RF = ENEM_RF, GLMBOOST = ENEM_GLMB))
melhor_modelo

summary(melhor_modelo)

predVals <- extractPrediction(ENEM_RF, testX = testeENEM[ , -7]) 

pred_modelos <- data.frame(
  obs = testeENEM$nota,
  rf = predict(ENEM_RF, testeENEM)
) %>% mutate (rf_res = obs - rf)

ggplot(pred_modelos, aes(obs, rf)) + 
  geom_point() + geom_smooth()

ggplot(pred_modelos, aes(rf, rf_res)) + 
  geom_point() + geom_hline(yintercept = 0, color = "red")

####################
pacman::p_load(caretEnsemble, doParallel)

registerDoParallel(cores = detectCores() - 1)

lista_modelos <- c('glmnet', 'rpart', 'cforest', 'glmboost')

total_cv <- 100

train.control <- trainControl(method = "cv", number = total_cv, verboseIter = T) # controle de treino

ENEM_MELHOR_MODELO <- train(
  ENEM_formula, 
  data = treinoENEM, 
  methodList = lista_modelos, 
  metric = "RMSE",
  trControl = train.control)

ENEM_MELHOR_MODELO

ENEM_MODELOS <- caretList(
  ENEM_formula, 
  data = treinoENEM, 
  methodList = lista_modelos, 
  metric = "RMSE",
  trControl = train.control,
  tuneLength = 10)

ENEM_MODELOS

lista_resultados <- lapply(
  lista_modelos, 
  function(x) {ENEM_MODELOS[[x]]$resample})

df_resultados <- do.call("bind_rows", lista_resultados)

df_resultados <- df_resultados %>% mutate(
  Modelo = lapply(lista_modelos, function(x) {rep(x, total_cv)}) %>% unlist())

grafico_resultados <- df_resultados %>% 
  select(RMSE, Modelo) %>% 
  ggplot(aes(Modelo, RMSE, fill = Modelo, color = Modelo)) + 
  geom_boxplot(show.legend = FALSE, alpha = 0.3) + 
  theme_minimal() + 
  coord_flip()
plotly::ggplotly(grafico_resultados)

df_resultados %>% 
  select(RMSE, Modelo) %>% 
  group_by(Modelo) %>% 
  summarise_each(funs(min, max, median, mean, sd, n()), RMSE) %>%
  arrange(-mean) %>% 
  mutate_if(is.numeric, function(x) {round(x, 3)}) %>% 
  knitr::kable()

df_resultados %>% 
  select(Rsquared, Modelo) %>% 
  group_by(Modelo) %>% 
  summarise_each(funs(min, max, median, mean, sd, n()), Rsquared) %>%
  arrange(-mean) %>% 
  mutate_if(is.numeric, function(x) {round(x, 3)}) %>% 
  knitr::kable()


# MAE = Mean absolute error (Erro médio absoluto) - Média dos módulos dos resíduos
# RMSE = Root Mean Squared Error (Raiz quadrada do erro-médio) - Média da raiz quadrada do quadrado do resíduo
# Semelhanças
# Mesma escala da variável de interesse
# Quanto menor melhor (orientadas negativamente)
# Diferenças
# RMSE capta melhor a variância dos erros
# MAE é mais simples de interpretar

plot(erros1 <- c(rep(2,10)))
plot(erros2 <- c(rep(1,5), rep(3,5)))
plot(erros3 <- c(rep(0,8), rep(10,2)))
lista_erros <- matrix(c(sum(erros1)/10,sum(erros2)/10, sum(erros3)/10, sqrt(sum(erros1^2)/10), sqrt(sum(erros2^2)/10), sqrt(sum(erros3^2)/10)), ncol = 2, dimnames = list(c('Erros 1', 'Erros 2', 'Erros 3'), c('MAE', 'RMSE')))
lista_erros

# Rsquared ou R² = Coeficiente de Determinação - quantidade da variância que é explicada pelo modelo. 
# Varia entre 0 e 1
# Quanto maior melhor (orientada positivamente)

melhor_modelo <- resamples(list(LM = ENEM_LM, RPART = ENEM_RPART, RF = ENEM_RF, ADABOOST = ENEM_ADA_CLASS))
melhor_modelo

summary(melhor_modelo)