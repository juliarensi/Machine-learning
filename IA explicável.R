
# carrega as bibliotecas
pacman::p_load(
  caret, corrplot, data.table, dplyr, fastDummies, ggplot2, SmartEDA, tidyverse)

#Vendo a base de dados
View(mtcars)

# preparação

particaoMtcars = createDataPartition(mtcars$cyl, p=.7, list = F) # cria a partição 70-30
treinoMtcars = mtcars[particaoMtcars, ] # treino
testeMtcars = mtcars[-particaoMtcars, ] # - treino = teste

Mtcars_formula <- cyl ~ mpg + disp + hp + drat + wt + qsec 

lista_modelos <- c('lm', 'glmnet', 'glmboost', 'rpart', 'cforest')

total_cv <- 10

train.control <- trainControl(method = "cv", number = total_cv, verboseIter = T) # controle de treino

pacman::p_load(caretEnsemble, doParallel)

registerDoParallel(cores = detectCores() - 1)

# modelagem

Mtcars_modelos <- caretList(
  Mtcars_formula, 
  data = treinoMtcars, 
  methodList = lista_modelos, 
  metric = "RMSE",
  trControl = train.control,
  tuneLength = 5)

pacman::p_load(DALEX, iml, pdp)

# importância

Mtcars_varImp <- varImp(Mtcars_modelos$cforest)

Mtcars_varImp_df <- as.data.frame(as.matrix(Mtcars_varImp$importance))

Mtcars_varImp_df <- Mtcars_varImp_df %>% mutate(
  variável = c('mpg', 'disp', 'hp', 'drat', 'wt', 'qsec')
)

grafico_varImp <- ggplot(data=Mtcars_varImp_df, aes(x=reorder(variável, -Overall), y=Overall)) + geom_bar(stat="identity", fill='#007095') + theme_minimal() + 
  coord_flip() + 
  labs(
    title  = ~ underline("Importância das variáveis usadas no modelo"), 
    subtitle = "Mtcars",
    caption = 'Modelo: Floresta Aleatória',
    x = '',
    y = 'Importância Relativa') + theme(
      plot.title = element_text(face = 'bold', lineheight = 1, size = 16, color = "#007095"),
      plot.subtitle = element_text(face = 'italic', size = 12, color = "#007095") ,
      plot.caption = element_text(size = 10, color = "#007095") ,
      strip.text = element_text(size = 10, color = "white") ,
      axis.title.x = element_text(hjust=0, color = "#007095"),
      axis.text.x = element_text(face = 'bold', colour = '#5bc0de', size = 12, angle = 75, vjust = .5),
      axis.title.y = element_text(hjust=0, color = "#007095"),
      axis.text.y = element_text(face = 'bold', colour = '#5bc0de', size = 12),
      legend.position="bottom", 
      legend.box = "horizontal",
      legend.background = element_rect(fill="#dee2e6", colour ="white")
    )

grafico_varImp

# perfil parcial

treinoMtcars_x <- dplyr::select(treinoMtcars, -cyl)
testeMtcars_x <- dplyr::select(testeMtcars, -cyl)

explainer_rf <- DALEX::explain(model = Mtcars_modelos$cforest, data = testeMtcars_x, y = testeMtcars$cyl, label = "Random Forest")

pdp_rf_mpg <- model_profile(explainer = explainer_rf, variables = "disp", groups = "mpg")

grafico_pdp <- plot(pdp_rf_mpg, geom = "profiles") +  
  labs(
    title  = ~ underline("Perfis de dependência parcial para DISP e MPG"), 
    subtitle = "Mtcars",
    caption = 'Modelo: Florestas Aleatórias',
    tag = '',
    x = 'DISP',
    y = 'CYL',
    colour = "DISP") + 
  scale_colour_manual(
    values = c('#f68d7c', '#8ecda6', 'blue'),
    name = "DISP") + 
  theme(
    plot.title = element_text(face = 'bold', lineheight = 1, size = 16, color = "#007095"),
    plot.subtitle = element_text(face = 'italic', size = 12, color = "#007095") ,
    plot.caption = element_text(size = 10, color = "#007095") ,
    strip.text = element_text(size = 10, color = "white") ,
    axis.title.x = element_text(hjust=0, color = "#007095"),
    axis.text.x = element_text(face = 'bold', colour = '#5bc0de', size = 12),
    axis.title.y = element_text(hjust=0, color = "#007095"),
    axis.text.y = element_text(face = 'bold', colour = '#5bc0de', size = 12),
    legend.position="bottom", 
    legend.box = "horizontal",
    legend.background = element_rect(fill="#dee2e6", colour ="white")
  )

grafico_pdp
