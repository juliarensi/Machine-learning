

## QUI-QUADRADO COM R ##
# PRIMEIRO, VAMOS CARREGAR OS PACOTES
pacman::p_load(data.table, ggplot2, corrplot)

# AGORA, A BASE DE DADOS CAR EVALUATION #
breast_cancer <- fread('https://raw.githubusercontent.com/hugoavmedeiros/cp_com_r/master/bases_tratadas/breast_cancer.csv', stringsAsFactors = T)
breast_cancer <- fread('bases_tratadas/breast_cancer.csv', stringsAsFactors = T)

View(breast_cancer)

# TABELA DE CONTINGÊNCIA #
#Função table - frequencia que uma variável acontece de maneira cruzada#
breast_cancer_table <- table(breast_cancer$breast, breast_cancer$tumor_tamanho)
breast_cancer_table

# GRÁFICOS DE DISPERSÃO PAREADOS DAS VARIÁVEIS #
# aes = eixos #
ggplot(breast_cancer) + aes(x = tumor_tamanho, fill = breast) + geom_bar(position = "fill")

# TESTE QUI QUADRADO #
breast_cancer_test <- chisq.test(breast_cancer_table)
breast_cancer_test #Acima de 0,05 a gente não rejeita a H0, então NÃO há associação/dependência entre as variáveis#
breast_cancer_test$observed
breast_cancer_test$expected

#Qui-quadrado mede frequência observada (Com base nos dados) - frequência teórica esperada (estimação teórica)#

# CORRPLOT DAS VARIÁVEIS #
#Plotar correlação entre os resíduos. Valores específicos onde há associação#
corrplot(breast_cancer_test$residuals, is.cor = FALSE)
