
## CORRELAÇÃO COM R ##
# PRIMEIRO, VAMOS CARREGAR OS PACOTES
pacman::p_load(corrplot, dplyr, ggplot2, data.table)


## Correlação variáveis numéricas ##

# BASE DE DADOS NOTAS ENEM PE 2019 #
enem_escola_pe_2019 <- fread('https://raw.githubusercontent.com/hugoavmedeiros/cp_com_r/master/bases_tratadas/ENEM_ESCOLA_2019.csv', dec = ',')

names(enem_escola_pe_2019)
glimpse(enem_escola_pe_2019)

# Cor distorção idade/série no 3º ano e nota do ENEM #
Cor_Enem <- cor.test(enem_escola_pe_2019$nota, enem_escola_pe_2019$TDI_03)
Cor_Enem

# P valor abaixo de 0.05 então há associação entre as variáveis #