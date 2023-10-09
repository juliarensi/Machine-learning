

### CARREGAR PACOTES
pacman::p_load(ccaPP, lsa, minerva, Rfast)

### CRIAR FUNÇÃO PARA RODAR VÁRIAS ASSOCIAÇÕES
multi.cor <- function(x, y) {
  c(
    cor = cor(x, y), # Correlação
    dcor = dcor(x, y), # Distance correlation
    cosine = cosine(x, y), # Distância do Cosseno 
    maxCor = maxCorProj(x, y), # Maximal correlation
    MIC = mine (x, y) #  Maximal Information Coefficient
  )
}

data("mtcars")

### Cor na base Mtcars
x <- mtcars$mpg
y <- mtcars$hp

plot(x, y) # Plotar o gráfico

corList <- multi.cor(x, y)
names(corList)
corList <- corList[c(1,5,6,7, 15)]
corList
