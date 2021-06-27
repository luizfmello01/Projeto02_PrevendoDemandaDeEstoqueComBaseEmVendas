## Parte 1: Análise exploratória de dados ##

# Setar local de trabalho
setwd("C:/repos/Projeto02_PrevendoDemandaDeEstoqueComBaseEmVendas")
getwd()

# Carregar pacotes
library(data.table)

# Carregar dataset
treino <- fread("Datasets/train.csv", header = TRUE,
                   sep = ",", stringsAsFactors = FALSE, encoding = "UTF-8")

# Split no dataset treino
set.seed(357951)
indexes <- sample.int(nrow(treino), 450000)
treino_sample <- treino[indexes,]
rm(treino)
rm(indexes)

# Visualizar o dataset
str(treino_sample)
summary(treino_sample)

# Verificar se tem dados faltante no dataset
any(is.na(treino_sample))
# Resultado: Não tem nenhum dado NA dentro do dataset.

# Análise das variáveis numéricas (Venta_uni_hoy, Venta_hoy, Dev_uni_proxima, Dev_proxima e Demanda_uni_equil)
variaveis_numericas <- c("Venta_uni_hoy", "Venta_hoy", 
                         "Dev_uni_proxima", "Dev_proxima", "Demanda_uni_equil")

# Exibir medidas de tendencia central e dispersão das variáveis numéricas
for( i in variaveis_numericas) {
  cat(paste("Medidas da variável ", i, ":", "\n", sep = ""))
  print(summary(treino_sample[[i]]))
  cat(paste("Desvio padrão:", sd(treino_sample[[i]])))
  cat("\n\n")
  boxplot(treino_sample[[i]], main = paste("Variável", i))
}
# Resultado: Variáveis não estão em uma distribuição normal e contem muitos outliers 


# Análise de correlação entre as variáveis
cor(treino_sample$Venta_uni_hoy, treino_sample$Venta_hoy)
# Resultado: As variáveis Venta, tem uma boa correlação positiva

cor(treino_sample$Dev_uni_proxima, treino_sample$Dev_proxima)
# Resultado: As variáveis Dev, tem uma boa correlação positiva

cor(treino_sample$Venta_uni_hoy, treino_sample$Demanda_uni_equil)
# Resultado: Variável venta_uni_hoy, tem uma forte correlação positiva com a variável target

cor(treino_sample$Venta_hoy, treino_sample$Demanda_uni_equil)
# Resultado: Variável venta_hoy, tem uma forte correlação positiva com a variável target

cor(treino_sample$Dev_uni_proxima, treino_sample$Demanda_uni_equil)
# Resultado: Variável dev_uni_proxima, tem uma pequena correlação positiva com a variável target

cor(treino_sample$Dev_proxima, treino_sample$Demanda_uni_equil)
# Resultado: Variável dev_proxima, tem uma pequena correlação positiva com a variável target