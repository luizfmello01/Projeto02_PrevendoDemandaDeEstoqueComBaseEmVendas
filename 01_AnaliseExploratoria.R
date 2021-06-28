#### Parte 1: Análise exploratória de dados numéricos ####

# Setar local de trabalho
setwd("C:/repos/Projeto02_PrevendoDemandaDeEstoqueComBaseEmVendas")
getwd()

# Carregar pacotes
library(data.table)
library(moments)

# Carregar dataset
vendas <- fread("Datasets/train.csv", header = TRUE,
                   sep = ",", stringsAsFactors = FALSE, encoding = "UTF-8")

# Split no dataset treino
set.seed(357951)
indexes <- sample.int(nrow(vendas), 500000)
vendas <- vendas[indexes,]
rm(indexes)

# Adicionar variável ID para auxiliar na remoção de outliers posteriormente
vendas$id <- 1:nrow(vendas)

# Visualizar o dataset
str(vendas)
summary(vendas)

# Verificar se tem dados faltante no dataset
any(is.na(vendas))
# Resultado: Não tem nenhum dado NA dentro do dataset.

# Análise das variáveis numéricas (Venta_uni_hoy, Venta_hoy, Dev_uni_proxima, Dev_proxima e Demanda_uni_equil)
variaveis_numericas <- c("Venta_uni_hoy", "Venta_hoy", 
                         "Dev_uni_proxima", "Dev_proxima", "Demanda_uni_equil")

# Exibir medidas de tendencia central e dispersão das variáveis numéricas
for( i in variaveis_numericas) {
  cat(paste("Medidas da variável ", i, ":", "\n", sep = ""))
  print(summary(vendas[[i]]))
  cat(paste("Desvio padrão:", sd(vendas[[i]])))
  cat("\n\n")
  boxplot(vendas[[i]], main = paste("Variável", i))
}
rm(i)
# Resultado: Variáveis não estão em uma distribuição normal e contem muitos outliers 


# Análise de correlação entre as variáveis
cor(vendas$Venta_uni_hoy, vendas$Venta_hoy)
# Resultado: As variáveis Venta, tem uma boa correlação positiva

cor(vendas$Dev_uni_proxima, vendas$Dev_proxima)
# Resultado: As variáveis Dev, tem uma boa correlação positiva

cor(vendas$Venta_uni_hoy, vendas$Demanda_uni_equil)
# Resultado: Variável venta_uni_hoy, tem uma forte correlação positiva com a variável target

cor(vendas$Venta_hoy, vendas$Demanda_uni_equil)
# Resultado: Variável venta_hoy, tem uma forte correlação positiva com a variável target

cor(vendas$Dev_uni_proxima, vendas$Demanda_uni_equil)
# Resultado: Variável dev_uni_proxima, tem uma pequena correlação positiva com a variável target

cor(vendas$Dev_proxima, vendas$Demanda_uni_equil)
# Resultado: Variável dev_proxima, tem uma pequena correlação positiva com a variável target


# Análise de simetria das variáveis
for(i in variaveis_numericas) {
  cat(paste("Simetria da variável ", i, ":", "\n", sep = ""))
  print(skewness(vendas[[i]]))
  cat("\n")
}
rm(i)
# Resultado: Todas as variáveis numéricas do dataset são assimetrica positiva.


# Análise de valores duplicados
for(i in variaveis_numericas) {
  cat(paste("Valores duplicados da variável ", i, ":", "\n", sep = ""))
  print(sum(duplicated(vendas[[i]])))
  cat("\n")
}
rm(i)


# Identificando outliers de cada variável
datasets_outliers <- data.frame()
for(i in variaveis_numericas) {
  cat(paste("Outliers da variável ", i, ":", "\n", sep = ""))
  metrica = mean(vendas[[i]]) + (sd(vendas[[i]]) * 2)
  dataset <- subset(vendas, vendas[[i]] > metrica)
  datasets_outliers <- rbind(datasets_outliers, dataset)
  print( nrow(dataset) )
  datasets_outliers <- unique(datasets_outliers)
  rm(dataset)
  rm(metrica)
  cat("\n")
}
rm(i)

# O Dataset tem valores outliers
print( paste("O dataset tem ", round((nrow(datasets_outliers)/nrow(vendas)*100), 2), "% de outliers", sep = "" ))
