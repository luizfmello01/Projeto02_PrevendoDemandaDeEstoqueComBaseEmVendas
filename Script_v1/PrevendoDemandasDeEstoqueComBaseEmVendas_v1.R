### Script V1 ###

#### Parte 1: Análise exploratória de dados numéricos ####

# Setar local de trabalho
setwd("C:/repos/Projeto02_PrevendoDemandaDeEstoqueComBaseEmVendas")
getwd()

# Carregar pacotes
library(data.table)
library(moments)
library(caret)
library(ggplot2)
library(dplyr)

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



#### Parte 2: Manipulação dos dados ####

# Novos nomes para as variáveis
novos_nomes <- c("Semana", "Agencia_ID", "Canal_ID", "Rota_ID", "Cliente_ID",
                 "Produto_ID", "Quantidade_Vendida_Esta_Semana", "Peso_Vendido_Esta_Semana",
                 "Devolucao_Unid_Proxima_Semana", "Devolucao_Peso_Proxima_Semana",
                 "Demanda_Estoque", "ID")
colnames(vendas) <- novos_nomes


# Remover valores outliers
vendas_alt <- vendas[!(vendas$ID %in% datasets_outliers$id), -"ID"]


# Verificar sumário da variável target
summary(vendas_alt$Demanda_Estoque)
# A média da variável target, ficou mais próxima da mediana, já foram removidas os outliers


# Separar dados de treino e de teste
indexesTrain <- createDataPartition(vendas_alt$Agencia_ID, p = 0.7, list = FALSE)
data.train <- vendas_alt[indexesTrain,]
data.test <- vendas_alt[-indexesTrain,]


# Verificar se existe algum valor NA no dataframe
any(is.na(vendas_alt))
# Resultado: Não contem nenhum valor NA no dataframe.


# Finalizado manipulação dos dados



#### Parte 3: Feature Selection e Treinamento do modelo ####

# Feature selection das variáveis mais importantes
selecao_variaveis <- lm(Demanda_Estoque ~ ., 
             data = data.train)

varImp(selecao_variaveis)
summary(selecao_variaveis)
rm(selecao_variaveis)

# Variáveis com mais importancia para o modelo
# Produto_ID, Quantidade_Vendida_Esta_Semana, Peso_Vendido_Esta_Semana, 
# Quantidade_reservada_Proxima_Semana, Peso_Reservado_Proxima_Semana


# Treinamento do modelo de regressão linear
#modelo <- lm(Demanda_Estoque ~ Produto_ID
#             + Quantidade_Vendida_Esta_Semana
#             + Peso_Vendido_Esta_Semana
#             + Devolucao_Unid_Proxima_Semana
#             + Devolucao_Peso_Proxima_Semana,
#             data = data.train)

modelo <- train(Demanda_Estoque ~ Produto_ID
                + Quantidade_Vendida_Esta_Semana
                + Peso_Vendido_Esta_Semana
                + Devolucao_Unid_Proxima_Semana
                + Devolucao_Peso_Proxima_Semana,
                data = data.train, method = "lm",
                preProcess = c("center", "scale"))

summary(modelo)
# R-squared do modelo ficou com 99.97%



#### Parte 4: Avaliação do modelo preditivo ####

str(data.train)

# Previsoes do modelo com os dados de teste
previsao <- as.integer(predict(modelo, data.test[, -"Demanda_Estoque"]))


# Criar dataframe com dados observados, previstos e residuo
residuos <- data.frame(previsoes = previsao,
                       valores_reais = data.test$Demanda_Estoque)

residuos <- residuos %>%
  mutate(residuo = valores_reais - previsoes)


# Sumário dos residuos e métricas do modelo
print(summary(residuos$residuo))
print(modelo$results)


# Valores reais x Previsoes
axisRange <- extendrange(c(residuos$valores_reais, residuos$previsoes))
plot(residuos$valores_reais, residuos$previsoes, 
     ylim = axisRange, xlim = axisRange,
     xlab = "Valores reais", ylab = "Valores previstos",
     main = "Valores reais x Previsoes")
abline(0, 1, col = "red", lty = 2)


# Salvar o modelo na pasta
saveRDS(modelo, "Modelo/Modelo_v1.rds")