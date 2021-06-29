### Parte 1: Análise exploratória de dados ###
# O objetivo dessa parte do script é explorar os dados de treino


# Setar local de trabalho
setwd("C:/repos/Projeto02_PrevendoDemandaDeEstoqueComBaseEmVendas/Script_v2")
getwd()


# Carregar bibliotecas para essa parte do script
library(data.table)
library(dplyr)
library(ggplot2)


# Carregar o dataset
dados <- fread("../Datasets/train.csv", header = TRUE, sep = ",",
               encoding = "UTF-8")


# O dataset carregado é muito grande, Realizar split no dataset
indexSplit <- sample.int(nrow(dados), 200000)
vendas <- dados[indexSplit,]


# Remover variáveis que não vai ser utilizadas
rm(dados, indexSplit)


# Visualizar dados
str(vendas)


# Renomear as variáveis do dataset
novosNomes <- c("Semana", "Agencia_ID", "Canal_ID",
                "Rota_ID", "Cliente_ID", "Produto_ID",
                "Unidade_Vendida", "Peso_Vendido", "Unidade_Devolucao",
                "Peso_Devolucao", "Demanda_Estoque")
colnames(vendas) <- novosNomes


# Declarando as variáveis numéricas do dataset
var.numericas <- c("Unidade_Vendida", "Peso_Vendido", "Unidade_Devolucao",
                   "Peso_Devolucao", "Demanda_Estoque")


## Análise dos dados por semana
vendas_semana <- vendas %>%
  group_by(Semana) %>%
  summarise(Unidade_Devolucao = sum(Unidade_Devolucao),
            Unidade_Vendida = sum(Unidade_Vendida),
            Peso_Vendido = sum(Peso_Vendido),
            Peso_Devolucao = sum(Peso_Devolucao),
            Demanda_Estoque = sum(Demanda_Estoque)) %>%
  ungroup()

# Exibir tabela com os dados sumarizados por semana
print(vendas_semana)

# Plot de cada variável numérica por semana
for(i in  var.numericas) {
  plot(ggplot(vendas_semana, aes(Semana, .data[[i]])) +
         geom_line() + 
         ggtitle(paste(i, "por Semana")))
}