### Parte 1: Análise exploratória de dados ###
# O objetivo dessa parte do script é explorar os dados de treino

#### Preparando os dados para Análise exploratória ####

# Setar local de trabalho
setwd("C:/repos/Projeto02_PrevendoDemandaDeEstoqueComBaseEmVendas/Script_v2")
getwd()


# Carregar bibliotecas para essa parte do script
library(data.table)
library(dplyr)
library(ggplot2)
library(corrplot)


# Carregar o dataset
dados <- fread("../Datasets/train.csv", header = TRUE, sep = ",",
               encoding = "UTF-8")


# O dataset carregado é muito grande, Realizar split no dataset
set.seed(357123)
indexSplit <- sample.int(nrow(dados), 600000)
vendas <- dados[indexSplit,]


# Remover variáveis que não vão ser utilizadas
rm(dados, indexSplit)


# Visualizar dados
str(vendas)


# Verificar se existe valores NA
any(is.na(vendas))


# Renomear as variáveis do dataset
novosNomes <- c("Semana", "Agencia_ID", "Canal_ID",
                "Rota_ID", "Cliente_ID", "Produto_ID",
                "Unidade_Vendida", "Peso_Vendido", "Unidade_Devolucao",
                "Peso_Devolucao", "Demanda_Estoque")
colnames(vendas) <- novosNomes


# Alterar tipo de dado das variáveis categóricas
var.categoricas <- c("Semana", "Agencia_ID", "Canal_ID",
                     "Rota_ID", "Cliente_ID", "Produto_ID")

for(i in var.categoricas) {
  vendas[[i]] <- as.factor(vendas[[i]])
}


# Declarando as variáveis numéricas
var.numericas <- c(character(0))
for(i in colnames(vendas)) {
  if(is.numeric(vendas[[i]])) {
    var.numericas <- append(var.numericas, i)
  }
}



#### Análise exploratória das variáveis numéricas ####

# Medidas de tendencia central
for(i in var.numericas) {
  print(paste("Medidas de tendencia central da variável", i))
  print(summary(vendas[[i]]))
  cat("\n")
}

# Medidas de dispersão
for(i in var.numericas) {
  print(paste("Medidas de dispersão da variável", i))
  print(paste("Variância:", round(var(vendas[[i]]), 2)))
  print(paste("Desvio padrão:", round(sd(vendas[[i]]), 2)))
  cat("\n")
}

# Histogramas
for(i in var.numericas) {
  hist(vendas[[i]],
       main = paste("Histograma da variável", i),
       xlab = i)
}
# Conclusão: Analisando as frequencias, as variáveis não estão em uma 
# distribuição normal parece que temos muitos outliers, vamos verificar 
# melhor com boxplot.

# Boxplots
for(i in var.numericas) {
  boxplot(vendas[[i]],
       main = paste("Boxplot da variável", i),
       xlab = i)
}
# Conclusão: Muitos outliers em nossas variáveis.

# Análise de correlação (Plot de correlação das variáveis)
corrplot(cor(select(vendas, var.numericas)), method = "color")
# Conclusão: As variáveis que tem mais correlação com a variavel 
# target (Demanda_Estoque) é Unidade_Vendida e Peso_Vendido



#### Análise de dados por semana ####

# Preparar os dados para realizar a análise
vendas_semana <- vendas %>%
  group_by(Semana) %>%
  summarise(Unidade_Devolucao = sum(Unidade_Devolucao),
            Unidade_Vendida = sum(Unidade_Vendida),
            Peso_Vendido = sum(Peso_Vendido),
            Peso_Devolucao = sum(Peso_Devolucao),
            Demanda_Estoque = sum(Demanda_Estoque))

# Exibir tabela com os dados sumarizados por semana
print(vendas_semana)

# Gráfico de cada variável numérica por semana
for(i in var.numericas) {
    plot(ggplot(vendas_semana, aes(x = as.integer(Semana), y = .data[[i]])) +
      geom_line() +
      geom_point(color = "red") +
      ggtitle(paste(i, "por Semana")) +
      xlab("Semana"))
}
# Conclusão: 
# A semana com mais Unidades vendidas é a semana 4
# A semana com mais Pesos vendidos é a semana 8
# A semana com mais Unidades devolvidas é a semana 7
# A semana com mais Pesos devolvidos é a semana 7
# A semana com mais Demanda de estoque é a semana 4

# A semana 4 teve mais produtos vendidos que as demais e uma das que menos teve devoluções.



#### Análise de dados por produto ####

# Preparar os dados para realizar a análise
vendas_produto <- vendas %>%
  select(Produto_ID, Unidade_Vendida, Peso_Vendido,
         Unidade_Devolucao, Peso_Devolucao, Demanda_Estoque) %>%
  group_by(Produto_ID) %>%
  summarise(Unidade_Vendida = sum(Unidade_Vendida), 
            Peso_Vendido = sum(Peso_Vendido),
            Unidade_Devolucao = sum(Unidade_Devolucao), 
            Peso_Devolucao = sum(Peso_Devolucao), 
            Demanda_Estoque = sum(Demanda_Estoque))

# Exibir tabela com os dados sumarizados por produto
print(vendas_produto)

# Gráfico de cada variável numérica por produto
for(i in var.numericas) {
  # Ordenar o dataframe em ordem decrescente com a variável que está sendo 
  # iterada e pegar os 10 primeiros
  plot(ggplot(top_n(vendas_produto[order(-vendas_produto[[i]]),], 10),
         aes(Produto_ID, .data[[i]])) +
    geom_bar(stat = "identity") +
    ggtitle(paste("Top 10 produtos com mais", i)))
}
# Conclusão
# Produto 2425 com mais unidades vendidas
# Produto 2233 com mais peso vendido
# Produto 36610 com mais unidades devolvidas
# Produto 2233 com mais peso devolvido
# Produto 2425 com mais demanda de estoque

# Podemos concluir que o produto 2425 é o mais vendido, o produto 2233 é produto 
# que é mais vendido por peso, porém é o mais devolvido tambem e o produto 
# 36610 é o que tem mais unidades devolvidas e um dos menos vendidos.



#### Análise de dados por cliente ####

# Preparar os dados para realizar a análise
vendas_cliente <- vendas %>%
  select(Cliente_ID, Unidade_Vendida, Peso_Vendido,
         Unidade_Devolucao, Peso_Devolucao, Demanda_Estoque) %>%
  group_by(Cliente_ID) %>%
  summarise(Unidade_Vendida = sum(Unidade_Vendida), 
            Peso_Vendido = sum(Peso_Vendido),
            Unidade_Devolucao = sum(Unidade_Devolucao), 
            Peso_Devolucao = sum(Peso_Devolucao), 
            Demanda_Estoque = sum(Demanda_Estoque))

# Exibir tabela com os dados sumarizados por cliente
print(vendas_cliente)

# Gráfico de cada variável numérica por cliente
for(i in var.numericas) {
  # Ordenar o dataframe em ordem decrescente com a variável que está sendo 
  # iterada e pegar os 10 primeiros
  plot(ggplot(top_n(vendas_cliente[order(-vendas_cliente[[i]]),], 10),
              aes(Cliente_ID, .data[[i]])) +
         geom_bar(stat = "identity") +
         ggtitle(paste("Top 10 clientes com mais", i)))
}
# Conclusão: 653378 é o cliente que mais comprou e devolveu produtos.


# Remover variáveis que foram declaradas para fazer as análises
rm(vendas_cliente, vendas_produto, vendas_semana)
