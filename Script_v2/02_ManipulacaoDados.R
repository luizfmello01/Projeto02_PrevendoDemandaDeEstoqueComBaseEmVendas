### Parte 2: Manipulação de dados ###
# O objetivo dessa parte do script é transformar o dataset

# Remover valores outliers das variáveis numéricas
for(i in var.numericas) {
  maiorOutlier <- mean(vendas[[i]]) + (sd(vendas[[i]]) * 2)
  menorOutlier <- mean(vendas[[i]]) - (sd(vendas[[i]]) * 2)
  vendas[[i]][vendas[[i]] < menorOutlier] <- NA
  vendas[[i]][vendas[[i]] > maiorOutlier] <- NA
}

vendas <- na.omit(vendas)


# Transformar variáveis categóricas para numericas novamente
# Motivo: Modelo fica muito pesado com variáveis fatores
for(i in var.categoricas) {
  vendas[[i]] <- as.integer(vendas[[i]])
}


# Dividir os dados de treino e de teste
indexTest <- sample.int(nrow(vendas), 100000)
dados.test <- vendas[indexTest,]
dados.train <- vendas[-indexTest,]
rm(vendas)


# Conclusão: Fim da manipulação dos dados