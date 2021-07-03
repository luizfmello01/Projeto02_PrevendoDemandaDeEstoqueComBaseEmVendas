### Parte 2: Manipulação de dados ###
# O objetivo dessa parte do script é transformar o dataset

# Remover valores outliers das variáveis numéricas
for(i in var.numericas) {
  qnt <- quantile(vendas[[i]], probs = c(.25, .75), na.rm = TRUE)
  H <- 1.5 * IQR(vendas[[i]], na.rm = TRUE)
  vendas[vendas[[i]] < (qnt[1] - H)] <- NA
  vendas[vendas[[i]] > (qnt[2] + H)] <- NA
}

vendas <- na.omit(vendas)


# Dividir os dados de treino e de teste
indexTest <- sample.int(nrow(vendas), 100000)
dados.test <- vendas[indexTest,]
dados.train <- vendas[-indexTest,]
rm(vendas)


# Conclusão: Fim da manipulação dos dados