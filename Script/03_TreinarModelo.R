### Parte 3: Feature selection e treinar modelo preditivo ###
# O objetivo dessa parte do script é analisar as variáveis mais relevantes
# para o modelo preditivo e treina-lo

library(caret)
library(randomForest)

#### Feature selection ####
selecao_variaveis_v1 <- lm(Demanda_Estoque ~ .,
                        data = dados.train)

varImp(selecao_variaveis)
# Conclusão: Variáveis mais relevantes:
## Canal_ID, Rota_ID, Produto_ID, Unidade_Vendida, Peso_Vendido, Unidade_Devolucao,
## Peso_Devolucao

selecao_variaveis_v2 <- lm(Demanda_Estoque ~ Canal_ID
                           + Rota_ID
                           + Produto_ID
                           + Unidade_Vendida
                           + Peso_Vendido
                           + Unidade_Devolucao
                           + Peso_Devolucao,
                           data = dados.train)

varImp(selecao_variaveis_v2)
# Conclusão: Variáveis mais relevantes:
## Canal_ID, Rota_ID, Unidade_Vendida, Peso_Vendido, 
## Unidade_Devolucao, Peso_Devolucao



#### Construção do modelo preditivo ####
modelo_v1 <- train(Demanda_Estoque ~ Canal_ID
                   + Rota_ID
                   + Unidade_Vendida
                   + Peso_Vendido
                   + Unidade_Devolucao
                   + Peso_Devolucao,
                   data = dados.train, method = "lm",
                   preProcess = c("center", "scale"))

summary(modelo_v1)
# R-squared do modelo ficou com 99.98%