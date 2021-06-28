#### Parte 3: Feature Selection e Treinamento do modelo ####

# Feature selection das variáveis mais importantes
selecao_variaveis <- lm(Demanda_Estoque ~ ., 
             data = vendas_alt)

varImp(selecao_variaveis)
summary(selecao_variaveis)
rm(selecao_variaveis)

# Variáveis com mais importancia para o modelo
# Produto_ID, Quantidade_Vendida_Esta_Semana, Peso_Vendido_Esta_Semana, 
# Quantidade_reservada_Proxima_Semana, Peso_Reservado_Proxima_Semana


# Treinamento do modelo de regressão linear
modelo <- lm(Demanda_Estoque ~ Produto_ID
             + Quantidade_Vendida_Esta_Semana
             + Peso_Vendido_Esta_Semana
             + Quantidade_Reservada_Proxima_Semana
             + Peso_Reservado_Proxima_Semana,
             data = vendas_alt)

summary(modelo)
# R-squared do modelo ficou com 99.97%