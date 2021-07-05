### Parte 4: Feature selection e treinar modelo preditivo ###
# O objetivo dessa parte do script é avaliar e testar o modelo treinado

# Visualizar modelo
summary(modelo_v1)


# Resíduos do modelo preditivo
residuos <- residuals(modelo_v1)
summary(residuos)
# Conclusão: Mediana do residuo, muito próximo de zero, ou seja o modelo
# aprendeu uma boa relação com os dados

previsoes <- predict(modelo_v1, 
                     dados.test[,c("Canal_ID", "Rota_ID", "Unidade_Vendida", 
                                   "Peso_Vendido", "Unidade_Devolucao", 
                                   "Peso_Devolucao")])

# Criar datatable com resultados para calcular os residuos
data.resultados <- data.frame(real = dados.test$Demanda_Estoque,
                              previsoes = round(previsoes,0))

# Remover os valores negativos das previsoes
data.resultados$previsoes <- sapply(data.resultados$previsoes,
                                    function(x) ifelse(x < 0, 0, x))

# Calcular os residuos das previsoes
data.resultados <- data.resultados %>%
  mutate(residuo = previsoes - real)


# Visualizar residuos das previsoes
summary(data.resultados$residuo)
# Conclusão: Modelo preditivo aprendeu muito bem com os dados e está apto para
# realizar previsões com novos dados