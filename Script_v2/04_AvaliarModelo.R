### Parte 4: Feature selection e treinar modelo preditivo ###
# O objetivo dessa parte do script é avaliar e testar o modelo treinado

# Visualizar modelo
summary(modelo_v1)


# Resíduos do modelo preditivo
residuos <- residuals(modelo_v1)
summary(residuos)
# Conclusão: Mediana do residuo, muito próximo de zero, ou seja o modelo
# aprendeu uma boa relação com os dados