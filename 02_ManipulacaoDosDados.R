#### Parte 2: Manipulação dos dados ####


# Novos nomes para as variáveis
novos_nomes <- c("Semana", "Agencia_ID", "Canal_ID", "Rota_ID", "Cliente_ID",
                 "Produto_ID", "Quantidade_Vendida_Esta_Semana", "Peso_Vendido_Esta_Semana",
                 "Quantidade_Reservada_Proxima_Semana", "Peso_Reservado_Proxima_Semana",
                 "Demanda_Estoque", "ID")
colnames(vendas) <- novos_nomes


# Remover valores outliers
vendas_alt <- vendas[!(vendas$ID %in% datasets_outliers$id), -"ID"]


# Verificar sumário da variável target
summary(vendas_alt$Demanda_Estoque)
# A média da variável target, ficou mais próxima da mediana, já foram removidas os outliers

reserva <- vendas_alt

# Realizar normalização nas variáveis numéricas
normalizar <- function(x) {
  return( (x - min(x)) / (max(x) - min(x)) )
}

variaveis_numericas <- c("Quantidade_Vendida_Esta_Semana", "Peso_Vendido_Esta_Semana",
                         "Quantidade_Reservada_Proxima_Semana", "Peso_Reservado_Proxima_Semana",
                         "Demanda_Estoque")

for(i in variaveis_numericas) {
  vendas_alt[[i]] <- normalizar(vendas_alt[[i]])
}
# Resultado: Variáveis númericas foram normalizadas.


# Verificar se existe algum valor NA no dataframe
any(is.na(vendas_alt))
# Resultado: Não contem nenhum valor NA no dataframe.


# Finalizado manipulação dos dados