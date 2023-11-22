library(readr)
library(dplyr)
library(tidyverse)



# Captura de dados --------------------------------------------------------

EmpresasContabilidade_CNPJ_6920601 <- read_csv("C:/Users/igor-/OneDrive/Área de Trabalho/Deepen/Flamengo/Dados_Contabilidade/EmpresasContabilidade_CNPJ_6920601.csv")

str(EmpresasContabilidade_CNPJ_6920601)



# Limpeza básica ----------------------------------------------------------

EmpresasContabilidade_CNPJ_6920601$porte[EmpresasContabilidade_CNPJ_6920601$porte == '01'] <- '1'

# Removendo Nulos
EmpresasContabilidade_CNPJ_6920601 <- EmpresasContabilidade_CNPJ_6920601[complete.cases(EmpresasContabilidade_CNPJ_6920601$porte), ]


#Fazer os filtros de acordo com porte, Existencia de telefone, inicio da atividade e depois localização

x<-EmpresasContabilidade_CNPJ_6920601 %>%
  dplyr::filter(porte == c('1'))






