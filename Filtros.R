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


#Tem que ter os números de telefone e email de preferência
# Removendo NAs de emails -------------------------------------------------

EmpresasContabilidade_CNPJ_6920601 <- EmpresasContabilidade_CNPJ_6920601 %>% filter(!is.na(email))


#Fazer os filtros de acordo com porte, Existencia de telefone, inicio da atividade e depois localização

x<-EmpresasContabilidade_CNPJ_6920601 %>%
  dplyr::filter(porte == c('3', '5'))


# Só números de telefone que fazem sentido

x<-x %>%
  dplyr::filter(telefone1 != c('-'))


# export const portes = [
#   "Não informado",1
#   "Micro empresa",2
#   "Pequeno porte",3
#   "Mei",4
#   "Demais",5
# ];
# 

# Filtro por cidade -------------------------------------------------------


x<-x %>%
  dplyr::filter(cidade == c('6001'))


x %>% write.csv("EmpresasContabilidade3_5_RioCOd6001.csv")



