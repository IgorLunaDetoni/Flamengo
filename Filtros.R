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
x<- EmpresasContabilidade_CNPJ_6920601

#Fazer os filtros de acordo com porte, Existencia de telefone, inicio da atividade e depois localização
# export const portes = [
#   "Não informado",1
#   "Micro empresa",2
#   "Pequeno porte",3
#   "Mei",4
#   "Demais",5
# ];
# CASO PRECISE RETIRAR PORTE
# x<-x %>%
#   dplyr::filter(porte == c('3', '5', '4', '5', '1'))


# Só números de telefone que fazem sentido

x<-x %>%
  dplyr::filter(telefone1 != c('-'))


#Substituindo NULL por NA
x$razao_social <- ifelse(x$razao_social=='NULL', NA, x$razao_social)
x$email <- ifelse(x$email=='NULL', NA, x$email)

# Se os valores das colunas razao social e nome fantasia forem NA remover a coluna
x <- x[!(is.na(x$razao_social) & is.na(x$nome_fantasia)), ]
x <- x[complete.cases(x$email),]
# Filtro por cidade -------------------------------------------------------


# x<-x %>%
#   dplyr::filter(cidade == c('6001'))
# 
 x<-x %>%
   dplyr::filter(matriz_filial == 1)


# Rankear  ----------------------------------------------------------------

x$rank <- ifelse(x$cidade=='6001'& x$porte %in% c('3','5'), 1,
                 ifelse(x$cidade != '6001' & !x$porte %in% c('3', '5'), 3, 2))

x<-x %>%
  dplyr::filter(lat != 0)

# Removendo emails duplicados ---------------------------------------------

unicos <- x %>% dplyr::distinct(email,.keep_all = TRUE)


# Removendo emails claramente errados -------------------------------------

# List of strings to be removed
strings_to_remove <- c("0000", "&8203MATHEUSSILVALUAN3@GMAIL.COM", "string3")  # Replace with your strings


unicos <- unicos[!(unicos$email %in% strings_to_remove), ]



# Remover participante de edital, cidade, matriz filial, porte

x<- x %>% select(-c(participante_edital,matriz_filial,cidade, porte))
x<- x %>% select(-c(cod_nat_juridica))

unicos %>% write.csv2("EmpresasContabilidade3_5_RioCOd6001.csv", sep = ";")


library(openxlsx)

# Save dataframe to an Excel file
write.xlsx(unicos, file = "EmpresasContabilidade3_5_RioCOd6001.xlsx", rowNames = FALSE)
