library(readxl)
Cartórios <- read_excel("DataRawCont/Cartórios.xlsx")


x<-Cartórios %>%
  dplyr::filter(Latitudo != 0)

x<-x %>%
  dplyr::filter(`Matriz Filial` == 1)



# Emails e nome razão social ----------------------------------------------


# Removendo emails duplicados ---------------------------------------------

x <- x %>% dplyr::distinct(Email,.keep_all = TRUE)



# Removendo linhas que não tem Razao social e nome fantasia ---------------

x <- x[!(is.na(x$`Razao Social`) & is.na(x$`Nome Fantasia`)), ]
x <- x[complete.cases(x$Email),]




# Remover colunas ---------------------------------------------------------
 

x<- x %>% select(-c(`Matriz Filial`, `Capital Social`,Porte))

# Contando os estados--------------------------------------
count_by_value <- x %>%
  group_by(Estado) %>%
  summarise(Count = n())
View(count_by_value)

# Separando Estados -------------------------------------------------------

### RIO  ###
x <- x  %>%
  dplyr::filter(Estado == 'RJ')
write.xlsx(x, file = "Output/Cartorios_RJ.xlsx", rowNames = FALSE)

### DF ###
x <- x  %>%
  dplyr::filter(Estado == 'DF')
write.xlsx(x, file = "Output/Cartorios_DF.xlsx", rowNames = FALSE)


x <- x %>%  dplyr::filter(Estado != c('DF','RJ'))


write.xlsx(x, file = "Output/Cartorios_Sem_DF_RJ.xlsx", rowNames = FALSE)



write.xlsx(x, file = 'Output/Cartorios_TodosPosFiltros.xlsx', rowNames = FALSE)

