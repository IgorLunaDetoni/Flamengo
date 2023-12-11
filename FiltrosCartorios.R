library(readxl)
library(stringdist)

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
# View(count_by_value)

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





# Comparando os dados das duas tabelas ------------------------------------


x <- x %>%  dplyr::filter(Estado == c('RJ'))

TemplateCartorios <- read_excel("DataRawCont/TemplateCartorios.xlsx")


computeSimilarity <- function(text1, text2) {
  stringdist::stringdist(text1, text2, method = "lv")
}

similarities <- mapply(computeSimilarity, x$`Razao Social`, TemplateCartorios$Cartório)


# Add similarities to a new column in one of the dataframes
TemplateCartorios$similaridades_cartorio2 <- similarities






# Modelo de similaridade de levchstein ------------------------------------


#Função
checkSimilarity <- function(text1, text2, threshold = 0.99) {
  similarity <- stringdist::stringdist(text1, text2, method = "jaccard")
  similarity_score <- 1 - similarity / max(nchar(text1), nchar(text2))
  similarity_score >= threshold
}

# Identificando as colunas
similar_rows <- mapply(checkSimilarity, x$`Razao Social`, TemplateCartorios$Cartório)

# pegando os indicies
matching_indices <- which(similar_rows)

# Fazendo similaridade
print(x[matching_indices, ])
print(TemplateCartorios[matching_indices, ])




y <- TemplateCartorios[matching_indices, ]


write.xlsx(y, file = 'Output/Lista_de_Similaridades.xlsx', rowNames = FALSE)










