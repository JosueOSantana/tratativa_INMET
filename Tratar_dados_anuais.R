library(readr)
library(dplyr)
library(stringr)

pasta <- getwd()

#Define o ano e configura a pasta dos arquivos a serem unificados
ano <- "2020"
setwd(ano)

#Lista os arquivos csv de cada estação meteorológica
formato_arq <- "CSV"
lista <- dir(pattern=formato_arq)

#Define colunas de interesse
mant_col <- c(1,2,5,6,10,11,14,15)

#Gera o data frame inicial com o primeiro arquivo csv
df_unif <- data.table::fread(lista[1], skip=9, select = mant_col)

#Filtra e adiciona cada csv a um data frame principal
for(i in lista[2:length(lista)]){
  df_temp <- data.table::fread(i, skip=9, select = mant_col)
  df_temp <- df_temp %>% mutate(ID = i)
  df_unif <- dplyr::bind_rows(df_temp, df_unif)
}

#Renomeia as colunas e trata os valores NA, "" ou "-9999"
df_unif <- df_unif %>% 
  rename("data" = "V1",
         "hora" = "V2",
         "patm_max" = "V5",
         "patm_min" = "V6",
         "temph_max" = "V10",
         "temph_min" = "V11",
         "urph_max" = "V14",
         "urph_min" = "V15") %>%
  filter(patm_max != -9999) %>%
  filter(patm_min != -9999) %>%
  filter(temph_max != -9999) %>%
  filter(temph_min != -9999) %>%
  filter(urph_max != -9999) %>%
  filter(urph_min != -9999) %>%
  na.omit()

#Retorna a pasta principal e gera um arquivo csv tratado e unificado
setwd(pasta)
nome_arq <- paste("Dados unificados - ", ano, ".csv")
write.csv2(df_unif1, nome_arq, row.names = FALSE)
