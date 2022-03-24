#INMET climates data - Statistics
#Climatic design information for Cooling & Dehumitification projects in Brazil
#Josué Santana
#Data source: https://portal.inmet.gov.br/dadoshistoricos

#Librarys
library(readr)
library(dplyr)
library(psychrolib)

#SI System for psychrolib library
SetUnitSystem("SI")

#Project folder
pasta <- getwd()

#Importando o primeiro csv como df principal
df_proj <- read.csv2(paste("Dados unificados - ", "2010", ".csv", sep = ""))

#Unificando os csv dos anos seguintes
for(i in 2011:2021)
{
  df_temp <- read.csv2(paste("Dados unificados - ", i, ".csv", sep = ""))
  df_proj <- dplyr::bind_rows(df_temp, df_proj) #Adds to the main data frame
}

#------------substituir valores -9999 por NA

#Verificando NAs no data.frame
sapply(df_proj, function(x) sum(is.na(x)))

#-------------Médias para umidade relativa com base na temperatura
medias_tempmax <- df_proj %>% group_by(temph_max, ID_estacao) %>% 
                      summarise(urph_max = round(mean(urph_max, na.rm = T),0))

medias_tempmin <- df_proj %>% group_by(temph_min, ID_estacao) %>% 
                      summarise(urph_min = round(mean(urph_min, na.rm = T),0))

#---------Incluir coluna mes

#----------Médias para a temperatura com base 
medias_mes <- df_proj %>% group_by(mes, ID_estacao) %>% 
                      summarise(patm_max = round(mean(patm_max, na.rm = T),1),
                                patm_min = round(mean(patm_min, na.rm = T),1),
                                temph_max = round(mean(temph_max, na.rm = T),1),
                                temph_min = round(mean(temph_min, na.rm = T),1))

#------------substituir valores NA por médias calculadas

#-----------Includes Wet Bulb Temperatures
#df_unif <- df_unif %>% mutate(tbu_max = round(GetTWetBulbFromRelHum(df_unif$temph_max, df_unif$urph_min/100, df_unif$patm_max*100),1),
#                              tbu_min = round(GetTWetBulbFromRelHum(df_unif$temph_min, df_unif$urph_max/100, df_unif$patm_min*100),1))

#--------------gerar estatísticas

#_____________________________________________
