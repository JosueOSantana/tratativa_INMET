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

#Importando o primeiro csv como df principal
df_proj <- read.csv2(paste("Dados unificados - ", "2010", ".csv", sep = ""))

#Unificando os csv dos anos seguintes
for(i in 2011:2021)
{
  df_temp <- read.csv2(paste("Dados unificados - ", i, ".csv", sep = ""))
  df_proj <- dplyr::bind_rows(df_temp, df_proj) #Adds to the main data frame
}

#NAs in data.frame
sapply(df_proj, function(x) sum(is.na(x)))

#RelHum mean values - Grouped by temperatures and weather station - ALL YEARS
medias_tempmin <- df_proj %>% group_by(temph_min, ID_estacao) %>% 
  summarise(urph_max = round(mean(urph_max, na.rm = T),0))

medias_tempmax <- df_proj %>% group_by(temph_max, ID_estacao) %>% 
  summarise(urph_min = round(mean(urph_min, na.rm = T),0))

#Includes dia_mes column
df_proj <- df_proj %>% 
  mutate(dia_mes = paste(lubridate::day(df_proj$dia), "/", lubridate::month(df_proj$dia), sep = ""))

#Temperatures mean values - Grouped by day and weather station - ALL YEARS
medias_mes <- df_proj %>% group_by(dia_mes, ID_estacao) %>% 
  summarise(patm_max = round(mean(patm_max, na.rm = T),1),
            patm_min = round(mean(patm_min, na.rm = T),1),
            temph_max = round(mean(temph_max, na.rm = T),1),
            temph_min = round(mean(temph_min, na.rm = T),1))

df_proj <- df_proj %>% ungroup () %>% droplevels(.)

#Replaces NA values by mean values - ALL YEARS
df_proj <- df_proj %>% left_join(medias_mes, by = c("dia_mes", "ID_estacao"), suffix = c("", ".y")) %>%
  mutate(patm_max = coalesce(patm_max, patm_max.y),
         patm_min = coalesce(patm_min, patm_min.y),
         temph_max = coalesce(temph_max, temph_max.y),
         temph_min = coalesce(temph_min, temph_min.y)) %>%
  left_join(medias_tempmin, by = c("temph_min", "ID_estacao"), suffix = c("", ".y")) %>%
  mutate(urph_max = coalesce(urph_max, urph_max.y)) %>%
  left_join(medias_tempmax, by = c("temph_max", "ID_estacao"), suffix = c("", ".y")) %>%
  mutate(urph_min = coalesce(urph_min, urph_min.y)) %>%
  mutate(patm_max.y = NULL,
         patm_min.y = NULL,
         temph_max.y = NULL,
         temph_min.y = NULL,
         urph_max.y = NULL,
         urph_min.y = NULL,
         dia_mes = NULL)

#Includes Wet Bulb Temperatures
df_proj <- df_proj %>% mutate(tbu_max = round(GetTWetBulbFromRelHum(df_proj$temph_max, df_proj$urph_min/100, df_proj$patm_max*100),1),
                              tbu_min = round(GetTWetBulbFromRelHum(df_proj$temph_min, df_proj$urph_max/100, df_proj$patm_min*100),1))

#--------------Importar dados geográficos das estações

#--------------gerar estatísticas
#Cidade/UF
#Latitude, longitude e altitude
#Período - Contabilizar de acordo com cada estação
#TBS, TBUc, TBU, TBSc
#Frequência cumulativa de 0.4%, 1% e 2%
#TBU máximo, TBS máximo e mínimo 

#_____________________________________________

#Generates a final csv file
write.csv2(df_proj, "Dados para projeto HVAC.csv", row.names = FALSE)
