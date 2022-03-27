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

#Generates the initial data frame with the first csv file
df_proj <- read.csv2(paste("Dados unificados - ", "2010", ".csv", sep = ""))

#Adds each csv to the main data frame
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
                              tbu_min = round(GetTWetBulbFromRelHum(df_proj$temph_min, df_proj$urph_max/100, df_proj$patm_min*100),1)) %>%
                       mutate(tbu = pmax(df_proj$tbu_max,df_proj$tbu_min),
                              tbu_max = NULL,
                              tbu_min = NULL)

#Statistics for Cooling and Dehumidification
df_tbuc <- df_proj %>% group_by(ID_estacao, temph_max) %>% summarise(tbuc=round(mean(tbu),1)) #Coincident wet temperature
df_tbsc <- df_proj %>% group_by(ID_estacao, tbu) %>% summarise(tbsc=round(mean(temph_max),1)) #Coincident dry temperature

df_result <- df_proj %>% group_by(ID_estacao) %>% summarise(ano_ini=min(lubridate::year(dia)),
                                                            ano_fim=max(lubridate::year(dia)),
                                                            tbs_max=max(temph_max),
                                                            tbs_min=min(temph_min),
                                                            tbs_996=round(quantile(temph_max, type=5, 0.996), 1),
                                                            tbs_99=round(quantile(temph_max, type=5, 0.99), 1),
                                                            tbs_98=round(quantile(temph_max, type=5, 0.98), 1),
                                                            tbu_max=max(tbu),
                                                            tbu_996=round(quantile(tbu, type=5, 0.996), 1),
                                                            tbu_99=round(quantile(tbu, type=5, 0.99), 1),
                                                            tbu_98=round(quantile(tbu, type=5, 0.98), 1))

df_result <- df_result %>% ungroup () %>% droplevels(.)

df_result <- df_result %>% left_join(df_tbuc, by = c("ID_estacao", "tbs_996" = "temph_max")) %>%
                           rename("tbuc_996" = "tbuc") %>%
                           left_join(df_tbuc, by = c("ID_estacao", "tbs_99" = "temph_max")) %>%
                           rename("tbuc_99" = "tbuc") %>%
                           left_join(df_tbuc, by = c("ID_estacao", "tbs_98" = "temph_max")) %>%
                           rename("tbuc_98" = "tbuc") %>%
                           left_join(df_tbsc, by = c("ID_estacao", "tbu_996" = "tbu")) %>%
                           rename("tbsc_996" = "tbsc") %>%
                           left_join(df_tbsc, by = c("ID_estacao", "tbu_99" = "tbu")) %>%
                           rename("tbsc_99" = "tbsc") %>%
                           left_join(df_tbsc, by = c("ID_estacao", "tbu_98" = "tbu")) %>%
                           rename("tbsc_98" = "tbsc") %>%
                           select(ID_estacao, ano_ini, ano_fim,
                                  tbs_max, tbs_996, tbuc_996, tbs_99, tbuc_99, tbs_98, tbuc_98, tbs_min,
                                  tbu_max, tbu_996, tbsc_996, tbu_99, tbsc_99, tbu_98, tbsc_98)

#_____________________________________________

write.csv2(df_result, "Dados climáticos para projeto.csv", row.names = FALSE)

#_____________________________________________
