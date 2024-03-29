#INMET climates data - Data Wrangling
#Climatic design information for Cooling & Dehumitification projects in Brazil
#Josué Santana
#Data source: https://portal.inmet.gov.br/dadoshistoricos

#Librarys
library(readr)
library(dplyr)
library(stringr)

#Project folder
pasta <- getwd()

#Defines the year and set the folder of the files to be unified
ano <- "2021" #Choose manually
fonte <- paste(str_replace(pasta, "tratativa", "source"), "/", ano, sep = "")
setwd(fonte)

#Lists the csv files for each year
formato_arq <- "CSV"
lista <- dir(pattern=formato_arq)

#Defines interest columns
mant_col <- c(1,2,5,6,10,11,14,15)

#Generates the initial data frame with the first csv file
df_unif <- data.table::fread(lista[1], skip=9, select = mant_col, dec=",")
df_unif$V1 <- as.Date(df_unif$V1, "%Y/%m/%d") #Set column V1 as Date

#Includes data about the weather station
dados <- str_split(lista[1], "_", simplify = TRUE)
df_unif <- df_unif %>% mutate(estado = dados[,3],
                              ID_estacao = dados[,4],
                              local = dados[,5])
#Renames and rearranging the columns 
df_unif <- df_unif %>% rename("dia" = "V1",
                              "hora" = "V2",
                              "patm_max" = "V5",
                              "patm_min" = "V6",
                              "temph_max" = "V10",
                              "temph_min" = "V11",
                              "urph_max" = "V14",
                              "urph_min" = "V15") %>%
                        select(ID_estacao, local, estado, everything())

#Replaces -9999 by NA
df_unif[df_unif == -9999] <- NA

#Replaces NA values by mean values - Grouped by day and temperatures - SAME YEAR
medias_dia <- df_unif %>% group_by(dia) %>% 
                    summarise(patm_max = round(mean(patm_max, na.rm = T),1),
                              patm_min = round(mean(patm_min, na.rm = T),1),
                              temph_max = round(mean(temph_max, na.rm = T),1),
                              temph_min = round(mean(temph_min, na.rm = T),1))

medias_tempmin <- df_unif %>% group_by(temph_min) %>% 
                    summarise(urph_max = round(mean(urph_max, na.rm = T),0))

medias_tempmax <- df_unif %>% group_by(temph_max) %>% 
                    summarise(urph_min = round(mean(urph_min, na.rm = T),0))

df_unif <- df_unif %>% ungroup () %>% droplevels(.)

df_unif <- df_unif %>% left_join(medias_dia, by = c("dia"), suffix = c("", ".y")) %>%
                  mutate(patm_max = coalesce(patm_max, patm_max.y),
                         patm_min = coalesce(patm_min, patm_min.y),
                         temph_max = coalesce(temph_max, temph_max.y),
                         temph_min = coalesce(temph_min, temph_min.y)) %>%
                  left_join(medias_tempmin, by = c("temph_min"), suffix = c("", ".y")) %>%
                  mutate(urph_max = coalesce(urph_max, urph_max.y)) %>%
                  left_join(medias_tempmax, by = c("temph_max"), suffix = c("", ".y")) %>%
                  mutate(urph_min = coalesce(urph_min, urph_min.y)) %>%
                  mutate(patm_max.y = NULL,
                         patm_min.y = NULL,
                         temph_max.y = NULL,
                         temph_min.y = NULL,
                         urph_max.y = NULL,
                         urph_min.y = NULL)

#Prepares and adds each csv to the main data frame
for(i in lista[2:length(lista)])
{
  df_temp <- data.table::fread(i, skip=9, select = mant_col, dec=",")
  df_temp$V1 <- as.Date(df_temp$V1, "%Y/%m/%d")
  
  dados <- str_split(i, "_", simplify = TRUE)
  df_temp <- df_temp %>% mutate(estado = dados[,3],
                                ID_estacao = dados[,4],
                                local = dados[,5])

  df_temp <- df_temp %>% rename("dia" = "V1",
                                "hora" = "V2",
                                "patm_max" = "V5",
                                "patm_min" = "V6",
                                "temph_max" = "V10",
                                "temph_min" = "V11",
                                "urph_max" = "V14",
                                "urph_min" = "V15") %>%
                          select(ID_estacao, local, estado, everything())
  
  df_temp[df_temp == -9999] <- NA
  
  medias_dia <- df_temp %>% group_by(dia) %>% 
    summarise(patm_max = round(mean(patm_max, na.rm = T),1),
              patm_min = round(mean(patm_min, na.rm = T),1),
              temph_max = round(mean(temph_max, na.rm = T),1),
              temph_min = round(mean(temph_min, na.rm = T),1))
  
  medias_tempmin <- df_temp %>% group_by(temph_min) %>% 
    summarise(urph_max = round(mean(urph_max, na.rm = T),0))
  
  medias_tempmax <- df_temp %>% group_by(temph_max) %>% 
    summarise(urph_min = round(mean(urph_min, na.rm = T),0))
  
  df_temp <- df_temp %>% ungroup () %>% droplevels(.)
  
  df_temp <- df_temp %>% left_join(medias_dia, by = c("dia"), suffix = c("", ".y")) %>%
    mutate(patm_max = coalesce(patm_max, patm_max.y),
           patm_min = coalesce(patm_min, patm_min.y),
           temph_max = coalesce(temph_max, temph_max.y),
           temph_min = coalesce(temph_min, temph_min.y)) %>%
    left_join(medias_tempmin, by = c("temph_min"), suffix = c("", ".y")) %>%
    mutate(urph_max = coalesce(urph_max, urph_max.y)) %>%
    left_join(medias_tempmax, by = c("temph_max"), suffix = c("", ".y")) %>%
    mutate(urph_min = coalesce(urph_min, urph_min.y)) %>%
    mutate(patm_max.y = NULL,
           patm_min.y = NULL,
           temph_max.y = NULL,
           temph_min.y = NULL,
           urph_max.y = NULL,
           urph_min.y = NULL)
  
  df_unif <- dplyr::bind_rows(df_temp, df_unif) #Adds to the main data frame
}

#Excludes Antarctica's weather station
df_unif <- filter(df_unif, ID_estacao != "C891")

#Returns the main folder and generates a treated and unified csv file by year
setwd(pasta)
write.csv2(df_unif, paste("Dados unificados - ", ano, ".csv", sep = ""), row.names = FALSE)

#_____________________________________________

#Generates the initial data frame with the first csv file
df_proj <- read.csv2(paste("Dados unificados - ", "2010", ".csv", sep = ""))

#Adds each csv to the main data frame
for(i in 2011:2021)
{
  df_temp <- read.csv2(paste("Dados unificados - ", i, ".csv", sep = ""))
  df_proj <- dplyr::bind_rows(df_temp, df_proj) #Adds to the main data frame
}

#Separates data for each weather station
for(i in lista_estacoes)
{
  df_temp <- dplyr::filter(df_proj, df_proj$ID_estacao == i)
  write.csv2(df_temp, paste("Dados climáticos", i, ".csv", sep = ""), row.names = FALSE)
}