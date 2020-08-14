# BBVA Competencia Camaras (ult. act 13 agosto 2020)
# Revision de si estan o no afiliaciones enviadas en listas por AMC y DBR el 13ago20

gc()
library(tidyverse)
library(readxl)
library(xlsx)

##### Lectura bases de datos #####

source <- function(f, encoding = 'UTF-8') { # Para que el source si lea acentos y caracteres raros
  l <- readLines(f, encoding=encoding)
  eval(parse(text=l),envir=.GlobalEnv)
}

source('Codigos/Base nueva juntando columnas de tasas de descueto (propia, ajena, base).R')

Afil_larga <- read_excel("Inputs/numeros de afiliacion a revisar ago20.xlsx",
                         sheet = 'Lista_Diana') %>%
  distinct() %>% 
  rename(AFILIACION_NUM = `No. De Afiliación`) %>% 
  mutate(ind_larga = 1) 

Afil_corta <- read_excel("Inputs/numeros de afiliacion a revisar ago20.xlsx", 
                                                    sheet = "Lista_Gerardo") %>% 
  distinct() %>% 
  rename(AFILIACION_NUM = `Lista Gerardo`) %>% 
  mutate(ind_corta = 1)

##### Revisiones #####

Corta_enLarga <- Afil_larga %>% 
  left_join(Afil_corta) %>% 
  filter(is.na(ind_corta))

str_c('Todas las de la corta estan en la larga, solo falta remanente: ',
      nrow(Afil_corta) + nrow(Corta_enLarga), ' observaciones totales')

Larga_enCorta <- Afil_corta %>% 
  left_join(Afil_larga) %>% 
  filter(is.na(ind_larga))

str_c('Todas las de la la larga, estan en la corta: ',
      nrow(Larga_enCorta), ' observaciones no halladas')

Base_enLarga <- Afil_larga %>% 
  left_join(Nueva_juntas) %>% 
  filter(is.na(GRUPO)) %>% 
  select(AFILIACION_NUM)

str_c('Existen ', nrow(Base_enLarga), ' observaciones de la base larga',
' que no estan en la base completa')

write.xlsx2(as.data.frame(Base_enLarga), "Outputs/Afiliaciones en base de 800 de Diana no encontradas en base completa.xlsx", 
            sheetName = "Base_enLarga", row.names = F, append = F)

