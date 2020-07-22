# BBVA Competencia Camaras (ult. act 19 mayo 2020)
# Base nueva con columnas de tasas de descuento vueltas colapsadas (ajena, propia, base)

gc()
library(tidyverse)
library(readxl)
library(xlsx)

##### Lectura de bases de datos #####

Nueva <- read_excel("Inputs/Ejecicio tasas de descuento.xlsx") %>% 
  select(-...19, -...20, -...21, -...22) %>% 
  slice(3:193921)

##### Procesamiento base nueva #####

Nueva_proc <- Nueva %>% 
  rename(AFILIACION_NUM = AFILIACION, Tasa_cred_base = `TASA DE DESCUENTO CRÉDITO BASE`, 
         Tasa_cred_propia =`TASA DE DESCUENTO CRÉDITO PROPIA`, 
         Tasa_cred_ajena =`TASA DE DESCUENTO CRÉDITO AJENA`,
         Tasa_deb_base = `TASA DE DESCUENTO  DÉBITO BASE`, 
         Tasa_deb_propia =`TASA DE DESCUENTO DÉBITO PROPIA`, 
         Tasa_deb_ajena =`TASA DE DESCUENTO DÉBITO AJENA`,
         Cuota_deb = `CUOTA DÉBITO`, Ind_cuota = `CUOTA DEBITO`) %>% 
  mutate_at(c("Tasa_cred_base", "Tasa_deb_base", "Tasa_cred_propia",
              "Tasa_cred_ajena", "Tasa_deb_propia", "Tasa_deb_ajena",
              "Cuota_deb"), as.numeric) %>% 
  select(AFILIACION_NUM, GRUPO, CADENA, `RAZON SOCIAL`,GIRO, Tasa_cred_base, Tasa_cred_propia, Tasa_cred_ajena,
         Tasa_deb_base, Tasa_deb_propia, Tasa_deb_ajena, Cuota_deb, Ind_cuota)

##### Base con columnas de tasa de descuento juntas #####

Nueva_juntas <- Nueva_proc %>% 
  mutate(
    Tasa_cred = 
      case_when(
        (Tasa_cred_propia - Tasa_cred_ajena == 0) & is.na(Tasa_cred_base) ~ as.character(Tasa_cred_ajena),
        Tasa_cred_propia == 0  & Tasa_cred_ajena == 0 & !is.na(Tasa_cred_base) ~ as.character(Tasa_cred_base),
        (Tasa_cred_propia - Tasa_cred_ajena != 0) & is.na(Tasa_cred_base) ~ str_c(Tasa_cred_propia,"/",
                                                                                 Tasa_cred_ajena)),
    Tasa_deb = 
      case_when(
        Ind_cuota == "C" ~ "N/A",
        (Tasa_deb_propia - Tasa_deb_ajena == 0) & is.na(Tasa_deb_base) ~ as.character(Tasa_deb_ajena),
        Tasa_deb_propia == 0  & Tasa_deb_ajena == 0 & !is.na(Tasa_deb_base) ~ as.character(Tasa_deb_base),
        (Tasa_deb_propia - Tasa_deb_ajena != 0) & is.na(Tasa_deb_base) ~ str_c(Tasa_deb_propia,"/",
                                                                                  Tasa_deb_ajena)),
    Cuota_deb2 = 
      case_when(Ind_cuota == "T" ~ "N/A",
                TRUE ~ as.character(Cuota_deb))) %>% 
  select(AFILIACION_NUM, GRUPO, CADENA, `RAZON SOCIAL`,GIRO, Tasa_cred, Tasa_deb, Cuota_deb2, Ind_cuota)


write.csv(Nueva_juntas, "Outputs/Base nueva con columnas de TD colapsadas.csv", row.names = F)
