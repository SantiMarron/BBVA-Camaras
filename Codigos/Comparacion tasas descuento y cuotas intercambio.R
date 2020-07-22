# BBVA Competencia Camaras 15 abril 2020
# Comparacion de cuotas de intercambio con tasas de descuento en base de datos nueva 

gc()
library(tidyverse)
library(readxl)
library(xlsx)

##### Lectura de bases de datos necesarias #####

source <- function(f, encoding = 'UTF-8') { # Para que el source si lea acentos y caracteres raros
  l <- readLines(f, encoding=encoding)
  eval(parse(text=l),envir=.GlobalEnv)
}

source("Codigos/Cruce bases de datos tasas de descuento.R")

rm(list=setdiff(ls(), "Nueva_pegada"))

Base_CI <- read_excel("Inputs/Tasa intercambio 2019. (OPI).xlsx", skip = 1) %>% 
  slice(2:284) %>% 
  rename(CI_cred = CREDITO, CI_deb = DÉBITO) %>% 
  select(MCC, CI_cred, CI_deb) %>% 
  mutate(ind = ifelse(str_sub(CI_deb, 5, 5) == "%", "Tasa_deb", "Cuota_deb_CI")) %>% 
  mutate(i = row_number(MCC)) %>% # Para hacer identificadores unicos en spread
  spread(ind, CI_deb) %>% select(-i) %>% 
  rename(CI_deb = Tasa_deb) %>% 
  mutate(CI_cred = str_sub(CI_cred, 1, 4), CI_deb = str_sub(CI_deb, 1, 4)) %>% 
  mutate_at(c("CI_cred", "Cuota_deb_CI", "CI_deb"), as.numeric) %>% 
  group_by(MCC) %>% summarise_at(c("CI_cred", "Cuota_deb_CI", "CI_deb"), mean, na.rm = TRUE) %>% 
  distinct()

##### Comparacion de tasas de descuento ajenas con cuotas de intercambio por giro #####

Comparacion_CI <- Nueva_pegada %>% 
  mutate(GIRO = as.numeric(GIRO)) %>% 
  mutate(MCC = case_when(GIRO >= 3000 & GIRO <= 3299 ~ "3000 AL 3299",
                         GIRO >= 3351 & GIRO <= 3441 ~ "3351 AL 3441",
                         GIRO >= 3501 & GIRO <= 3899 ~ "3501 AL 3899",
                         TRUE ~ as.character(GIRO))) %>% 
  left_join(Base_CI) %>% 
  rename(Tasa_cred_ajena = `TASA DE DESCUENTO CRÉDITO AJENA`, 
         Tasa_deb_ajena = `TASA DE DESCUENTO DÉBITO AJENA`,
         Tasa_cred_base = `TASA DE DESCUENTO CRÉDITO BASE`,
         Tasa_deb_base = `TASA DE DESCUENTO  DÉBITO BASE`,
         Ind_cuota = `CUOTA DEBITO`,
         Cuota_deb2 = `CUOTA DÉBITO`) %>% 
  mutate_at(c("Tasa_cred_base", "Tasa_cred_ajena", "CI_cred", 
              "Tasa_deb_base", "Tasa_deb_ajena", "Cuota_deb2", 
              "CI_deb", "Cuota_deb_CI"), as.numeric) %>% 
  mutate(
    Comp_CI_cred = 
      case_when(is.na(Tasa_cred_base) & Tasa_cred_ajena > CI_cred ~ "TD mayor a CI",
                !is.na(Tasa_cred_base) & Tasa_cred_base > CI_cred ~ "TD mayor a CI",
                is.na(Tasa_cred_base) & Tasa_cred_ajena == CI_cred ~ "TD igual a CI",
                !is.na(Tasa_cred_base) & Tasa_cred_base == CI_cred ~ "TD igual a CI",
                is.na(Tasa_cred_base) & Tasa_cred_ajena < CI_cred ~ "TD menor a CI",
                !is.na(Tasa_cred_base) & Tasa_cred_base < CI_cred ~ "TD menor a CI",
                is.na(CI_cred)  ~ "No se tiene la CI")) %>% 
  mutate(
    Comp_CI_deb = 
      case_when(Ind_cuota == "T" & is.na(Tasa_deb_base) & Tasa_deb_ajena > CI_deb ~ "TD mayor a CI",
                Ind_cuota == "T" & !is.na(Tasa_deb_base) & Tasa_deb_base > CI_deb ~ "TD mayor a CI",
                Ind_cuota == "T" & is.na(Tasa_deb_base) & Tasa_deb_ajena == CI_deb ~ "TD igual a CI",
                Ind_cuota == "T" & !is.na(Tasa_deb_base) & Tasa_deb_base == CI_deb ~ "TD igual a CI",
                Ind_cuota == "T" & is.na(Tasa_deb_base) & Tasa_deb_ajena < CI_deb ~ "TD menor a CI",
                Ind_cuota == "T" & !is.na(Tasa_deb_base) & Tasa_deb_base < CI_deb ~ "TD menor a CI",
                is.na(CI_deb) ~ "No se tiene la CI")) %>% 
  mutate(
    Comp_CI_deb2 =
      case_when(Ind_cuota == "C" & !is.nan(Cuota_deb_CI) & Cuota_deb2 > Cuota_deb_CI ~ "TD mayor a CI",
                Ind_cuota == "C" & !is.nan(Cuota_deb_CI) & Cuota_deb2 == Cuota_deb_CI ~ "TD igual a CI",
                Ind_cuota == "C"&  !is.nan(Cuota_deb_CI) & Cuota_deb2 < Cuota_deb_CI ~ "TD menor a CI",
                Ind_cuota == "C" & is.nan(Cuota_deb_CI) ~ "No aplica",
                TRUE ~ as.character (Comp_CI_deb))) %>% 
  select(-estatus, -Tasa_cred, -Tasa_deb, - Cuota_deb, -revtasa_cred, -revtasa_deb, -revcuota_deb,
         -MCC, -Comp_CI_deb, -`CUOTA DE APERTURA`, -`COMISION USO GPRS`, -`COMISION BAJA FACTURACION`,
         -CRÉDITO, -Débito)

write.csv(Comparacion_CI, "Outputs/Comparacion de TD y CI en base nueva.csv", 
          row.names = F)

a <- Comparacion_CI %>% 
  group_by(Comp_CI_cred) %>% 
  summarise(Num_de_afiliaciones_cred = n()) %>% 
  rename(Estatus = Comp_CI_cred)

b <- Comparacion_CI %>% 
  group_by(Comp_CI_deb2) %>% 
  summarise(Num_de_afiliaciones_deb = n()) %>% 
  rename(Estatus = Comp_CI_deb2)

T_resum_CI <- a %>% 
  full_join(b) %>% 
  mutate(Porc_cred = Num_de_afiliaciones_cred / sum(Num_de_afiliaciones_cred, na.rm = T),
         Porc_deb = Num_de_afiliaciones_deb / sum(Num_de_afiliaciones_deb)) %>% 
  arrange(desc(Porc_deb))

rm(a, b)

write.csv(T_resum_CI, "Outputs/Tabla resumen comparacion TD y CI.csv", 
          row.names = F)
