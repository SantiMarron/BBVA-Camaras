# BBVA Competencia Camaras (ult act 14 abril 2020)
# Cruce de bds de tasas de descuento para ver discrepancias

gc()
library(tidyverse)
library(readxl)
library(xlsx)

##### Lectura de bases de datos #####

Entregada <- read_excel("Inputs/124_vi_2019_10.xlsx")

Nueva <- read_excel("Inputs/Ejecicio tasas de descuento.xlsx") %>% 
  select(-...19, -...20, -...21, -...22) %>% 
  slice(3:193921)

##### Procesamiento base entregada #####

Entregada_proc <- Entregada %>% 
  rename(Tasa_cred = `TASA CRED`, 
         Tasa_deb = `TASA DEB`, 
         Cuota_deb = `CUOT DEB`) %>% 
  mutate_at(c("Tasa_cred", "Tasa_deb", "Cuota_deb"), as.numeric) %>% 
  select(AFILIACION_NUM, Tasa_cred, Tasa_deb, Cuota_deb)

##### Procesamiento base nueva #####

Nueva_proc <- Nueva %>% 
  rename(AFILIACION_NUM = AFILIACION, Tasa_cred2_base = `TASA DE DESCUENTO CRÉDITO BASE`, 
         Tasa_cred2_propia =`TASA DE DESCUENTO CRÉDITO PROPIA`, 
         Tasa_cred2_ajena =`TASA DE DESCUENTO CRÉDITO AJENA`,
         Tasa_deb2_base = `TASA DE DESCUENTO  DÉBITO BASE`, 
         Tasa_deb2_propia =`TASA DE DESCUENTO DÉBITO PROPIA`, 
         Tasa_deb2_ajena =`TASA DE DESCUENTO DÉBITO AJENA`,
         Cuota_deb2 = `CUOTA DÉBITO`) %>% 
  mutate_at(c("Tasa_cred2_base", "Tasa_deb2_base", "Tasa_cred2_propia",
              "Tasa_cred2_ajena", "Tasa_deb2_propia", "Tasa_deb2_ajena",
              "Cuota_deb2"), as.numeric) %>% 
  select(AFILIACION_NUM, Tasa_cred2_base, Tasa_cred2_propia, Tasa_cred2_ajena,
         Tasa_deb2_base, Tasa_deb2_propia, Tasa_deb2_ajena, Cuota_deb2)

##### Union de bases y chequeos #####

Unida <- Nueva_proc %>% 
  left_join(Entregada_proc) %>% 
  mutate(revtasa_cred = ifelse(!is.na(Tasa_cred - Tasa_cred2_base), Tasa_cred - Tasa_cred2_base,
                               ifelse(!is.na(Tasa_cred - Tasa_cred2_ajena), 
                                      Tasa_cred - Tasa_cred2_ajena,
                                      ifelse(!is.na(Tasa_cred - Tasa_cred2_propia),
                                             Tasa_cred - Tasa_cred2_propia, NA))),
         revtasa_deb = ifelse(!is.na(Tasa_deb - Tasa_deb2_base), Tasa_deb - Tasa_deb2_base,
                               ifelse(!is.na(Tasa_deb - Tasa_deb2_ajena), 
                                      Tasa_deb - Tasa_deb2_ajena,
                                      ifelse(!is.na(Tasa_deb - Tasa_deb2_propia),
                                             Tasa_deb - Tasa_deb2_propia, NA))),
         revcuota_deb = Cuota_deb - Cuota_deb2) %>% 
  mutate(
    estatus = 
      case_when((is.na(revtasa_cred) & is.na(revtasa_deb) & is.na(revcuota_deb)) ~ "Nuevos",
                revtasa_cred == 0 & revtasa_deb == 0 & is.na(revcuota_deb) ~ "Tasas_iguales",
                revtasa_cred != 0 & revtasa_deb == 0 & is.na(revcuota_deb) ~ "Tcred_distinta",
                revtasa_cred == 0 & revtasa_deb != 0 & is.na(revcuota_deb) ~ "Tdeb_distinta",
                revtasa_cred != 0 & revtasa_deb != 0 & is.na(revcuota_deb) ~ "Tasas_distintas",
                revtasa_cred != 0 & revtasa_deb == 0 & revcuota_deb == 0 ~ "DebIgual_CredDist",
                revtasa_deb == 0 & revcuota_deb != 0 ~ "Cdeb_distinta",
                revtasa_cred == 0 & revcuota_deb == 0 ~ "Cdeb_igual",
                revtasa_cred == 0 & revtasa_deb == 0 & revcuota_deb == 0 ~ "Todas_iguales")) %>% 
  select(AFILIACION_NUM, estatus, 1, Tasa_cred, 2:4, revtasa_cred, Tasa_deb, 5:7,
         revtasa_deb, Cuota_deb, Cuota_deb2, revcuota_deb) 

##### Pegado de estatus con base nueva completa #####

Llave <- Unida %>% 
  rename(AFILIACION = AFILIACION_NUM) %>% 
  select(AFILIACION, estatus, Tasa_cred, Tasa_deb, 
         Cuota_deb, revtasa_cred, revtasa_deb, revcuota_deb)

Descrip_giros <- Entregada %>% 
  select(COD_GIRO_CO, `Giro comercial (Familia)`) %>% 
  distinct() %>% 
  rename(GIRO = COD_GIRO_CO, DESC_GIRO = `Giro comercial (Familia)`) %>% 
  mutate(GIRO = as.character(GIRO))

Nueva_pegada <- Nueva %>% 
  left_join(Llave) %>% 
  left_join(Descrip_giros) %>% 
  select(1:5, 26, 6:25) %>% 
  mutate(
    estatus = 
      case_when(estatus == "Tasas_iguales" | estatus == "Todas_iguales" | estatus == "Cdeb_igual" ~ "Iguales",
                estatus == "Tasas_distintas" ~ "Todo difiere",
                estatus == "Tdeb_distinta" | estatus == "Cdeb_distinta" ~ "Debito difiere",
                estatus == "DebIgual_CredDist"| estatus == "Tcred_distinta" ~ "Credito difiere",
                estatus == "Nuevos" ~ "No entregados (nuevos)")) %>% 
  distinct(AFILIACION, .keep_all = T)

write.csv(Nueva_pegada, "Outputs/Base nueva con columna de comparacion entre bases.csv", 
          row.names = F)

Tabla_resumen <- Nueva_pegada %>% 
  group_by(estatus) %>% 
  summarise(Numero_de_afiliaciones = n()) %>% 
  mutate(Porcentaje = Numero_de_afiliaciones / sum(Numero_de_afiliaciones)) %>% 
  arrange(desc(Porcentaje))

write.csv(Tabla_resumen, "Outputs/Tabla resumen de comparaciones entre bases.csv", 
          row.names = F)

