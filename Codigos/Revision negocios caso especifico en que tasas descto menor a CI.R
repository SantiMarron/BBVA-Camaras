# BBVA Competencia Camaras (ult. act 27 julio 2020)
# Revision de negocios con tasas de descuento menores a cuotas de intercambio

gc()
library(tidyverse)
library(readxl)
library(xlsx)

##### Lectura de bases de datos #####

source <- function(f, encoding = 'UTF-8') { # Para que el source si lea acentos y caracteres raros
  l <- readLines(f, encoding=encoding)
  eval(parse(text=l),envir=.GlobalEnv)
}

source("Codigos/Comparacion tasas descuento y cuotas intercambio.R")

rm(list=setdiff(ls(), "Comparacion_CI"))

Fact_riesgo <- read_excel("Inputs/Transaccionalidad_comercios_v2.xlsx", 
                          sheet = "VentasxComercio", skip = 3) %>% 
  rename(AJE_CRED_TXS = TXNS...5, AJE_CRED_VAL = MONTO...6,
         PRO_CRED_TXS = TXNS...7, PRO_CRED_VAL = MONTO...8,
         AJE_DEB_TXS = TXNS...9, AJE_DEB_VAL = MONTO...10,
         PRO_DEB_TXS = TXNS...11, PRO_DEB_VAL = MONTO...12) %>% 
  select(AFILIACION, GRUPO, CADENA, `RAZON SOCIAL`, AJE_CRED_VAL, AJE_DEB_VAL) 

Total_liquidacion <- read_excel("Inputs/Total_liquidacion.xlsx", sheet = "Total", skip = 4) %>% 
  rename(AJE_CRED_TXS_TOT = TXNS...1, AJE_CRED_VAL_TOT = MONTO...2,
         AJE_DEB_TXS_TOT = TXNS...5, AJE_DEB_VAL_TOT = MONTO...6)

##### Filtro de TD menor a CI e indicador de igualdad entre ajena y propia #####

Casos <- Comparacion_CI %>% 
  filter(Comp_CI_cred == "TD menor a CI" | Comp_CI_deb2 == "TD menor a CI") %>% 
  select(-GRUPO, -CADENA) %>% 
  rename(Tasa_cred_propia =`TASA DE DESCUENTO CRÉDITO PROPIA`, 
         Tasa_deb_propia =`TASA DE DESCUENTO DÉBITO PROPIA`) %>% 
  mutate_at(c("Tasa_cred_base", "Tasa_deb_base", "Tasa_cred_propia",
              "Tasa_cred_ajena", "Tasa_deb_propia", "Tasa_deb_ajena",
              "Cuota_deb2"), as.numeric) %>% 
  mutate(Ind_cred = 
           case_when(
             (Tasa_cred_propia - Tasa_cred_ajena == 0) & is.na(Tasa_cred_base) ~ "Iguales",
             Tasa_cred_propia == 0  & Tasa_cred_ajena == 0 & !is.na(Tasa_cred_base) ~ "Base",
             (Tasa_cred_propia - Tasa_cred_ajena != 0) & is.na(Tasa_cred_base) ~ "Distintas"),
         Ind_deb = 
           case_when(
             Ind_cuota == "C" ~ "Cuota",
             (Tasa_deb_propia - Tasa_deb_ajena == 0) & is.na(Tasa_deb_base) ~ "Iguales",
             Tasa_deb_propia == 0  & Tasa_deb_ajena == 0 & !is.na(Tasa_deb_base) ~ "Base",
             (Tasa_deb_propia - Tasa_deb_ajena != 0) & is.na(Tasa_deb_base) ~ "Distintas"))

##### Bases de casos para credito y para debito y listas de negocios en Excel #####

Llave_gpo_cad <- Comparacion_CI %>% 
  select(AFILIACION, GRUPO, CADENA, `RAZON SOCIAL`, GIRO) 

Casos_cred <- Casos %>% 
  filter(Comp_CI_cred == "TD menor a CI") %>% 
  select(-Tasa_deb_base, -Tasa_deb_propia, -Tasa_deb_ajena, -Cuota_deb2, -Cuota_deb_CI, 
         -Comp_CI_deb2, -Ind_cuota, -Ind_deb) %>% 
  left_join(Llave_gpo_cad)

Casos_deb <- Casos %>% 
  filter(Comp_CI_deb2 == "TD menor a CI") %>% 
  select(-Tasa_cred_base, -Tasa_cred_propia, -Tasa_cred_ajena, -CI_cred, 
         -Comp_CI_cred, -Ind_cred) %>% 
  left_join(Llave_gpo_cad)

a <- Casos_cred %>% distinct(AFILIACION, GRUPO, CADENA, `RAZON SOCIAL`, GIRO) 

b <- Casos_deb %>% distinct(AFILIACION, GRUPO, CADENA, `RAZON SOCIAL`, GIRO) 

write.xlsx2(as.data.frame(a), "Outputs/Negocios con potencial riesgo TD menor a CI.xlsx", 
            sheetName = "Credito", row.names = F, append = F)

write.xlsx2(as.data.frame(b), "Outputs/Negocios con potencial riesgo TD menor a CI.xlsx", 
            sheetName = "Debito", row.names = F, append = T)

##### Casos en cred y deb donde TD es cero #####

percent <- function(x, digits = 2, format = "f", ...) {      # Create user-defined function
  paste0(formatC(x * 100, format = format, digits = digits, ...), "%")
}

Cred_cero <- Casos_cred %>% 
  mutate(Ind = 
           case_when(Ind_cred == 'Base' & Tasa_cred_base == 0 ~ 1,
                     Ind_cred == 'Iguales' & Tasa_cred_ajena == 0 ~ 1,
                     TRUE ~ 0)) %>% 
  filter(Ind == 1) %>% 
  left_join(Fact_riesgo) %>% 
  select(-CI_deb, -Ind_cred, -Ind, -AJE_DEB_VAL)

print(str_c('La facturacion para negocios en riesgo con TD de cred = 0 representa el: ',
            percent(sum(Cred_cero$AJE_CRED_VAL, na.rm = T)/Total_liquidacion$AJE_CRED_VAL_TOT),
            ' del total de facturacion en credito ajena'))

Deb_cero <- Casos_deb %>% 
  mutate(Ind = 
           case_when(Ind_deb == 'Cuota' & Cuota_deb2 == 0 ~ 1,
                     Ind_deb == 'Base' & Tasa_deb_base == 0 ~ 1,
                     Ind_deb %in% c('Iguales', 'Diferentes') & Tasa_deb_ajena == 0 ~ 1,
                     TRUE ~ 0)) %>% 
  filter(Ind == 1) %>% 
  left_join(Fact_riesgo) %>% 
  select(-Ind_deb, -Ind, -AJE_CRED_VAL) 

print(str_c('La facturacion para negocios en riesgo con TD de deb = 0 representa el: ',
            percent(sum(Deb_cero$AJE_DEB_VAL, na.rm = T)/Total_liquidacion$AJE_DEB_VAL_TOT),
            ' del total de facturacion en debito ajena'))

write.xlsx2(as.data.frame(Cred_cero), "Outputs/Casos en riesgo donde TD es igual a cero.xlsx", 
            sheetName = "Credito", row.names = F, append = F)

write.xlsx2(as.data.frame(Deb_cero), "Outputs/Casos en riesgo donde TD es igual a cero.xlsx", 
            sheetName = "Debito", row.names = F, append = T)

##### Anexo: Filtro de tasas que podrian ser ponderables entre ajena y propia #####

# Alejandra Magaña indico en Teams el 1 de junio que BBVA tiene participacion de emisor de 28%, esa seria para la tasa propia

Cred_ponderados <- Casos_cred %>% 
  filter(Ind_cred == "Distintas") # Todas son iguales o la base

Deb_ponderados <- Casos_deb %>% 
  filter(Ind_deb == "Distintas" ) %>% 
  mutate(Tasa_deb_pond = 0.28*Tasa_deb_propia + 0.72*Tasa_deb_ajena) #Sigue siendo menor, de hecho aun menor

rm(Cred_ponderados)
rm(Deb_ponderados)


