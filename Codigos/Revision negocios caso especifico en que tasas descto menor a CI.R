# BBVA Competencia Camaras (ult. act 1 junio 2020)
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

Entregada <- read_excel("Inputs/124_vi_2019_10.xlsx") %>% 
  rename(AFILIACION = AFILIACION_NUM,
         Tasa_cred = `TASA CRED`, 
         Tasa_deb = `TASA DEB`, 
         Cuota_deb = `CUOT DEB`) %>% 
  mutate_at(c("Tasa_cred", "Tasa_deb", "Cuota_deb", "FACT_TDC", "FACT_DEB"), as.numeric) %>% 
  select(AFILIACION, FACT_TDC, FACT_DEB)

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

##### Union con base entregada para ver % de facturacion, credito y debito #####

Fact_cred <- Casos_cred %>% 
  left_join(Entregada) %>% 
  filter(!is.na(FACT_TDC)) %>% select(-Ind_cred)
  
print(nrow(Fact_cred)/nrow(Casos_cred)) # % que hace match con original para facturacion
print(sum(Fact_cred$FACT_TDC)/sum(Entregada$FACT_TDC)) # % de la facturacion de los que TD < CI

Fact_deb <- Casos_deb %>% 
  left_join(Entregada) %>% 
  filter(!is.na(FACT_TDC)) %>% select(-Ind_deb)

print(nrow(Fact_deb)/nrow(Casos_deb)) # % que hace match con original para facturacion
print(sum(Fact_deb$FACT_DEB)/sum(Entregada$FACT_DEB)) # % de la facturacion de los que TD < CI

##### Anexo: Filtro de tasas que podrian ser ponderables entre ajena y propia #####

# Alejandra Magaña indico en Teams el 1 de junio que BBVA tiene participacion de emisor de 28%, esa seria para la tasa propia

Cred_ponderados <- Casos_cred %>% 
  filter(Ind_cred == "Distintas") # Todas son iguales o la base

Deb_ponderados <- Casos_deb %>% 
  filter(Ind_deb == "Distintas" ) %>% 
  mutate(Tasa_deb_pond = 0.28*Tasa_deb_propia + 0.72*Tasa_deb_ajena) #Sigue siendo menor, de hecho aun menor

rm(Cred_ponderados)
rm(Deb_ponderados)
