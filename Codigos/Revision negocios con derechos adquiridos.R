# BBVA Competencia Camaras (ult. act 2 junio 2020)
# Revision de casos con cuotas en pesos aunque debieran ser %, por derechos adquiridos (restaurantes, etc)

gc()
library(tidyverse)
library(readxl)
library(xlsx)

##### Lectura de bases de datos #####

source <- function(f, encoding = 'UTF-8') { # Para que el source si lea acentos y caracteres raros
  l <- readLines(f, encoding=encoding)
  eval(parse(text=l),envir=.GlobalEnv)
}

source("Codigos/Revision negocios caso especifico en que tasas descto menor a CI.R")

rm(list=setdiff(ls(), "Comparacion_CI"))

Der_adquiridos <- read_excel("Inputs/Lista Derechos Adquiridos_con tasas de intercambio_2019_3.xlsx", 
                             range = "A3:G48")

Entregada <- read_excel("Inputs/124_vi_2019_10.xlsx") %>% 
  rename(AFILIACION = AFILIACION_NUM,
         Tasa_cred = `TASA CRED`, 
         Tasa_deb = `TASA DEB`, 
         Cuota_deb = `CUOT DEB`) %>% 
  mutate_at(c("Tasa_cred", "Tasa_deb", "Cuota_deb", "FACT_TDC", "FACT_DEB"), as.numeric) %>% 
  select(AFILIACION, FACT_TDC, FACT_DEB)


##### Procesamiento de bases para union #####

Casos_aplicables <- Comparacion_CI %>% 
  filter(Comp_CI_deb2 == "No aplica") %>% 
  select(AFILIACION, `RAZON SOCIAL`, GRUPO, CADENA, GIRO, Cuota_deb2, Ind_cuota, Cuota_deb_CI, CI_deb)

Der_proc <- Der_adquiridos %>% 
  mutate(GRUPO = case_when(str_length(GRUPO) == 1 ~ str_c("00000", GRUPO),
                           str_length(GRUPO) == 2 ~ str_c("0000", GRUPO),
                           str_length(GRUPO) == 3 ~ str_c("000", GRUPO),
                           str_length(GRUPO) == 4 ~ str_c("00", GRUPO)),
         CADENA = case_when(str_length(CADENA) == 1 ~ str_c("00000", CADENA),
                            str_length(CADENA) == 2 ~ str_c("0000", CADENA))) %>% 
  select(-`Categoría Crédito`)


##### Union de bases #####

Unidas <- Casos_aplicables %>% 
  left_join(Der_proc)
  
Neg_c_derechos <- Unidas %>% 
  filter(!is.na(NOMBRE)) # Si cuadra, aqui estan Toks y Sanborns, los importantes

print(nrow(Neg_c_derechos)/nrow(Casos_aplicables)) # % de registros con derechos adquiridos que se explican
  
Resto <- Unidas %>% 
  filter(is.na(NOMBRE)) %>% 
  select(-NOMBRE, -`Intercambio Créd`, -`Cuota Débito`, -Cuota_deb_CI, -Ind_cuota) # Si cuadra, desde el Input dice que Vips no

write.xlsx2(as.data.frame(Resto), "Outputs/Negocios atipicos-TD en pesos (cuota)-CI en tasa.xlsx", 
            sheetName = "Lista", row.names = F, append = F)

##### Porcentaje de facturacion ##### 

Fact <- Resto %>% 
  left_join(Entregada) %>% 
  filter(!is.na(FACT_DEB))

print(sum(Fact$FACT_DEB)/sum(Entregada$FACT_DEB))

  
