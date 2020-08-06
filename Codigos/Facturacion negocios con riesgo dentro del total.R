# BBVA Competencia Camaras (ult. act 27 julio 2020)
# Facturacion de negocios riesgosos dentro del total

gc()
library(tidyverse)
library(readxl)
library(xlsx)
library(scales)

##### Lectura de bases de datos #####

source <- function(f, encoding = 'UTF-8') { # Para que el source si lea acentos y caracteres raros
  l <- readLines(f, encoding=encoding)
  eval(parse(text=l),envir=.GlobalEnv)
}

source("Codigos/Revision negocios caso especifico en que tasas descto menor a CI.R")

rm(list=setdiff(ls(), c('Comparacion_CI', 'a', 'b')))

Fact_riesgo <- read_excel("Inputs/Transaccionalidad_comercios_v2.xlsx", 
                          sheet = "VentasxComercio", skip = 3) %>% 
  rename(AJE_CRED_TXS = TXNS...5, AJE_CRED_VAL = MONTO...6,
         PRO_CRED_TXS = TXNS...7, PRO_CRED_VAL = MONTO...8,
         AJE_DEB_TXS = TXNS...9, AJE_DEB_VAL = MONTO...10,
         PRO_DEB_TXS = TXNS...11, PRO_DEB_VAL = MONTO...12)

Total_liquidacion <- read_excel("Inputs/Total_liquidacion.xlsx", sheet = "Total", skip = 4) %>% 
  rename(AJE_CRED_TXS_TOT = TXNS...1, AJE_CRED_VAL_TOT = MONTO...2,
         AJE_DEB_TXS_TOT = TXNS...5, AJE_DEB_VAL_TOT = MONTO...6)

##### Procesamiento de bases de datos para union #####

Fact_riesgo_proc <- Fact_riesgo %>% 
  select(AFILIACION, GRUPO, CADENA, `RAZON SOCIAL`, AJE_CRED_VAL, AJE_DEB_VAL) 

CompCI_proc <- Comparacion_CI %>% 
  select(AFILIACION, GRUPO, CADENA, `RAZON SOCIAL`, GIRO, Comp_CI_cred, Comp_CI_deb2) 

##### Union ##### 

Unida <- Fact_riesgo_proc %>% 
  left_join(CompCI_proc) %>% 
  filter(!is.na(Comp_CI_cred))

##### Facturacion como % del total credito y debito #####

Fact_cred <- Unida %>% 
  filter(Comp_CI_cred == 'TD menor a CI') 

percent <- function(x, digits = 2, format = "f", ...) {      # Create user-defined function
  paste0(formatC(x * 100, format = format, digits = digits, ...), "%")
}

print(str_c('La facturacion para negocios en riesgo representa el: ',
      percent(sum(Fact_cred$AJE_CRED_VAL)/Total_liquidacion$AJE_CRED_VAL_TOT),
      ' del total de facturacion en credito ajena'))

Fact_deb <- Unida %>% 
  filter(Comp_CI_deb2 == 'TD menor a CI') 

print(str_c('La facturacion para negocios en riesgo representa el: ',
            percent(sum(Fact_deb$AJE_DEB_VAL)/Total_liquidacion$AJE_DEB_VAL_TOT),
            ' del total de facturacion en debito ajena'))

Fact_tot <- Unida %>% 
  filter(Comp_CI_cred == 'TD menor a CI' | Comp_CI_deb2 == 'TD menor a CI') %>% 
  mutate(Ind_cred = ifelse(Comp_CI_cred == 'TD menor a CI', 1, 0),
         Ind_deb = ifelse(Comp_CI_deb2 == 'TD menor a CI', 1, 0)) %>% 
  group_by(Ind_cred, Ind_deb) %>% 
  summarise(AJE_CRED = sum(AJE_CRED_VAL), AJE_DEB = sum(AJE_DEB_VAL)) %>% 
  group_by(Ind_cred) %>% mutate(AJE_CRED_RISK = sum(AJE_CRED)) %>% ungroup() %>% 
  group_by(Ind_deb) %>% mutate(AJE_DEB_RISK = sum(AJE_DEB)) %>% ungroup() %>% 
  filter(Ind_cred == 1, Ind_deb == 1) %>% 
  select(-Ind_cred, -Ind_deb) 

print(str_c(
  'La facturacion para negocios en riesgo representa el: ',
  percent(
    (Fact_tot$AJE_CRED_RISK + Fact_tot$AJE_DEB_RISK) / (
      Total_liquidacion$AJE_CRED_VAL_TOT + Total_liquidacion$AJE_DEB_VAL_TOT)),
    ' del total de facturacion total ajena'))
  

##### Negocios que faltaron #####  

Cred_falt <- a %>% 
  left_join(Fact_riesgo_proc) %>% 
  filter(is.na(AJE_CRED_VAL)) %>% select(-AJE_CRED_VAL, -AJE_DEB_VAL) %>% 
  distinct()

Deb_falt <- b %>% 
  left_join(Fact_riesgo_proc) %>% 
  filter(is.na(AJE_DEB_VAL)) %>% select(-AJE_CRED_VAL, -AJE_DEB_VAL) %>% 
  distinct()

rm(a, b)

Tot_falt <- Cred_falt %>% 
  bind_rows(Deb_falt) %>% 
  distinct()

write.xlsx2(as.data.frame(Tot_falt), "Outputs/Negocios en riesgo sin facturacion enviada.xlsx", 
            sheetName = "Lista_completa", row.names = F, append = F)

write.xlsx2(as.data.frame(Cred_falt), "Outputs/Negocios en riesgo sin facturacion enviada.xlsx", 
            sheetName = "Credito", row.names = F, append = T)

write.xlsx2(as.data.frame(Deb_falt), "Outputs/Negocios en riesgo sin facturacion enviada.xlsx", 
            sheetName = "Debito", row.names = F, append = T)
