# BBVA Competencia Camaras (ult. act 23 julio 2020)
# Base nueva con columnas de tasas de descuento vueltas colapsadas (ajena, propia, base), y 
# comparacion con base original 

gc()
library(tidyverse)
library(readxl)
library(xlsx)

##### Lectura bases de datos #####

source <- function(f, encoding = 'UTF-8') { # Para que el source si lea acentos y caracteres raros
  l <- readLines(f, encoding=encoding)
  eval(parse(text=l),envir=.GlobalEnv)
}

source("Codigos/Base nueva juntando columnas de tasas de descueto (propia, ajena, base).R")

source("Codigos/Cruce bases de datos tasas de descuento.R")

rm(list=setdiff(ls(), c('Nueva_juntas', 'Nueva_pegada')))

##### Procesamiento de base cruzada para union con base entregada #####

Nueva_pegada_proc <- Nueva_pegada %>% 
  select(AFILIACION, GRUPO, CADENA, `RAZON SOCIAL`, GIRO, estatus) %>% 
  rename(AFILIACION_NUM = AFILIACION)

##### Base nueva/colapsada entregada a COFECE con columna para comparar con base anterior #####

Comparac_bases <- Nueva_juntas %>% 
  left_join(Nueva_pegada_proc)

write.csv(as.data.frame(Comparac_bases), "Outputs/Comparacion entre bases entregadas a COFECE.csv",
          row.names = F)


