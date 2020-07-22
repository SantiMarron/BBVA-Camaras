# BBVA Competencia Camaras (ult. act 4 junio 2020)
# Lista de supermercados y grandes superficies y comparacion con base de TD y CI

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

rm(list=setdiff(ls(), "Casos")) # Casos con TD menor a CI de credito y deb, y su estatus (si es cuota, si tasas iguales, etc)

Gdes_sup <- read_excel("Inputs/GDES_SUPS.xlsx", sheet = "grandes superficies") %>% 
  mutate(Tipo = "Grandes Superficies")

Superm <- read_excel("Inputs/GDES_SUPS.xlsx", sheet = "supermercados") %>% 
  mutate(Tipo = "Supermercados")

##### Procesamiento de base de lista de superficies y supermercados #####

Lista_sup <- Gdes_sup %>% 
  bind_rows(Superm) %>% 
  rename(GIRO = COD_GIRO_CO) %>% 
  mutate(GIRO = as.double(GIRO), 
         Ind_cuota2 = case_when(Tipo == "Grandes Superficies" ~ "C",
                                Tipo == "Supermercados" ~ "T")) %>% 
  select(AFILIACION, DES_COMERCIO, GIRO, Tipo, Ind_cuota2)
  

##### Procesamiento base de TD con casos debito en supermercados y gdes sup #####

Casos_sup <- Casos %>% 
  filter(Comp_CI_deb2 == "TD menor a CI") %>% 
  filter(GIRO %in% c(5300, 5311, 5411, 5310, 5331)) %>% 
  select(AFILIACION, `RAZON SOCIAL`, GIRO, Ind_cuota, Ind_deb) 
  

##### Union de bases #####

Unida <- Casos_sup %>% 
  left_join(Lista_sup) %>% 
  mutate(Estatus = case_when(Ind_cuota == Ind_cuota2 ~ "Igual",
                             TRUE ~ "Difiere"))

Unida_sum <- Unida %>% 
  group_by(`RAZON SOCIAL`, Estatus) %>% 
  summarise(Casos = n()) %>% 
  arrange(desc(Casos), Estatus)

# Los pocos que difieren, precisamente es porque tienen derechos adquiridos (ver lista en inputs), entonces aunque debería
# aplicarles tasa, sí les aplica cuota, está bien la base original y los calculos de comparacion
# entre TD y CI

