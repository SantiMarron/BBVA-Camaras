# BBVA Competencia Camaras (ult. act 5 agosto 2020)
# Lectura de archivos log transaccional, p/ dar respuesta a numero de observaciones en numeral 125, 
# p/ base 124.ix del secto reiteramiento. Los archivos se encuentran en disco duro de AMC al 05ago20

gc()
library(tidyverse)
library(readxl)
library(xlsx)
library(vroom)

##### Lectura de bases de otro lado #####

x <- vroom("E:/Particion 1/Adquirente/LOG_ADQ_201907.txt", col_select = c(1)) #no se pudo, tuvo que ser con el 
#cmd solo para ver el numero de lineas, o bien usando el programa Large Text File Viewer


x1 <- vroom("E:/Particion 1/Adquirente/LOG_ADQ_201907.txt", col_names = T, 
            guess_max = 10, n_max = 1) #no se pudo, tuvo que ser con el 
#cmd solo para ver el numero de lineas, o bien usando el programa Large Text File Viewer


Fraudes <- vroom("C:/Users/santi/Downloads/124_XI_ BBDD.csv") 

Fraudes_proc <- Fraudes %>% distinct(Folio)

Ctos_fraudes <- vroom("C:/Users/santi/Downloads/Anexo 124.xii.1.csv") 

