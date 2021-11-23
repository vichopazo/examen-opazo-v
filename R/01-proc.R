#Cargar paquetes

library(pacman)
pacman::p_load(tidyverse,
               sjmisc,
               srvyr,
               tidyr, 
               dplyr, 
               sjPlot, 
               car, 
               haven)

#Cargar y renombrar BDD (Encuesta nacional de empleo 2021_08, j-a-s) 

ENE_2021_08 <- read_sav(url("https://ine.cl/docs/default-source/ocupacion-y-desocupacion/bbdd/2021/spss/ene-2021-08-jas.sav?sfvrsn=a5e96065_4&download=true")) 

#Inspección inicial BDD

head(ENE_2021_08)
view_df(ENE_2021_08)

#Explorar BDD 1: Encontrar variables de interés

find_var(ENE_2021_08, "sexo") #nr col: 15
find_var(ENE_2021_08, "cine") #nr col: 173 
find_var(ENE_2021_08, "edad") #nr col: 177
find_var(ENE_2021_08, "activ") #nr col: 178
find_var(ENE_2021_08, "e12") #nr col: 125 -motivos de no estar disponible para trabajar
find_var(ENE_2021_08, "e9") #nr col: 119 -razón de no buscar empleo

#Explorar BDD 2: Encontrar variables de identificación, estrato y ponderación

find_var(ENE_2021_08, "id_identificacion") #nr col: 8
find_var(ENE_2021_08, "estrato") #nr col: 5
find_var(ENE_2021_08, "fact_cal") #nr col: 181

#Explorar BDD 3: Definir clase variables de interés

class("sexo")
class("cine")
class("tramo_edad")
class("activ")
class("e12")
class("e9") #all==character

#Explorar casos variables analíticas

frq(ENE_2021_08$sexo)
frq(ENE_2021_08$cine)
frq(ENE_2021_08$tramo_edad)
frq(ENE_2021_08$activ)
frq(ENE_2021_08$e12)
frq(ENE_2021_08$e9)

#Crear objeto BBD procesada, seleccionar y renombrar variables de interés

datos_proc <- select(ENE_2021_08, sexo, cine, edad, activ, motiv_no_dis_trab = e12, motiv_no_bus_trab = e9, 
                     id = id_identificacion, estrato, fact_exp = fact_cal)

view_df(datos_proc) 

#Recodificación de variables de interés (saturar categorías_1)

datos_proc <- datos_proc %>% 
  filter(edad >= 15) %>%
  mutate(edad_tramo = case_when(edad >= 15 & edad <=29 ~ "Joven", 
                                edad > 29 & edad <=59 ~ "Adulto/a", 
                                edad > 59 ~ "Adulto/a mayor", TRUE ~ NA_character_))

datos_proc <- datos_proc %>% 
  mutate(activ = case_when(activ >= 1 & activ < 3 ~ "Dentro de la fuerza de trabajo", activ >= 3 ~ 
                                            "Fuera de la fuerza de trabajo", TRUE ~ NA_character_))

datos_proc <- datos_proc %>% 
  mutate(motiv_no_dis_trab = if_else(motiv_no_dis_trab %in% c(5,6,7), "Es pensionado/a, jubilado/a o rentista", 
                         if_else(motiv_no_dis_trab %in% c(3), "Porque está estudiando o empezará a estudiar pronto",
                         if_else(motiv_no_dis_trab %in% c(1,2,10,12,13), "Otra razón", 
                         if_else(motiv_no_dis_trab %in% c(4,11), "Por responsabilidades familiares (cuidado de 
                                 niños o personas dependientes) permanentes o temporales", 
                         if_else(motiv_no_dis_trab %in% c(8,9), "Por motivos de salud permanentes o temporales", 
                                 NA_character_))))))

datos_proc <- datos_proc %>% 
  mutate(motiv_no_bus_trab = if_else(motiv_no_bus_trab %in% c(5,6,7), "Es pensionado/a, jubilado/a o rentista", 
                             if_else(motiv_no_bus_trab %in% c(4), "Porque está estudiando o preparando estudios", 
                             if_else(motiv_no_bus_trab %in% c(1,2,9,12,13,14,15,16,17,18,19,20,21,22), "Otra razón", 
                             if_else(motiv_no_bus_trab %in% c(3,11), "Por responsabilidades familiares (cuidado de 
                                 niños o personas dependientes) permanentes o temporales", 
                             if_else(motiv_no_bus_trab %in% c(8,10), "Por motivos de salud permanentes o temporales", 
                                                             NA_character_))))))

#Limpieza variables de interés (definición NAs para valor "999" para var. cine)

datos_proc[datos_proc == 999] <- NA                                              

#Guardar BDD procesada

saveRDS(datos_proc, file= "output/data/datos_proc.rds")



