rm(list=ls())

library(tidyverse)
library(here)
library(rio)
library(googlesheets4)
source(here("0-insumos", "0-funciones-nuevas.R"))

# (1) importamos bases de datos ----
lista = import_list(Sys.glob(here("1-bases", "2-rendimiento", "02-pegado-con-ffaa", "*.sav")), setclass = "tibble") %>% 
  map(.,~rio::factorize(.))

lista <- set_names(lista, str_remove(names(lista), "_r"))

# matriz
matriz <- read_sheet("https://docs.google.com/spreadsheets/d/1Ur2phcc84D72tlUw44nhnOYed48BMb2cwyw3jwJzntc/edit#gid=341416768")

# lista_rend = import_list(Sys.glob(here("1-bases", "2-rendimiento", "*.sav")), setclass = "tibble") %>% 
#   map(.,~rio::factorize(.))
# bdhse <- lista_rend[[1]]; names(bdhse)
# bdhse <- select(bdhse, dni_est = pid, dni_ppff, Peso_HSE)
# lista$EVA2021_2Sestudiante_EBRG2 <- left_join(lista$EVA2021_2Sestudiante_EBRG2, bdhse, by = "dni_est")
# lista$EVA2021_2Sfamilia_EBRG2 <- left_join(lista$EVA2021_2Sfamilia_EBRG2, bdhse, by = "dni_ppff")
# lista <- keep(lista, str_detect(names(lista), "G2"))
# lista <- lista[3:4]
# names(lista$EVA2021_2Sestudiante_EBRG2)

#mismas bases en miau y lista
matriz1 <- matriz %>% filter(Concatena1 %in% names(lista))
matriz1 <- filter(matriz1, TipoV %in% c("Categorico1", "Categorico2"))
lista <- keep(lista, names(lista) %in% matriz1$Concatena1) 

matriz_lista <- split(matriz1, matriz1$Concatena1) #separamos matriz en listas
matriz_lista <- matriz_lista[names(lista)] #mismo orden

lista_vars <- matriz_lista %>% map(~pull(.x, cod_preg))
lista_vars <- map(lista_vars, ~set_names(.x, .x)) #colocarle el nombre
identical(names(lista_vars),  names(lista))

matriz_pegar <- bind_rows(matriz_lista) %>%
  select(Concatena1, Instrumento, cod_gen, cod_preg, Pregunta, Enunciado, TipoV, OpcionL)

#acomodamos pesos y bases
est <- names(lista)[str_detect(names(lista), "estudiante")] #para estudiantes 
lista[est] <- map(lista[est], ~rename(.x, peso = peso_final_lectura))

fam <- names(lista)[str_detect(names(lista), "familia")] #para familias 
lista[fam[-1]] <- map(lista[fam[-1]], ~rename(.x, peso = peso_final_lectura))
lista$EVA2021_2Pfamilia_EBR <- rename(lista$EVA2021_2Pfamilia_EBR, peso = peso_final)

table(lista$EVA2021_2Pfamilia_EBR$estrato)
table(lista$EVA2021_6Pfamilia_EBR$estrato)

dirs <- names(lista)[str_detect(names(lista), "director")] #director 
lista[dirs[-1]] <- map(lista[dirs[-1]], ~mutate(.x, peso = 1/pikIE_lec))
lista[dirs[-1]] <- lista[dirs[-1]] %>% map(~distinct(.x, cod_mod7, .keep_all = TRUE))
lista$EVA2021_2Pdirector_EBR <- lista$EVA2021_2Pdirector_EBR %>% rename(peso = peso_escuela)

docs <- names(lista)[str_detect(names(lista), "docente")] #docente 
lista[docs] <- map(lista[docs], ~mutate(.x, peso = 1))
#docente no tiene peso, pongamos 1 a todos
lista[docs] <- lista[docs] %>% map(~distinct(.x, dni_doc, .keep_all = TRUE))

#quitamos levels que ya no hay
# eso genera algunos problemas con 'aggregate()'
# dentro de tabla_freq uso la funcion aggregate
lista <- map(lista, ~mutate_if(.x, is.factor, factor))

# (1) generamos tablas  ----
# generamos tablas con las proporciones de cada item de cada cuestionario

tabla <- 
  map2(
    lista, lista_vars, function(b, v)
      map(v, ~tabla_freq(b, .x, peso = "peso")) %>% pega_lista("cod_preg")
    ) %>%
  pega_lista("Concatena1") %>% 
  bind_cols(., estrato = "General")

tabla_estrato <- 
  map2(
    lista, lista_vars, function(b, v)
      map(v, ~tabla_freq_estrato(b, .x, "estrato", peso = "peso")) %>% pega_lista("cod_preg")
    ) %>%
  pega_lista("Concatena1")

est <- names(lista)[str_detect(names(lista), "estudiante")] #para estudiantes 

tabla_estudiante_sex <- 
  map2(
    lista[est], lista_vars[est], function(b, v)
      map(v, ~tabla_freq_estrato(b, .x, "sexo", "peso")) %>% pega_lista("cod_preg")
  ) %>%
  pega_lista("Concatena1")

tabla_estudiante_sex <- rename(tabla_estudiante_sex, estrato = sexo)

# pegado

tabla1 <- bind_rows(tabla, tabla_estrato, tabla_estudiante_sex)
tabla2 <- left_join(tabla1, matriz_pegar, by = c("Concatena1", "cod_preg"))

tabla3 <- tabla2 %>%
  select(Instrumento, Concatena1, cod_gen, cod_preg, Pregunta, Enunciado, 
         opcion, n, prop, estrato, TipoV, OpcionL)

#quitamos NA ? 
#esos na son los que tienen pesos pero no tienen fa, 
tabla3 <- drop_na(tabla3, estrato)

# en .rdata
save(tabla3, file = here("3-reportes", "1-descriptivos", "2-con-pesos", "01-descriptivos-ffaa-pesos.Rdata"))

# en excel
tabla_excel <- pivot_wider(tabla3, names_from = estrato, values_from = c(n, prop)) %>%
  select(-OpcionL, -TipoV, -Concatena1)

writexl::write_xlsx(tabla_excel, here("3-reportes", "1-descriptivos", "2-con-pesos", "01-descriptivos-ffaa-pesos.xlsx"))



#**************************************************************************************

# a <- lista_vars$EVA2021_2Sestudiante_EBRG1  
# b <- lista$EVA2021_2Sestudiante_EBRG1
# 
# map(b[a], table)
# 
# tabla <- 
#   map2(
#     lista, lista_vars, function(b, v)
#       map(v, ~tabla_freq(b, .x)) %>% pega_lista("cod_preg")
#   ) %>%
#   pega_lista("Concatena1") %>% 
#   bind_cols(., estrato = "General")






