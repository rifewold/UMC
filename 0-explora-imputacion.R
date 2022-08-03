rm(list = ls())

library(tidyverse)
rowSumsNA <- function(x) rowSums(is.na(x)) #contar NA en las observaciones

# imputacion (?)

# (0) importamos bases de datos ----
lista = rio::import_list(Sys.glob(here("Bases ejemplo", "*.sav")), setclass = "tibble") %>%
  map(.,~rio::factorize(.))

matriz <- import(here("Bases ejemplo","MIAU2021.xlsx"))

matriz_cfa <- matriz %>%
  filter(Analisis2 %in% c("CFA", "PCA")) %>%
  select(Concatena1, Constructo = Constructo_indicador,
         sub_escala, starts_with("Cod"), Enunciado,
         Analisis2, Invertir, OpcionE)


#mismas bases en miau y lista
matriz_cfa <- matriz_cfa %>% filter(Concatena1 %in% names(lista))
lista <- keep(lista, names(lista) %in% matriz_cfa$Concatena1)

matriz_lista <- split(matriz_cfa, matriz_cfa$Concatena1) #separamos matriz en listas
matriz_lista <- matriz_lista[names(lista)] #mismo orden

#comprobemos si estan iguales
length(names(matriz_lista)) == length(names(lista)) #misma cantidad de elementos
setequal(names(matriz_lista), names(lista)) #mismos elementos sin importar el orden
identical(names(matriz_lista), names(lista)) #exactamente mismos elementos

nom <- names(matriz_lista) #para filtrar

for(i in 1:length(nom)){ #i=2

  #Preparamos los insumos/variables para la rutina de la base/cuestionario 'i'
  matriz_i <- matriz_lista[[nom[i]]]
  vcod_indice <- unique(matriz_i$Cod_indice) #escalas del cuestionario i

  bd <- lista[[nom[i]]] #tomamos la base i

  for(j in 1:length(vcod_indice)){ #j=1

    #Rutina para la escala 'j' de la base 'i'
    escala_j <- matriz_i[which(matriz_i$Cod_indice == vcod_indice[j]), ]
    preg <- escala_j[c("cod_preg", "Cod_indice", "Cod_indice2")] #id de la escala
    variables <- c("id") #le añadi esta variable para que pueda identificar cuales serían las columnas que añadiríamos a la base (además del ID)
    bd1 <- bd[c(variables, preg$cod_preg)] #base con id para pegar los puntajes a la base

    # cuantos missing (?)

    nrow(bd1)

    sapply(bd1, function(x) mean(is.na(x)))

    bd1_b  <- bd1 %>%
      mutate(sum_nas = rowSumsNA(across(all_of(preg$cod_preg))),
             porc_nas = sum_nas/length(preg$cod_preg),
             con_algun_missing = ifelse(porc_nas > 0, 1, 0),
             se_recupera_con_imputacion = ifelse(porc_nas > 0 & porc_nas < 0.25, 1, 0),
             completo = ifelse(porc_nas == 0, 1, 0))

    tabla_info_missing <- data.frame(
      observaciones = c("Total", "Completas", "Incompletas", "Casos imputados"),
      cantidad = c(nrow(bd1_b), sum(bd1_b$completo), sum(bd1_b$con_algun_missing), sum(bd1_b$se_recupera_con_imputacion)),
      porcentaje = c(100,
                     round(sum(bd1_b$completo)/nrow(bd1_b)*100, 2),
                     round(sum(bd1_b$con_algun_missing)/nrow(bd1_b)*100,2),
                     round(sum(bd1_b$se_recupera_con_imputacion)/nrow(bd1_b)*100, 2)
                     )
    )

    bd2 <- bd1 %>%
      #identificar observaciones con mas de 25% de missing y retirarlas
      filter(apply(.[preg$cod_preg], 1, function(x) mean(is.na(x))) < 0.25) %>%
      mutate(across(all_of(preg$cod_preg), as.numeric))

    # cuantos missing (?)


    #imputación **********
    pred <- mice(bd2, maxit = 0, print = F)$predictorMatrix #'falso' mice, para excluir id
    pred[,'id'] <- 0 #excluir id de la prediccion
    mice_data <- mice(bd2, m = 5, maxit = 5, meth = 'pmm', predictorMatrix = pred, seed = 343, print = FALSE) #imputacion
    bd3 <- as_tibble(complete(mice_data, 1)) # bd3: datos imputados


