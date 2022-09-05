rm(list = ls())

library(tidyverse)
library(here)
library(rio)
library(mice)
rowSumsNA <- function(x) rowSums(is.na(x)) #contar NA en las observaciones
invertir <- function(x, i) i+1 - x #para invertir

chequeo <- function(mc){

  c1 <- apply(mc, 1, function(x) all(x[x != 1] > 0)) #todas las correlaciones son positivas?
  c2 <- apply(mc, 1, function(x) any(x[x != 1] > 0)) #hay algun positivo?
  c3 <- apply(mc, 1, function(x) any(x[x != 1] > 0.30)) #todas las correlaciones son mayores a .30?

  tabla1 <- data.frame(
    item = names(c1),
    todo_positivo = c1,
    hay_positivo = c2,
    todos_min_30 = c3,
    KMO_item = psych::KMO(mc)$MSAi,
    KMO = psych::KMO(mc)$MSA)

  return(tabla1)

}

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

datos_imputados <- map(lista, function(x) NULL) #para guardar los insumos del RT
datos_imputados_lista <- NULL
tabla_info_missing_pegar <- NULL
bd3_corr_check_lista <- map(lista, function(x) NULL) #para guardar los insumos del RT

for(i in 1:length(nom)){ #i=1

  #Preparamos los insumos/variables para la rutina de la base/cuestionario 'i'
  matriz_i <- matriz_lista[[nom[i]]]
  vcod_indice <- unique(matriz_i$Cod_indice) #escalas del cuestionario i

  bd <- lista[[nom[i]]] #tomamos la base i

  for(j in 1:length(vcod_indice)){ #j=2

    #Rutina para la escala 'j' de la base 'i'
    escala_j <- matriz_i[which(matriz_i$Cod_indice == vcod_indice[j]), ]
    preg <- escala_j[c("cod_preg", "Cod_indice", "Cod_indice2")] #id de la escala
    constructo_j <- unique(escala_j$Constructo)
    elim_opc <- unique(escala_j$OpcionE)
    cod_constructo <- unique(escala_j$Cod_indice)
    variables <- c("id") #le añadi esta variable para que pueda identificar cuales serían las columnas que añadiríamos a la base (además del ID)
    bd1 <- bd[c(variables, preg$cod_preg)] #base con id para pegar los puntajes a la base

    # cuantos missing (?)
    #sapply(bd1, function(x) mean(is.na(x)))

    # hay pocos missing, generar algunos
    # https://stackoverflow.com/questions/30904564/generate-random-missing-values-in-a-dataframe-using-r
    p = 0.10
    sel <- sample( nrow(bd1)*ncol(bd1), size = p*nrow(bd1)*ncol(bd1) )
    for(t in 1:length(sel)){ # t=1
      is.na(bd1[[sel[t]%/%nrow(bd1) +1]]) <- sel[t]%%nrow(bd1) + 1
    }

    # sapply(bd1, function(x) mean(is.na(x)))

    # le puso missing al id tambien... pongamos otro id
    bd1 <- rowid_to_column(bd1, "id2")

    bd1_b <- bd1 %>%
      mutate(sum_nas = rowSumsNA(across(all_of(preg$cod_preg))),
             porc_nas = sum_nas/length(preg$cod_preg),
             nas_completo = ifelse(porc_nas == 0, 1, 0),
             nas_alguno = ifelse(porc_nas > 0, 1, 0),
             nas_se_recupera = ifelse(porc_nas > 0 & porc_nas < 0.25, 1, 0),
             nas_missing = ifelse(porc_nas >= 0.25, 1, 0)
             )

    tabla_info_missing <- bd1_b %>%
      summarise(across(starts_with("nas"), ~round(sum(.x)/nrow(bd1_b)*100, 2))) %>%
      rename(Completos = 1, Incompletos = 2, Casos_imputados = 3, Porcentaje_missing = 4) %>%
      bind_cols(cod_indice = cod_constructo,
                constructo = constructo_j,
                Total = nrow(bd1_b), .)

    bd2 <- bd1 %>%
      #identificar observaciones con mas de 25% de missing y retirarlas
      filter(apply(.[preg$cod_preg], 1, function(x) mean(is.na(x))) < 0.25) %>%
      mutate(across(all_of(preg$cod_preg), as.numeric)) %>%
      select(-id)

    # (1 - nrow(bd2)/nrow(bd1))*100

    #imputación **********
    pred <- mice(bd2, maxit = 0, print = F)$predictorMatrix #'falso' mice, para excluir id
    pred[,'id2'] <- 0 #excluir id de la prediccion

    mice_data <- mice(bd2, m = 1, maxit = 20, meth = 'pmm', predictorMatrix = pred, seed = 343, print = FALSE) #imputacion
    bd3 <- as_tibble(complete(mice_data, 1)) # bd3: datos imputados
    # complete(mice_data, 1) #de esta manera tomamos la primera imputacion de las 5 que hacemos....
    # entonces imputamos porlas...
    # debemos usar toda la informacion ? o solo una

    #para invertir
    if(any(!is.na(escala_j$Invertir))){
      items_invertir <- filter(escala_j, Invertir == "Invertir")$cod_preg
      bd3 <- mutate(bd3, across(all_of(items_invertir), ~invertir(.x, max(select(bd3, -id2)))))
    }

    #para eliminar la opcion que no entra al modelo
    if(!is.na(elim_opc)){
      bd3 <- filter(bd3, across(all_of(preg$cod_preg), ~.x != elim_opc))
      bd3 <- mutate(bd3, across(all_of(preg$cod_preg), ~.x - 1))
    }

    bd3_corr_check <- preg %>%
      mutate(Cod_indice2 = ifelse(is.na(Cod_indice2), Cod_indice, Cod_indice2)) %>%
      split(., .$Cod_indice2) %>%
      map(1) %>%
      map(~select(bd3, all_of(.x))) %>% #sub_escalas
      map(~psych::polychoric(.x)$rho) %>% #correlacion policor
      map(~chequeo(.x))

    bd3_corr_check_lista[[i]][[j]] <- bd3_corr_check

    datos_imputados[[i]][[j]] <- bd3
    tabla_info_missing_pegar <- bind_rows(tabla_info_missing_pegar, tabla_info_missing)

  }

    datos_imputados_lista[[i]] <- datos_imputados[[i]] %>%
      reduce(full_join, by = "id2") %>%
      left_join(lista[[i]][1], ., by = c("id" = "id2"))

}


bd3_corr_check_lista
tabla_info_missing_pegar

# podemos guardar esto en un excel:
map(bd3_corr_check_lista, flatten) %>%
  map(~pega_lista(.x, "constructo")) %>%
  pega_lista("base")

tabla_info_missing_pegar

# y guardamos datos imputados:
datos_imputados_lista

#************************************************

lista$EVA2021_2Sestudiante_EBRG1$p01
attr(lista$EVA2021_2Sestudiante_EBRG1$p02_01, "Pregunta") <- "A continuación encontrarás varios enunciados que describen el lugar dentro de tu casa donde estudias y haces tus tareas. Marca “Sí” o “No” en cada enunciado según corresponda con tu caso"
attr(lista$EVA2021_2Sestudiante_EBRG1$p02_01, "Enunciado") <- "Hay limpieza y orden en el lugar donde estudio"
attr(lista$EVA2021_2Sestudiante_EBRG1$p02_01)

attr(y, "my_attribute") <- "This is a vector"
attr(lista$EVA2021_2Sestudiante_EBRG1$p02_01, "Pregunta")
attr(lista$EVA2021_2Sestudiante_EBRG1$p02_01, "Enunciado")

ff <- lista$EVA2021_2Sestudiante_EBRG1

export(ff, "ejemplo.sav")

map_df(ff, ~attr(.x, "Enunciado")) %>%
  gather()






