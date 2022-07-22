rm(list = ls())

library(tidyverse)
library(psych)
library(mice) #para la imputación
library(lavaan)
library(here)
library(googlesheets4)
library(rio)

source(here("0-insumos", "0-funciones-nuevas.R"))
invertir <- function(x, i) i+1 - x #para invertir

#cargamos resultados descriptivos
load(here("3-reportes", "1-descriptivos", "2-con-pesos", "01-descriptivos-ffaa-pesos.Rdata"))
tdesc <- tabla3[which(tabla3$estrato == "General"), ]
tdesc <- mutate(tdesc, prop = round(prop, 1)) #redondeamos

# Generar excel con los indicadores de ajuste y las cargas factoriales #

# (0) importamos bases de datos ----
lista = rio::import_list(Sys.glob(here("1-bases", "1-ffaa", "2-para-usar", "*.sav")), setclass = "tibble") %>%
  map(.,~rio::factorize(.))

# quitar posprueba 6P y 2S ise pp
lista <- discard(lista, str_detect(names(lista), "ISE|6Pestudiante_EBR_PP"))

#acomodaciones
#colapsar opciones 'Regularmente' y 'Siempre' en la pregunta p20
lista$EVA2021_2Sdirector_EBRG2 <- lista$EVA2021_2Sdirector_EBRG2 %>%
  mutate(across(starts_with("p20"), ~fct_recode(.x, "Regularmente" = "Siempre")))

# names(lista)
# bdlistos <- c("EVA2021_2Sestudiante_EBRG1", "EVA2021_6Pestudiante_EBR", "EVA2021_6Pfamilia_EBR",
#               "EVA2021_2Sestudiante_EBR_PPLEC", "EVA2021_2Sestudiante_EBR_PPMAT",
#               "EVA2021_4Pestudiante_EBR_PPLEC", "EVA2021_4Pestudiante_EBR_PPMAT")
#
# lista <- keep(lista, names(lista) %in% bdlistos)

vv <- c("EVA2021_2Sestudiante_EBRG2")
#vv <- c("EVA2021_2Pfamilia_EBR")
# b <- names(lista)[str_detect(names(lista), "EVA2021_6Pestudiante_EBR")]
# lista <- keep(lista, names(lista) %in% b)

#cargamos matriz
matriz <- read_sheet("https://docs.google.com/spreadsheets/d/1Ur2phcc84D72tlUw44nhnOYed48BMb2cwyw3jwJzntc/edit#gid=341416768")

#nos quedamos con la info asociada a cfa, pca
matriz1 <- filter(matriz, Analisis2 %in% c("CFA", "PCA")) %>%
  rename(Constructo = Constructo_indicador)

#mismas bases en miau y lista
matriz1 <- matriz1 %>% filter(Concatena1 %in% names(lista))
lista <- keep(lista, names(lista) %in% matriz1$Concatena1)

matriz_lista <- split(matriz1, matriz1$Concatena1) #separamos matriz en listas
matriz_lista <- matriz_lista[names(lista)] #mismo orden

#comprobemos si estan iguales
length(names(matriz_lista)) == length(names(lista)) #misma cantidad de elementos
setequal(names(matriz_lista), names(lista)) #mismos elementos sin importar el orden
identical(names(matriz_lista), names(lista)) #exactamente mismos elementos

#para guardar
indicad1 <- NULL; carga1 <- NULL
indicad1_pca <- NULL; carga1_pca <- NULL

nom <- names(matriz_lista) #para filtrar
lista <- map(lista, ~rowid_to_column(.x, "id")) #agregar id a las bases

quieres_puntajes <- FALSE #(!) IMPORTANTE: si es TRUE genera puntaje, si es FALSE nop
quieres_excel <- TRUE

ins <- map(lista, function(x) NULL) #para guardar los insumos del RT
warn <- map(lista, function(x) NULL)

{
  inicio <- Sys.time()
  for(i in 1:length(nom)){ #i=8

    #Preparamos los insumos/variables para la rutina de la base/cuestionario 'i'
    matriz_i <- select(matriz_lista[[nom[i]]], starts_with(c("cod", "Cod")), Analisis2, Invertir,
                       Enunciado, Constructo, OpcionE, sub_escala)
    vcod_indice <- unique(matriz_i$Cod_indice) #escalas del cuestionario i
    tipo <- distinct(matriz_i, Cod_indice, .keep_all = T)$Analisis2 #para identifcar pca o cfa despues

    bd <- lista[[nom[i]]] #tomamos la base i

    tdesci <- tdesc[which(tdesc$Concatena1 == nom[i]), ] #descriptivos

    for(j in 1:length(vcod_indice)){ #j=4

      #Rutina para la escala 'j' de la base 'i'
      escala_j <- matriz_i[which(matriz_i$Cod_indice == vcod_indice[j]), ]
      preg <- escala_j[c("cod_preg", "Cod_indice", "Cod_indice2")] #id de la escala
      enunciado <- escala_j[c("cod_preg", "Enunciado")] #enunciados de la escala
      elim_opc <- unique(escala_j$OpcionE)
      constructo_j <- unique(escala_j$Constructo)
      cod_constructo <- unique(escala_j$Cod_indice)

      bd1 <- bd[c("id", preg$cod_preg)] #base con id para pegar los puntajes a la base

      bd2 <- bd1 %>%
        #identificar observaciones con mas de 25% de missing y retirarlas
        filter(apply(.[preg$cod_preg], 1, function(x) mean(is.na(x))) < 0.25) %>%
        mutate(across(all_of(preg$cod_preg), as.numeric))

      #imputación **********
      pred <- mice(bd2, maxit = 0, print = F)$predictorMatrix #'falso' mice, para excluir id
      pred[,'id'] <- 0 #excluir id de la prediccion
      mice_data <- mice(bd2, m = 5, maxit = 5, meth = 'pmm', predictorMatrix = pred, seed = 343, print = FALSE) #imputacion
      bd3 <- as_tibble(complete(mice_data, 1)) # bd3: datos imputados

      #para invertir
      if(any(!is.na(escala_j$Invertir))){
        items_invertir <- filter(escala_j, Invertir == "Invertir")$cod_preg
        bd3 <- mutate(bd3, across(all_of(items_invertir), ~invertir(.x, max(bd3[-1]))))
      }

      #para eliminar la opcion que no entra al modelo
      if(!is.na(elim_opc)){
        bd3 <- filter(bd3, across(all_of(preg$cod_preg), ~.x != elim_opc))
        bd3 <- mutate(bd3, across(all_of(preg$cod_preg), ~.x - 1))
      }

      #acomodamos string para la funcion de lavaan
      # y de paso los labels y cod_indices segun n factores
      if(tipo[j] == "CFA"){
        if(length(unique(preg$Cod_indice2)) == 1){
          mm <- paste(unique(preg$Cod_indice), paste(preg$cod_preg, collapse = '+'), sep = '=~')
        }else{
          mm <- split(preg, preg$Cod_indice2) %>%
            map(~paste(pull(.x, cod_preg), collapse = "+")) %>%
            imap(~paste(.y, .x, sep = '=~')) %>%
            paste(collapse = "\n")
          constructo_j <- unique(escala_j$sub_escala)
          cod_constructo <- unique(escala_j$Cod_indice2)
        }
      }

      #reporte insumos aplica cfa o pca segun tipo[j]
      resultados1 <- reporte_insumos(bd3[-1], tipo = tipo[j], model_lavaan = mm, puntajes = quieres_puntajes)

      mm <- cfa(mm, data = bd3[-1], ordered = TRUE, mimic = "Mplus", estimator = "WLSMV")
      summary(mm)
      lavaan::parameterEstimates(mm)

      #para identificar los warnings
      # warn[[i]][[vcod_indice[j]]]  <-
      #   tryCatch(reporte_insumos(bd3[-1], tipo = tipo[j], model_lavaan = mm),
      #            error = function(e) e,
      #            warning = function(w) w )


      if(quieres_puntajes != FALSE){
        if(tipo[j] == "PCA"){ #nombre a la columna generada con PCA
          resultados1$puntajes <- setNames(as.data.frame(resultados1$puntajes), unique(preg$Cod_indice))
        }

        #mean = 0, sd = 1 [scale() usa matrices, nos desconfigura las cosas]
        resultados1$puntajes <- as.data.frame(apply(resultados1$puntajes, 2, function(x) (x - mean(x))/sd(x) ))

        lista[[i]] <-  lista[[i]] %>% #pegado de puntajes a base inicial
          left_join(
            bind_cols(bd3[1], resultados1$puntajes), by = "id") %>%
          asigna_label(constructo_j, cod_constructo)
      }

      #tabla de descriptivos ********
      tabla_desc <- tdesci[which(tdesci$cod_preg %in% preg$cod_preg), ] %>%
        .[c("cod_preg", "Enunciado", "opcion", "prop")] %>%
        pivot_wider(names_from = opcion, values_from = prop)

      #guardamos los insumos
      ins[[i]][[vcod_indice[j]]] <- list(tabla_desc, resultados1$cargas, resultados1$indicadores)

      if(quieres_excel == TRUE){
        # PARA EL EXCEL ****************************************************************************************

        if(tipo[j] == "CFA"){
          #indicadores de ajuste *****
          para_pegar_indica <- resultados1$indicadores %>%
            select(-3) %>%
            pivot_wider(names_from = Indicadores, values_from = Valores) %>%
            mutate(Variable = vcod_indice[j],
                   Constructo = str_flatten(constructo_j,"-"),
                   Base = nom[i]) %>%
            select(Base, Constructo, Variable, everything())

          indicad1 <- bind_rows(indicad1, para_pegar_indica) #generamos la tabla

          #cargas factoriales *****
          para_pegar_cargas <- resultados1$cargas %>%
            mutate(Base = nom[i]) %>%
            left_join(., enunciado, by = c("Item" = "cod_preg")) %>%
            select(Base, Variable = Escala, Item, Enunciado, Est, SE, sig.)

          carga1 <- bind_rows(carga1, para_pegar_cargas) #generamos la tabla

        }else{
          para_pegar_indica_pca <- data.frame(
            Variable = vcod_indice[j],
            Constructo = constructo_j,
            Base = nom[i],
            Varianza_explicada = resultados1$indicadores) %>%
            select(Base, Constructo, Variable, everything())

          indicad1_pca <- bind_rows(indicad1_pca, para_pegar_indica_pca) #generamos la tabla

          para_pegar_cargas_pca <- resultados1$cargas %>%
            select(Item, Cargas) %>%
            mutate(Variable = vcod_indice[j], Base = nom[i]) %>%
            left_join(.,enunciado,by=c("Item"="cod_preg")) %>%
            select(Base, Variable, Enunciado, everything())

          carga1_pca <- bind_rows(carga1_pca, para_pegar_cargas_pca) #generamos la tabla
        }
        #*******************************************************************************************************
      }
      #Sys.sleep(1)
    }

  }

  final <- Sys.time()
  final - inicio
}

# map_depth(warn, 2, length) %>% flatten() %>% keep(~.x < 3)
#

if(quieres_puntajes != FALSE){
  rio::export_list(lista, here("1-bases","1-ffaa", "3-con-puntajes", paste0(names(lista), ".sav")))
}

# export(lista$EVA2021_6Pestudiante_EBR, here("1-bases","1-ffaa", "3-con-puntajes", paste0(names(lista), ".sav")))
# names(lista$EVA2021_6Pestudiante_EBR)

#
# # lista$EVA2021_2Sdirector_EBRG1$DIR2SGEN_DSED %>% mean()
# # lista$EVA2021_2Sdirector_EBRG1$DIR2SGEN_DSED %>% hist()
# #
# # #insumos adicionales para el reporte -----
# #
# #vector con los nombres de las escalas para filtrar en ins
cod_tipo <- matriz1 %>%
  distinct(Cod_indice, .keep_all = T) %>%
  split(.$Analisis2) %>%
  map(~select(.x, Cod_indice)) %>%
  map(~apply(.x, 2, paste, collapse = "|"))

#
# #acomodaciones para los textos
text_por_escala <- matriz1 %>%
  mutate(cod_indice3 = ifelse(is.na(Cod_indice2), Cod_indice, Cod_indice2)) %>%
  group_by(Concatena1, Cod_indice) %>%
  #para identificar si tiene mas de un factor
  mutate(nfac = n_distinct(cod_indice3)) %>%
  distinct(Concatena1, cod_indice3, .keep_all = TRUE) %>%
  #texto_2: subescala con definicion:
  mutate(texto_2 = ifelse(nfac > 1, paste0("**", sub_escala, " (",Cod_indice2,")", "**", ": ", Definicion, "\\"), NA)) %>%
  group_by(Concatena1, Cod_indice) %>%
  #juntamos todo el string de texto_2
  mutate(texto_2 = ifelse(nfac > 1, paste0(texto_2, collapse = "\n"), NA)) %>%
  # acomodamos los otros textos:
  distinct(Cod_indice, .keep_all = TRUE) %>%
  mutate(texto = ifelse(nfac > 1,
                        paste0("Los ítems conformaron las siguientes escalas:\\","\n\n",texto_2,"\n\n","La pregunta que se realizó al ",Dirigido," fue la siguiente: ","*",Pregunta,"*"),
                        glue::glue("Los ítems hacen referencia a '{Definicion}'. La pregunta que se realizó al {Dirigido} fue la siguiente: *{Pregunta}*")),
         tit_escala = ifelse(nfac > 1, glue::glue("### {Constructo}"), glue::glue("### {Constructo} ({Cod_indice})")),
         encab_cuest = glue::glue("# {Instrumento}")) %>%
  select(Concatena1, Cod_indice, Analisis2, texto, tit_escala, encab_cuest, nfac, Instrumento)
#
insumos2 <- list(insumos = ins, tipo = cod_tipo, textos = text_por_escala)
save(insumos2, file = here("3-reportes", "2-psicometricos", "0-ins-reporte.Rdata")) #guardamos


# dd <- format(Sys.Date(), "%d%m%y")
#format(Sys.time(), "%H:%M")

export(list("CFA_indicadores de ajuste" = indicad1,
            "CFA_cargas factoriales" = carga1,
            "PCA_indicadores" = indicad1_pca,
            "PCA_cargas" = carga1_pca),
       here("3-reportes", "2-psicometricos", "0-indicadores-ffaa3.xlsx"))


#*****************************************


# for(i in 1:5){
#   cat("Step", i, "out of 5\r")
#   Sys.sleep(1) # Sleep for one second
# }
#
#
# install.packages("beepr")
#
# library(beepr)
# # Play all 11 sounds available in beepr:
# for(i in 1:11){
#   beep(sound = i)
#   Sys.sleep(2) # Sleep for 2 seconds
# }
#
