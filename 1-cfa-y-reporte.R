rm(list = ls())

library(tidyverse)
library(psych)
library(mice) #para la imputación
library(lavaan)
library(here)
library(googlesheets4)
library(rio)
library(semTools)

source(here("0-funciones-nuevas.R"))
invertir <- function(x, i) i+1 - x #para invertir

#cargamos resultados descriptivos
# load(here("3-reportes", "1-descriptivos", "2-con-pesos", "01-descriptivos-ffaa-pesos.Rdata"))
# tdesc <- tabla3[which(tabla3$estrato == "General"), ]
# tdesc <- mutate(tdesc, prop = round(prop, 1)) #redondeamos

# Generar excel con los indicadores de ajuste y las cargas factoriales #

# (0) importamos bases de datos ----
lista = rio::import_list(Sys.glob(here("Bases ejemplo", "*.sav")), setclass = "tibble") %>%
  map(.,~rio::factorize(.))

# quitar posprueba 6P y 2S ise pp
lista <- discard(lista, str_detect(names(lista), "ISE|6Pestudiante_EBR_PP"))

#acomodaciones
#colapsar opciones 'Regularmente' y 'Siempre' en la pregunta p20
# lista$EVA2021_2Sdirector_EBRG2 <- lista$EVA2021_2Sdirector_EBRG2 %>%
#   mutate(across(starts_with("p20"), ~fct_recode(.x, "Regularmente" = "Siempre")))

# names(lista)
# bdlistos <- c("EVA2021_2Sestudiante_EBRG1", "EVA2021_6Pestudiante_EBR", "EVA2021_6Pfamilia_EBR",
#               "EVA2021_2Sestudiante_EBR_PPLEC", "EVA2021_2Sestudiante_EBR_PPMAT",
#               "EVA2021_4Pestudiante_EBR_PPLEC", "EVA2021_4Pestudiante_EBR_PPMAT")
#
# lista <- keep(lista, names(lista) %in% bdlistos)

# vv <- c("EVA2021_2Sestudiante_EBRG2")
#vv <- c("EVA2021_2Pfamilia_EBR")
# b <- names(lista)[str_detect(names(lista), "EVA2021_6Pestudiante_EBR")]
# lista <- keep(lista, names(lista) %in% b)

#cargamos matriz
# matriz <- read_sheet("https://docs.google.com/spreadsheets/d/1Ur2phcc84D72tlUw44nhnOYed48BMb2cwyw3jwJzntc/edit#gid=341416768")
matriz <- import(here("Bases ejemplo","MIAU2021.xlsx"))
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
confi1_cfa <- NULL; confi2_cfa <- NULL

indicad2 <- NULL; carga2 <- NULL
indicad2_pca <- NULL; carga2_pca <- NULL
confi1_pca <- NULL; confi2_pca <- NULL

invarianza2 <- NULL

nom <- names(matriz_lista) #para filtrar
# lista <- map(lista, ~rowid_to_column(.x, "id")) #agregar id a las bases

quieres_puntajes <- FALSE #(!) IMPORTANTE: si es TRUE genera puntaje, si es FALSE nop
quieres_excel <- TRUE

ins <- map(lista, function(x) NULL) #para guardar los insumos del RT
warn <- map(lista, function(x) NULL)

{
  inicio <- Sys.time()
  for(i in 1:length(nom)){ #i=1

    #Preparamos los insumos/variables para la rutina de la base/cuestionario 'i'
    matriz_i <- select(matriz_lista[[nom[i]]], starts_with(c("cod", "Cod")), Analisis2, Invertir,
                       Enunciado, Constructo, OpcionE, sub_escala)
    vcod_indice <- unique(matriz_i$Cod_indice) #escalas del cuestionario i
    tipo <- distinct(matriz_i, Cod_indice, .keep_all = T)$Analisis2 #para identifcar pca o cfa despues

    bd <- lista[[nom[i]]] #tomamos la base i

    # tdesci <- tdesc[which(tdesc$Concatena1 == nom[i]), ] #descriptivos

    for(j in 1:length(vcod_indice)){ #j=1

      #Rutina para la escala 'j' de la base 'i'
      escala_j <- matriz_i[which(matriz_i$Cod_indice == vcod_indice[j]), ]
      preg <- escala_j[c("cod_preg", "Cod_indice", "Cod_indice2")] #id de la escala
      enunciado <- escala_j[c("cod_preg", "Enunciado")] #enunciados de la escala
      elim_opc <- unique(escala_j$OpcionE)
      constructo_j <- unique(escala_j$Constructo)
      cod_constructo <- unique(escala_j$Cod_indice)
      variables <- c("id","estrato") #le añadi esta variable para que pueda identificar cuales serían las columnas que añadiríamos a la base (además del ID)
      bd1 <- bd[c(variables,preg$cod_preg)] #base con id para pegar los puntajes a la base

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
        bd3 <- mutate(bd3, across(all_of(items_invertir), ~invertir(.x, max(select(bd3,-variables)))))
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
      }else{mm=NULL}

      #reporte insumos aplica cfa o pca segun tipo[j]
      resultados1 <- reporte_insumos(select(bd3,-variables), tipo = tipo[j], model_lavaan = mm, puntajes = quieres_puntajes)

      if(tipo[j] == "PCA"){ #Si se quiere PCA
        if(resultados1$indicadores<50){ #Si no cumple esta primera condición, ya no pasa por el repeat
          bd4=bd3
          resultados2=resultados1
        repeat{
          if(length(names(bd4[-1]))<=4){break}
          if(resultados2$indicadores<50){
            if(length(resultados2$cargas %>%  filter(abs(Cargas)<0.4) %>% select(Item) %>% pull())==0){
              eliminar=resultados2$cargas %>%  filter(Cargas==min(abs(Cargas))) %>% select(Cargas) %>% pull()
            }else{
              eliminar=resultados2$cargas %>%  filter(abs(Cargas)<0.4) %>% select(Item) %>% pull()
            }
            bd4=bd4 %>% select(!eliminar)
            resultados2 <- reporte_insumos(select(bd4,-variables), tipo = tipo[j], model_lavaan = mm, puntajes = quieres_puntajes)
          }else{break}
        }
        }else{resultados2=NULL}
      }else{#Si se quiere CFA de uno o dos niveles
        if(resultados1$indicadores[1,2]<0.95|resultados1$indicadores[2,2]<0.95|resultados1$indicadores[3,2]>0.10|resultados1$indicadores[4,2]>0.10){ #Si no cumple esta primera condición, ya no pasa por el repeat
        resultados2=resultados1
        preg2=preg
        repeat{
          if(length(preg2$cod_preg)<=4){break}
          if(resultados2$indicadores[1,2]<0.95|resultados2$indicadores[2,2]<0.95|resultados2$indicadores[3,2]>0.10|resultados2$indicadores[4,2]>0.10){

            if(length(resultados2$cargas %>%  filter(Est<0.4) %>% select(Item) %>% pull())==0){
              eliminar=resultados2$cargas %>%  filter(Est==min(Est)) %>% select(Item) %>% pull()
            }else{
              eliminar=resultados2$cargas %>%  filter(Est<0.4) %>% select(Item) %>% pull()
            }

            preg2=preg2 %>% filter(!cod_preg %in% all_of(eliminar))

            if(tipo[j] == "CFA"){
              if(length(unique(preg2$Cod_indice2)) == 1){
                mm <- paste(unique(preg2$Cod_indice),
                            paste(preg2$cod_preg, collapse = '+'), sep = '=~')
              }else{
                mm <- split(preg2, preg2$Cod_indice2) %>%
                  map(~paste(pull(.x, cod_preg), collapse = "+")) %>%
                  imap(~paste(.y, .x, sep = '=~')) %>%
                  paste(collapse = "\n")
                constructo_j <- unique(escala_j$sub_escala)
                cod_constructo <- unique(escala_j$Cod_indice2)
              }
            }else{mm=NULL}
            resultados2 <- reporte_insumos(select(bd3,-variables), tipo = tipo[j],model_lavaan = mm, puntajes = quieres_puntajes)
          }else{break}
        }
        }else{resultados2=NULL}
      }

      #Saco invarianza
      if(tipo[j] == "CFA"){
      if(is.null(resultados2)){
        resultados1$invarianza=invarianza1(mm,bd3,"estrato")
      }else{resultados2$invarianza=invarianza1(mm,bd3,"estrato")}
      }

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
      # tabla_desc <- tdesci[which(tdesci$cod_preg %in% preg$cod_preg), ] %>%
      #   .[c("cod_preg", "Enunciado", "opcion", "prop")] %>%
      #   pivot_wider(names_from = opcion, values_from = prop)

      #guardamos los insumos
      if(is.null(resultados2)){
        ins[[i]][[vcod_indice[j]]] <- list(resultado_inicial=list(resultados1$cargas, resultados1$indicadores, resultados1$confiabilidad))
      }else{
        ins[[i]][[vcod_indice[j]]] <- list(resultado_inicial=list(resultados1$cargas, resultados1$indicadores, resultados1$confiabilidad),
                                           resultado_recomendado=list(resultados2$cargas, resultados2$indicadores, resultados2$confiabilidad))
      }



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

          #Confiabilidad *****
          confi <- resultados1$confiabilidad %>% pivot_longer(cols = -estadistico,
                                                              names_to = "constructos",
                                                              values_to = "valores") %>%
            mutate(Base = nom[i]) %>% filter(str_detect(estadistico,"omega"))

          confi1_cfa <- bind_rows(confi1_cfa, confi) #generamos la tabla

          if(is.null(resultados2)){confi2_cfa <- bind_rows(confi2_cfa)} #Si no tienen una versión optimizada, se queda como estaba
          else{#De las versiones optimizadas
          #indicadores de ajuste *****
          para_pegar_indica <- resultados2$indicadores %>%
            select(-3) %>%
            pivot_wider(names_from = Indicadores, values_from = Valores) %>%
            mutate(Variable = vcod_indice[j],
                   Constructo = str_flatten(constructo_j,"-"),
                   Base = nom[i]) %>%
            select(Base, Constructo, Variable, everything())

          indicad2 <- bind_rows(indicad2, para_pegar_indica) #generamos la tabla

          #cargas factoriales *****
          para_pegar_cargas <- resultados2$cargas %>%
            mutate(Base = nom[i]) %>%
            left_join(., enunciado, by = c("Item" = "cod_preg")) %>%
            select(Base, Variable = Escala, Item, Enunciado, Est, SE, sig.)

          carga2 <- bind_rows(carga2, para_pegar_cargas) #generamos la tabla

          #Confiabilidad *****
          confi <- resultados2$confiabilidad %>% pivot_longer(cols = -estadistico,
                                                                          names_to = "constructos",
                                                                          values_to = "valores") %>%
            mutate(Base = nom[i]) %>% filter(str_detect(estadistico,"omega"))

          confi2_cfa <- bind_rows(confi2_cfa, confi) #generamos la tabla
          }

          #Saco invarianza
          invarianza=invarianza1(mm,bd3,"estrato") %>% mutate(Variable = vcod_indice[j], Base = nom[i])
          invarianza2=bind_rows(invarianza2,invarianza)

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

          para_pegar_confi <- data.frame(
            Variable = vcod_indice[j],
            Constructo = constructo_j,
            Base = nom[i],
            confiabilidad_alpha_ord = resultados1$confiabilidad)

          confi1_pca <- bind_rows(confi1_pca, para_pegar_confi) #generamos la tabla

          if(is.null(resultados2)){confi2_pca <- bind_rows(confi2_pca)}else{#De las versiones optimizadas
          para_pegar_indica_pca <- data.frame(
            Variable = vcod_indice[j],
            Constructo = constructo_j,
            Base = nom[i],
            Varianza_explicada = resultados2$indicadores) %>%
            select(Base, Constructo, Variable, everything())

          indicad2_pca <- bind_rows(indicad2_pca, para_pegar_indica_pca) #generamos la tabla

          para_pegar_cargas_pca <- resultados2$cargas %>%
            select(Item, Cargas) %>%
            mutate(Variable = vcod_indice[j], Base = nom[i]) %>%
            left_join(.,enunciado,by=c("Item"="cod_preg")) %>%
            select(Base, Variable, Enunciado, everything())

          carga2_pca <- bind_rows(carga2_pca, para_pegar_cargas_pca) #generamos la tabla

          para_pegar_confi <- data.frame(
            Variable = vcod_indice[j],
            Constructo = constructo_j,
            Base = nom[i],
            confiabilidad_alpha_ord = resultados2$confiabilidad)

          confi2_pca <- bind_rows(confi2_pca, para_pegar_confi) #generamos la tabla
          }


        }


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
save(insumos2, file = here("0-ins-reporte.Rdata")) #guardamos


# dd <- format(Sys.Date(), "%d%m%y")
#format(Sys.time(), "%H:%M")

export(list("CFA_indicadores de ajuste" = indicad1,"CFA_indicadores de ajuste_modif" = indicad2,
            "CFA_cargas factoriales" = carga1,"CFA_cargas factoriales_modif" = carga2,
            "CFA_confiabilidad" = confi1_cfa,"CFA_confiabilidad_modif" = confi2_cfa,
            "PCA_indicadores" = indicad1_pca,"PCA_indicadores_modif" = indicad2_pca,
            "PCA_cargas" = carga1_pca,"PCA_cargas_modif" = carga2_pca,
            "PCA_confiabilidad" = confi1_pca,"PCA_confiabilidad_modif" = confi2_pca,"invarianza"=invarianza2),
       here("0-indicadores-ffaa3.xlsx"))


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
