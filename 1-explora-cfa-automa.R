
# funcion para probar
acomoda_string_lavaan <- function(data_preg){
  if(length(unique(data_preg$Cod_indice2)) == 1){
    mm <- paste(unique(data_preg$Cod_indice), paste(data_preg$cod_preg, collapse = '+'), sep = '=~')

  }else{
    mm <- split(data_preg, data_preg$Cod_indice2) %>%
      map(~paste(pull(.x, cod_preg), collapse = "+")) %>%
      imap(~paste(.y, .x, sep = '=~')) %>%
      paste(collapse = "\n")
  }
  return(mm)
}


lista = rio::import_list(Sys.glob(here("Bases ejemplo", "*.sav")), setclass = "tibble") %>%
  map(.,~rio::factorize(.))

bd <- lista$EVA2021_2Sestudiante_EBRG1
matriz <- import(here("Bases ejemplo","MIAU2021.xlsx"))

bd1 <- bd %>%
  select(starts_with("p11")) %>%
  mutate_all(as.numeric)

preg <- filter(matriz, Concatena1 == "EVA2021_2Sestudiante_EBRG1", Cod_indice == "EST2SMAT_MOTIV") %>%
  select(cod_preg, Cod_indice, Cod_indice2, Analisis2)

mm <- acomoda_string_lavaan(preg)

resultados1 <- reporte_insumos(bd1, tipo = "CFA", model_lavaan = mm, puntajes = FALSE)


#"resultados1$indicadores[1,2] < 0.95"
#eval(parse(text="resultados1$indicadores[1,2] < 0.95"))


if(resultados1$indicadores[1,2] < 0.95 | # cfi
   resultados1$indicadores[2,2] < 0.95 | # tli
   resultados1$indicadores[3,2] > 0.10 | # srmr
   resultados1$indicadores[4,2] > 0.10){ # rmsea

  resultados2 = resultados1
  preg2 = preg

  repeat{
    if(length(preg2$cod_preg) <= 4){break} # si son 4 o menos items, pará

    if(resultados2$indicadores[1,2] < 0.95|
       resultados2$indicadores[2,2] < 0.95|
       resultados2$indicadores[3,2] > 0.10|
       resultados2$indicadores[4,2] > 0.10){

      # identificamos items
      if(nrow(filter(resultados2$cargas, Est < 0.4)) == 0){ # si no hay items con cargas menores a 0.4, identificamos el menor
        eliminar = filter(resultados2$cargas, Est == min(Est))$Item
      }else{
        eliminar = filter(resultados2$cargas, Est < 0.4)$Item # identificamos items con cargas menores a 0.4
      }

      preg2 = filter(preg2, !cod_preg %in% all_of(eliminar)) # nuevo modelo
      mm2 <- acomoda_string_lavaan(preg2) # generamos string con nuevo modelo

      resultados2 <- reporte_insumos(bd1, tipo = "CFA", model_lavaan = mm2, puntajes = FALSE)

    }else{ #cuando cumpla alguno de los criterios, paramos

        break

      }
  }


}



###############

# generamos una funcion a ver que tal

reporte_insumos_cfa_recurs <- function(data, model_lavaan, recursivo = TRUE){

  mod1 <- cfa(model_lavaan, data = data, ordered = TRUE, mimic = "Mplus", estimator = "WLSMV")

  if(recursivo){

    indi <- lavaan::fitmeasures(mod1,  c("cfi", "tli", "srmr", "rmsea"))
    cargafac <- subset(lavaan::parameterEstimates(mod1), op == "=~")


    if(
      any(c(purrr::map_lgl(indi[c("cfi", "tli")], ~.x < 0.95),
            purrr::map_lgl(indi[c("srmr", "rmsea")], ~.x > 0.10)))
    ){

      indi_nueva = indi
      cargafac_nueva = cargafac

      repeat{
        if(nrow(cargafac_nueva) <= 4){break} # si son 4 o menos items, pará

        if(any(c(purrr::map_lgl(indi_nueva[c("cfi", "tli")], ~.x < 0.95),
                 purrr::map_lgl(indi_nueva[c("srmr", "rmsea")], ~.x > 0.10)))){

          # identificamos items
          if(nrow(filter(cargafac_nueva, est < 0.4)) == 0){ # si no hay items con cargas menores a 0.4, identificamos el menor
            eliminar = filter(cargafac_nueva, est == min(est))$rhs
          }else{
            eliminar = filter(cargafac_nueva, est < 0.4)$rhs # identificamos items con cargas menores a 0.4
          }

          cargafac_nueva = filter(cargafac_nueva, !rhs %in% all_of(eliminar)) # nuevo modelo

          modstring <- split(cargafac_nueva, cargafac_nueva$lhs) %>%
            map(~paste(pull(.x, rhs), collapse = "+")) %>%
            imap(~paste(.y, .x, sep = '=~')) %>%
            paste(collapse = "\n")

          mod2 <- cfa(modstring, data = data, ordered = TRUE, mimic = "Mplus", estimator = "WLSMV")

        }else{break} # paramos
      }

      return(mod2)

    }else{

      return(mod1)
    }

  }

  return(mod1)


}

# funciona! :D pero donde va?

modo <- reporte_insumos_cfa_recurs(data = bd1, model_lavaan = mm, recursivo = FALSE)
reporte_lavaan(modo, puntajes = FALSE)



# **********************************************************************************************************


matriz1

matriz_1p <- select(matriz1, Concatena1, starts_with(c("cod", "Cod")), Analisis2, Invertir, Enunciado, Constructo, OpcionE, sub_escala)

matriz_1p_split <- split(matriz_1p, matriz_1p$Concatena1)

aa <- matriz_1p_split$EVA2021_2Sestudiante_EBRG1

dplyr::distinct(aa, Cod_indice, .keep_all = TRUE)$Analisis2

vcod_indice <- unique(aa$Cod_indice) #escalas del cuestionario i




#Rutina para la escala 'j' de la base 'i'
escala_j <- matriz_i[which(matriz_i$Cod_indice == vcod_indice[j]), ]
preg <- escala_j[c("cod_preg", "Cod_indice", "Cod_indice2")] #id de la escala
enunciado <- escala_j[c("cod_preg", "Enunciado")] #enunciados de la escala
elim_opc <- unique(escala_j$OpcionE)
constructo_j <- unique(escala_j$Constructo)
cod_constructo <- unique(escala_j$Cod_indice)
variables <- c("id","estrato") #le añadi esta variable para que pueda identificar cuales serían las columnas que añadiríamos a la base (además del ID)
bd1 <- bd[c(variables,preg$cod_preg)] #base con id para pegar los puntajes a la base





vcod_indice <- unique(matriz_i$Cod_indice) #escalas del cuestionario i
tipo <- distinct(matriz_i, Cod_indice, .keep_all = T)$Analisis2 #para identifcar pca o cfa despues









