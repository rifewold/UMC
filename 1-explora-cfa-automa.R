
library(lavaan)

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

cfa_recursivo <- function(data, model_lavaan, recursivo = TRUE, puntajes = TRUE){

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

      cfa_inicial <- reporte_lavaan(mod1, puntajes = FALSE)
      cfa_sugerido <- reporte_lavaan(mod2, puntajes = puntajes)

      return(list(cfa_inicial = cfa_inicial,
                  cfa_sugerido = cfa_sugerido))

    }else{

      cfa_inicial <- reporte_lavaan(mod1, puntajes = puntajes)
      return(cfa_inicial)

    }

  }

  cfa_inicial <- reporte_lavaan(mod1, puntajes = puntajes)
  return(cfa_inicial)


}

# funciona! :D pero donde va?

modo <- cfa_recursivo(data = bd1, model_lavaan = mm, recursivo = TRUE, puntajes = FALSE)

map(modo, "cargas") %>%
  map(~select(.x, -4, -5)) %>%
  reduce(~left_join(.x, .y, by = c("Escala", "Item"), suffix = c(".inicial", ".sugerido")))

indicadores <- map(modo, "indicadores") %>%
  map(~select(.x, -3)) %>%
  reduce(~left_join(.x, .y, by = "Indicadores", suffix = c(".inicial", ".sugerido")))

reliability(modo)[5, ]
compRelSEM(modo)
reporte_lavaan(modo, puntajes = FALSE)


estadistico = row.names(reliability(model_cfa_lavaan))


modo <- reporte_insumos_cfa_recurs(data = bd1, model_lavaan = mm, recursivo = TRUE)
subset(lavaan::parameterEstimates(modo), op == "=~")



# **********************************************************************************************************


# PCA recursivo .....

pca_umc_reporte(bd1, corr = "poly", puntajes = FALSE)

cor_pol <- psych::polychoric(bd1)$rho
psych::alpha(cor_pol)$feldt$alpha[[1]]


pca_recursivo <- function(data, recursivo = TRUE, puntajes = TRUE){

  summary(prcomp(drop_na(bd1)))

  cor_pol <- psych::polychoric(bd1)$rho
  val <- eigen(cor_pol)$values #autovalores
  varex <- val[1]/sum(val)
  t_vec <- t(eigen(cor_pol)$vectors) # transpuesta de autovectores
  cargas_x <- t_vec*sqrt(val)
  if(all(cargas_x[1, ] < 0)) cargas_x <- cargas_x*-1
  cargas <- data.frame(Item = names(bd1), Cargas = cargas_x[1, ])

  if(recursivo){

    indi <- varex
    cargafac <- cargas

    if(indi < .50){

      indi_nueva = indi
      cargafac_nueva = cargafac

      repeat{
        if(nrow(cargafac_nueva) <= 4){break} # si son 4 o menos items, pará

        if(indi < .50){

          # identificamos items
          if(nrow(filter(cargafac_nueva, abs(Cargas) < 0.4)) == 0){ # si no hay items con cargas menores a 0.4, identificamos el menor
            eliminar = filter(cargafac_nueva, Cargas == min(abs(Cargas)))$Item
          }else{
            eliminar = filter(cargafac_nueva, abs(Cargas) < 0.4)$Item # identificamos items con cargas menores a 0.4
          }

          # retiramos las columnas y nuevo modelo
          bd1_nueva <- bd1[, !(names(bd1) %in% eliminar)]

          mod2 <- cfa(modstring, data = data, ordered = TRUE, mimic = "Mplus", estimator = "WLSMV")

        }else{break} # paramos
      }

      cfa_inicial <- reporte_lavaan(mod1, puntajes = FALSE)
      cfa_sugerido <- reporte_lavaan(mod2, puntajes = puntajes)

      return(list(cfa_inicial = cfa_inicial,
                  cfa_sugerido = cfa_sugerido))

    }else{

      cfa_inicial <- reporte_lavaan(mod1, puntajes = puntajes)
      return(cfa_inicial)

    }

  }

  cfa_inicial <- reporte_lavaan(mod1, puntajes = puntajes)
  return(cfa_inicial)


}













