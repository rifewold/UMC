
library(lavaan)
library(here)
library(tidyverse)

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
matriz <- rio::import(here("Bases ejemplo","MIAU2021.xlsx"))

bd1 <- bd %>%
  select(starts_with("p11")) %>%
  mutate_all(as.numeric)

preg <- filter(matriz, Concatena1 == "EVA2021_2Sestudiante_EBRG1", Cod_indice == "EST2SMAT_MOTIV") %>%
  select(cod_preg, Cod_indice, Cod_indice2, Analisis2)

mm <- acomoda_string_lavaan(preg)

resultados1 <- reporte_insumos(bd1, tipo = "CFA", model_lavaan = mm, puntajes = FALSE)


#"resultados1$indicadores[1,2] < 0.95"
#eval(parse(text="resultados1$indicadores[1,2] < 0.95"))

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

cargas <- map(modo, "cargas") %>%
  map(~select(.x, -4, -5)) %>%
  reduce(~left_join(.x, .y, by = c("Escala", "Item"), suffix = c(".inicial", ".sugerido")))

indicadores <- map(modo, "indicadores") %>%
  map(~select(.x, -3)) %>%
  reduce(~left_join(.x, .y, by = c("Indicadores"), suffix = c(".inicial", ".sugerido")))

bind_cols(cargas, indicadores)

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

  pca_uno <- pca_1(data)

  if(recursivo){

    indi <- pca_uno$varex
    cargafac <- pca_uno$cargas

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
          data2 <- data[, !(names(data) %in% eliminar)]
          pca_dos <- pca_1(drop_na(data2))
          indi <- pca_dos$varex
          cargafac_nueva <- pca_dos$cargas

        }else{break} # paramos
      }

      pca_inicial <- pca_umc_reporte_b(data, corr = "poly", puntajes = FALSE)
      pca_sugerido <- pca_umc_reporte_b(data2, corr = "poly", puntajes = puntajes)

      return(list(pca_inicial = pca_inicial,
                  pca_sugerido = pca_sugerido))

    }else{

      pca_inicial <- pca_umc_reporte_b(data, corr = "poly", puntajes = puntajes)
      return(pca_inicial)

    }

  }

  pca_inicial <- pca_umc_reporte_b(data, corr = "poly", puntajes = puntajes)
  return(pca_inicial)


}


pca_recursivo(data = drop_na(bd1), recursivo = TRUE, puntajes = FALSE)

#****************************************************************************************************************
#*
#*
#*
#*
#*




bd1

cor_pol <- psych::polychoric(bd1)$rho
ee <- eigen(cor_pol, symmetric = FALSE)

#calculamos varianza (val), cargas (l), pesos (w)
val <- ee$values
val_sq <- sqrt(val) #desviacion
l <- ee$vectors %*% diag(val_sq)
w <- ee$vectors %*% diag(1/val_sq)

cargas <- data.frame(Item = names(bd1), Pesos = w[, 1], Cargas = l[, 1])
varex <- val[1]/sum(val)*100


pca_1 <- function(x){

  #if(sum(sapply(x, function(xx) sum(is.na(xx)))) > 0) stop("Es preferible que no hayan NAs en las columnas =)")

  cor_pol <- psych::polychoric(x)$rho
  ee <- eigen(cor_pol, symmetric = FALSE)

  #calculamos varianza (val), cargas (l), pesos (w)
  val <- ee$values
  val_sq <- sqrt(val) #desviacion
  l <- ee$vectors %*% diag(val_sq)
  w <- ee$vectors %*% diag(1/val_sq)

  cargas <- data.frame(Item = names(x), Pesos = w[, 1], Cargas = l[, 1])
  varex <- val[1]/sum(val)

  return(list(cargas = cargas, varex = varex))

}

pca_1(drop_na(bd1))







pca_umc_reporte_b <- function(x, corr = NULL, puntajes = TRUE){

  #if(sum(sapply(x, function(xx) sum(is.na(xx)))) > 0) stop("Es preferible que no hayan NAs en las columnas =)")

  if(is.null(corr)){
    ee <- eigen(cor(x), symmetric = FALSE) # symmetric=FALSE previene cargas negativas [espero]
  }else{
    cor_pol <- psych::polychoric(x)$rho
    ee <- eigen(cor_pol, symmetric = FALSE)
  }

  #calculamos varianza (val), cargas (l), pesos (w)
  val <- ee$values
  val_sq <- sqrt(val) #desviacion
  l <- ee$vectors %*% diag(val_sq)
  w <- ee$vectors %*% diag(1/val_sq)

  if(puntajes == TRUE){
    z <- as.matrix(scale(x)) # datos estandarizados y matrix
    s <- z %*% l # datos estandarizados por sus cargas
    s <- scale(s)
  }

  cargas <- data.frame(Item = names(x), Pesos = w[, 1], Cargas = l[, 1])
  vr <- c("Pesos", "Cargas")
  cargas[vr] <- apply(cargas[vr], 2, function(x) format(round(x, 3), decimal.mark = ","))
  varex <- format(round(val[1]/sum(val)*100, 2), decimal.mark = ",")

  if(puntajes == TRUE){
    return(list(puntajes = s[, 1], indicadores = varex, cargas = cargas))}
  else {
    return(list(indicadores = varex, cargas = cargas))}

}






