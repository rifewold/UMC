rm(list = ls())

library(tidyverse)
library(here)
library(rio)
library(mice)
rowSumsNA <- function(x) rowSums(is.na(x)) #contar NA en las observaciones
invertir <- function(x, i) i+1 - x #para invertir

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

for(i in 1:length(nom)){ #i=2

  #Preparamos los insumos/variables para la rutina de la base/cuestionario 'i'
  matriz_i <- matriz_lista[[nom[i]]]
  vcod_indice <- unique(matriz_i$Cod_indice) #escalas del cuestionario i

  bd <- lista[[nom[i]]] #tomamos la base i

  for(j in 1:length(vcod_indice)){ #j=1

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
    library(mice)
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


    bd3_corr <- split(preg, preg$Cod_indice2) %>%
      map(1) %>% # cod_preg
      map(~select(bd3, all_of(.x))) %>% #sub_escalas
      map(~psych::polychoric(.x)$rho) #correlacion policor


    map(bd3_corr, ~psych::KMO(.x)$MSA)

    psych::KMO(bd3_corr$FAM2SHSE_EMPATAP)$MSA


    chequeo <- function(mc){

      c1 <- apply(mc, 1, function(x) all(x[x != 1] > 0)) #todas las correlaciones son positivas?
      c2 <- apply(mc, 1, function(x) any(x[x != 1] > 0)) #hay algun positivo?
      c3 <- apply(mc, 1, function(x) any(x[x != 1] > 0.30)) #todas las correlaciones son mayores a .30?

      tabla1 <- data.frame(
        item = names(c1),
        todo_positivo = c1,
        hay_positivo = c2,
        todos_min_30 = c3)

      return(tabla1)


    }

    chequeo(bd3_corr$FAM2SHSE_EMPATAP)

    c1 <- apply(mc, 1, function(x) all(x[x != 1] > 0)) #todas las correlaciones son positivas?
    c2 <- apply(mc, 1, function(x) any(x[x != 1] > 0)) #hay algun positivo?
    c3 <- apply(mc, 1, function(x) any(x[x != 1] > 0.30)) #todas las correlaciones son mayores a .30?


    ej <- data.frame(
      v1 = c(1, 0.34, 0.25),
      v2 = c(0.34, 1, -0.10),
      v3 = c(0.25, -0.10, 1)
    )

    ej



    apply(ej, 1, function(x) all(x[x != 1] > 0)) #todas son positivas
    apply(ej, 1, function(x) any(x[x != 1] > 0)) #hay alguna positiva

    chequeo(cc)

    apply(cor(bd3[-1]), 1, function(x) any(x[x != 1] > 0))

    cc <- bd3_corr$FAM2SHSE_EMPATAP

    apply(cc, 1, function(x) any(x[x != 1] > 0))
    apply(cc, 1, function(x) any(x[x != 1] > 0.30))


    psych::KMO()

    datos_imputados[[i]][[j]] <- bd3
    tabla_info_missing_pegar <- bind_rows(tabla_info_missing_pegar, tabla_info_missing)


    apply(cor(bd3[-1]), 1, function(x) any(x[x != 1] > 0))
    apply(cor(bd3[-1]), 2, function(x) prop.table(table(x), 1))


  }

    datos_imputados_lista[[i]] <- datos_imputados[[i]] %>%
      reduce(full_join, by = "id2") %>%
      left_join(lista[[i]][1], ., by = c("id" = "id2"))


}




    tabla_info_missing

    df <- data.frame(Doubles=double(),
                     Ints=integer(),
                     Factors=factor(),
                     Logicals=logical(),
                     Characters=character(),
                     stringsAsFactors=FALSE)


    reporte_missing

    reporte_missing[]

    lista[[i]][1]

    pca_umc_reporte(bd3[-1], corr = "poly")


    complete(mice_data, action = "long")

    pool(mice_data)

    head(complete(mice_data, 2))
    head(bd2)
    bd %>% mutate(across(all_of(preg$cod_preg), as.numeric)) %>% head()

    mice_data$imp


    library(semTools)

    # generar puntajes con cada imputacion

    # primera imputacion
    bd_imp1 <- as_tibble(complete(mice_data, 1))
    m1_imp1 <- cfa(mm, data = bd_imp1, ordered = TRUE, mimic = "Mplus", estimator = "WLSMV")
    summary(m1_imp1)
    lavaan::fitmeasures(m1_imp1,  c("cfi", "tli", "srmr", "rmsea"))
    subset(lavaan::parameterEstimates(m1_imp1), op == "=~")
    puntajes1 <- as.data.frame(lavaan::lavPredict(m1_imp1))
    hist(puntajes1$EST2SMAT_MOTIV)

    # usando todas las imputaciones de manera integrada
    impList <- list()
    for (i in 1:mice_data$m) impList[[i]] <- complete(mice_data, action = i)
    mod_mi <- cfa.mi(mm, data = impList, ordered = TRUE, mimic = "Mplus", estimator = "WLSMV")
    mod_mi <- cfa.mi(mm, data = impList, mimic = "Mplus", estimator = "WLSMV")

    subset(summary(mod_mi, output = "data.frame"), op == "=~")
    lavaan::fitmeasures(mod_mi, c("cfi","tli","rmsea","srmr"))[c(1, 3, 5, 13)]
    puntajes_mi <- as.data.frame(lavaan::lavPredict(mod_mi))
    hist(puntajes_mi$EST2SMAT_MOTIV)

    puntajes_mi <- plausibleValues(mod_mi)
    hist(puntajes_mi[[1]][[2]])
    hist(puntajes_mi[[2]][[2]])
    hist(puntajes_mi[[3]][[2]])
    hist(puntajes_mi[[4]][[2]])

    # es una locura!

    bd_imp1_5 <- map(1:5, ~complete(mice_data, .x))


    # PENDIENTE -----

    # CFA en MPLUS
    MplusAutomation::prepareMplusData(bd2[-1], "C:/Users/factoresasociados03/Desktop/cfa_en_mplus/data1.dat")


    "C:/Users/factoresasociados03/Desktop/cfa_en_mplus/puntajes.dat"

    m1_mplus <- MplusAutomation::readModels("C:/Users/factoresasociados03/Desktop/cfa_en_mplus/mptext1.out")

    MplusAutomation::
    fc <- rio::import("C:/Users/factoresasociados03/Desktop/cfa_en_mplus/puntajes.dat")

    m1_mplus$parameters$stdyx.standardized %>%
      filter(paramHeader == "F1.BY")


    m1_mplus$summaries$CFI
    m1_mplus$summaries$TLI
    m1_mplus$summaries$RMSEA_Estimate


    lavaan::fitmeasures(m1_imp1,  c("cfi", "tli", "srmr", "rmsea"))
    subset(lavaan::parameterEstimates(m1_imp1), op == "=~")
    subset(lavaan::standardizedSolution(m1_imp1), op == "=~")

    mod_para_ejec <- MplusAutomation::mplusObject(
      TITLE = "CFA en Mplus",
      VARIABLE =
        "CATEGORICAL ARE p11_01-p11_09",
      MODEL = "F1 by p11_01-p11_09",
      OUTPUT = "STDYX SVALUES MODINDICES",
      rdata = bd2[-1],
      usevariables = colnames(bd2[-1]),
      SAVEDATA =
         "FILE is C:/Users/factoresasociados03/Desktop/cfa_en_mplus/cfa1/puntajes.dat;
          SAVE = FSCORES;
          FORMAT IS FREE;"
    )

    rr <- "C:/Users/factoresasociados03/Desktop/cfa_en_mplus/mplusautomat/"
    rr2 <- "C:/Users/factoresasociados03/Desktop/cfa_en_mplus/"

    rr3 <- "C:/Users/factoresasociados03/Desktop/cfa_en_mplus/cfa1/"

    resulta <- MplusAutomation::mplusModeler(mod_para_ejec,
                                  modelout = paste0(rr3, "cfa.inp"),
                                  dataout = paste0(rr3, "data1.dat"),
                                  run = 1L)

    fc <- rio::import("C:/Users/factoresasociados03/Desktop/cfa_en_mplus/cfa1/puntajes.dat")

    resulta$parameters$stdyx.standardized %>%
      filter(paramHeader == "F1.BY")










