# 30/10/20, DHC


# Hola! esta es una modificacion de prueba!

#En este script se han colocado algunas funciones que se han ido desarrollando
#para la generación de reportes de la ESAE.
#Proximamente, paquete....

library(tidyverse) #debe estar cargado los paquete de tidyverse!

#lecturas recomendadas para el uso de purrr ~~~
#https://jennybc.github.io/purrr-tutorial/ls03_map-function-syntax.html
#http://zevross.com/blog/2019/06/11/the-power-of-three-purrr-poseful-iteration-in-r-with-map-pmap-and-imap/

#lecturas recomendadas para hacer funciones con dplyr ~~~
#https://shipt.tech/https-shipt-tech-advanced-programming-and-non-standard-evaluation-with-dplyr-e043f89deb3d
#https://cran.r-project.org/web/packages/dplyr/vignettes/programming.html
#https://rpubs.com/lionel-/programming-draft

#**************************************************************************************************
#pega_lista
# primero agrega una columna con el nombre de la lista, despues pega las listas
# util cuando un df se separa en listas segun algun estrato y se le aplican las mismas funciones para luego
# pegarlas otra vez...

#  pega_lista(data, nc)
#    data: lista de dataframe
#    nc: nombre de la columna que tendra un vector con el nombre de la lista

pega_lista <- function(data, nc){
  if(!is.list(data)) stop("Se aplica sobre una lista")
  imap(data, ~mutate(.x, !!nc := .y)) %>% bind_rows() #para bind listas
}

#ejemplo:
#ejem <- list(hom = data.frame(v = 1:3), muj = data.frame(v = 4:6))
#pega_lista(ejem, "nombre")

#******************************************************************************************************
#tabla_freq1
# tabla de frecuencias para una columna
# lo mismo que table()... pero devuelve los % redondeados y en dataframe.

tabla_freq1 <- function(x) as.data.frame(round(prop.table(table(x))*100, 1))

#ejemplo:
#datos <- sample(c("a", "b", "c"), 102, replace = T)
#tabla_freq1(datos) #con un vector

#datos <- data.frame(v1 = sample(c("a", "b", "c"), 102, replace = T))
#tabla_freq1(datos2$v1) #con un data frame
#tabla_freq1(datos2["v1"])
#datos2 %>% select(v1) %>% tabla_freq1()
#datos2 %>% tabla_freq1(v1) #no funciona asi... porque? pendiente...

#*********************************************************************************************************
#tabla_freq2
# tabla de frecuencias para varias columnas, especificamente un grupo de items con las mismas
# opciones de respuesta, necesita de tabla_freq1 y pega_lista
# la funcion toma todas las preguntas que empiezan con cierto character, p11_01, p11_02, p11_03,....

tabla_freq2 <- function(data, x){

  data %>% select(starts_with(x)) %>%
    map(., tabla_freq1) %>% pega_lista(., "var")

}

#ejemplo
#v1 <- sample(c("a", "b", "c"), 102, replace = T)
#v2 <- sample(c("a", "b", "c"), 102, replace = T)
#v3 <- sample(c("a", "b", "c"), 102, replace = T)

#df <- data.frame(p02_01 = v1, p02_02 = v2, p02_03 = v3)
#tabla_freq2(df, "p02")

#***************************************************************************************
#tabla_freq2_estrato
# tabla de frecuencias para varias columnas, especificamente un grupo de items con las mismas
# opciones de respuesta y por estrato, necesita de tabla_freq1 y pega_lista
# la funcion toma todas las preguntas que empiezan con cierto character, p11_01, p11_02, p11_03,....
# y devuelve las frecuencias por estrato...

tabla_freq2_estrato <- function(data, x, estrato){
  if(!require(tidyverse))
    stop("'tidyverse' debe estar instalado")

  x <- enquo(x)

  split(data, data[estrato]) %>%
    map(~select(.x, starts_with(!!x))) %>%
    map(~map(.x, ~as.data.frame(round(prop.table(table(.x))*100, 1)))) %>%
    map(~pega_lista(.x, "var")) %>% pega_lista("Estrato") %>% rename(x = 1)
}

#ejemplo:
#gestion <- sample(c("Estatal", "No estatal"), 102, replace = T)
#df2 <- cbind(df, gestion) #usa el df del ejemplo anterior.

#******************************************************************************************************
#label_centro: suma acumulada y luego restarle por la mitad, para la posicion de los labels O___O
label_centro <- function(x){cumsum(x) - x*0.5}

#imaginemos que tenemos este vector para graficar en barras apiladas....
#v2 <- c(rep("a", 15), rep("b", 100), rep("c", 1), rep("d", 3), rep("e", 95))
#df1 <- tabla_freq1(v2)

#df1 %>%
#  mutate(pos_label = label_centro(Freq), #las opciones debes estar en el orden que apareceran el grafico
#         item = "item1") %>%
#  ggplot(aes(x = item, y = Freq, fill = fct_rev(x))) +
#  geom_bar(stat = "identity") + coord_flip() +
#  theme(legend.position = "none") +
#  geom_text(aes(label = Freq, y = pos_label, x = item), vjust = 0.5)

#las etiquetas de los porcentajes mas pequeños, estan muy pegados....

#******************************************************************************************************
#ajuste_pos_label: ajusta posicion de la etiqueta cuando estan muy pegados

#df3 <- df1 %>% mutate(pos_label = label_centro(Freq), item = "item1")

#df3 %>%
#  mutate(dif = pos_label - lag(pos_label, order_by = x),
#         pos_labelf = (case_when(
#           dif < 3 & Freq >= 3 ~ pos_label + 3, #ajustar
#           dif < 3 & Freq <= 3 ~ pos_label + 2,
#           TRUE ~ pos_label)),
#         pos_labelf = ifelse(is.na(pos_labelf), pos_label, pos_labelf)) %>% #por si acaso queda un NA por el lag
#  ggplot(aes(x = item, y = Freq, fill = fct_rev(x))) +
#  geom_bar(stat = "identity") + coord_flip() +
#  theme(legend.position = "none") +
#  geom_text(aes(label = Freq, y = pos_labelf, x = item), vjust = 0.5)

#df3 %>% ajuste_pos_label(pos_label, Freq, x)
#df3 %>% ajuste_poslabel(bbb, aaa, xxx)

#df3 <- rename(df3, aaa = 2, bbb = 3, xxx = 1)
#
# df <-
# data.frame(n_pregunta = rep("pregunta1", 5),
#            opcion = c("a", "b", "c", "d", "e"),
#            freq = c(7, 46.7, 0.5, 1.4, 44.4),
#            pos_label = c(3.5, 30.35, 53.95, 54.9, 77.8))
#
# ggplot(df, aes(x = n_pregunta, y = freq, fill = fct_rev(opcion))) +
#   geom_bar(stat = "identity") + coord_flip() +
#   theme(legend.position = "none") +
#   geom_text(aes(label = freq, y = pos_label, x = n_pregunta), vjust = 0.5)
#
#
# ajuste_poslabel <- function(data, posl, freq, xxn){
#   posl <- enquo(posl) #posicion actual de la etiqueta
#   freq <- enquo(freq) #porcentajes
#   xxn <- enquo(xxn) #opcion de respuesta
#
#   data %>%
#     mutate(
#       dif = !!posl - lag(!!posl, order_by = !!xxn), #diferencia entre las posiciones de las etiquetas contiguas
#       poslabel_aj = (case_when( #si dif es pequeño, sumarle unos puntos
#              dif < 3 & !!freq >= 3 ~ !!posl + 3, #ajustar
#              dif < 3 & !!freq < 3 ~ !!posl + 2,
#              TRUE ~ !!posl)),
#       poslabel_aj = ifelse(is.na(poslabel_aj), !!posl, poslabel_aj)) #por si acaso queda un NA por el lag
# }

#********************************************************************************************

#agrupa las funciones para el cfa
#debe estar cargado lavaan !
library(lavaan)

cfa_lavaan <- function(data, nomvar, puntajes = TRUE){

  m <- paste(nomvar, paste(names(data), collapse = '+'), sep = '=~')
  m_cfa <- cfa(m, data = data, ordered = names(data), mimic="Mplus", estimator="WLSMV")

  #cargas factoriales
  cargas <-
    as_tibble(parameterEstimates(m_cfa, standardized = TRUE)) %>%
    filter(op == "=~") %>%
    mutate(stars = case_when(
      pvalue < 0.001 ~ "***", pvalue < 0.01 ~ "**", pvalue < 0.05 ~ "*", TRUE ~ "")) %>%
    select(Item = rhs, B = est, SE = se, Beta = std.all, sig. = stars) %>%
    mutate(across(where(is.numeric), round, 3)) %>%
    mutate(across(where(is.numeric), format, decimal.mark = ","))

  #indicadores de ajuste
  indicadores <-
    fitMeasures(m_cfa, c("cfi","rmsea","tli","srmr")) %>%
    round(4) %>% as.data.frame() %>%
    rownames_to_column("Indicadores") %>%
    rename(Valores = 2)

  indicadores1 <- mutate(indicadores,
                         across(where(is.numeric), format, decimal.mark = ","))

  ajuste <-
  indicadores[1,2] > 0.9 & indicadores[2,2] <= 0.1 &
    indicadores[3,2] > 0.9 & indicadores[4,2] <= 0.09

  if(puntajes == TRUE){
  puntajes1 <- as.data.frame(lavPredict(m_cfa))
  }


  if(puntajes == TRUE){
    return(list(cargas = cargas, indicadores = indicadores1, puntajes = puntajes1, ajuste = ajuste))}
  else {return(list(cargas = cargas, indicadores = indicadores1))}


}

#
# x <- runif(1:100)
# y <- runif(1:100)
# z <- runif(1:100)
# zz <- runif(1:100)
# df <- cbind.data.frame(x, y, z, zz)
#
# start_time <- Sys.time()
# aa <- cfa_lavaan(bd3[-1], nomvar = "dd")
# end_time <- Sys.time()
# end_time - start_time
#
# start_time <- Sys.time()
# bb <- cfa_lavaan(bd3[-1], nomvar = "dd", puntajes = FALSE)
# end_time <- Sys.time()
# end_time - start_time


#********************************************************************************************
library(psych)
#para la PCA
#debe estar cargado psych!
# se ha añadido un argumento para indicar si es que debe generar puntajes
# en los analisis iniciales o reportes, no es necesario generar puntajes
pca_umc_reporte <- function(x, n, puntajes = TRUE){

  nomo <- names(x)

  if(n == 1){
    valor <- eigen(cor(x))$values #autovalores
    t.vector <- t(eigen(cor(x))$vectors) # transpuesta de autovectores
  }else{
    valor <- eigen(polychoric(x)$rho)$values #autovalores
    t.vector <- t(eigen(polychoric(x)$rho)$vectors)} # transpuesta de autovectores

  media.x <- map_dbl(x, mean); desv.x <- map_dbl(x, sd) #media y desviacion
  cargas.x <- (t.vector*sqrt(valor)) #cargas de cada una de las variables
  peso.x <- cargas.x/valor[1] #peso de cada una de las variables
  n <- rep(1, nrow(x))

  if(all(cargas.x[1, ] < 0)){
    cargas.x <- cargas.x*-1; peso.x <- peso.x*-1
  }else{
    cargas.x[1, ] <- cargas.x[1, ]; peso.x[1, ] <- peso.x[1, ]}

  if(puntajes == TRUE){

  for(j in 1:length(x)){
    aaa <- (t(t(x) - media.x)[, j]*peso.x[1, j])/desv.x[j]
    n <- cbind(n, aaa)
  }

  n <- n[, -1]
  indice1 <- apply(n, 1, sum)

  }

  cargas <-
    bind_cols(
      Item = nomo,
      Pesos = round(peso.x[1, ], 3),
      Cargas = round(cargas.x[1, ], 3)) %>%
    mutate(across(where(is.numeric), format, decimal.mark = ","))

  varex <- format(round(valor[1]/sum(valor)*100, 2), decimal.mark = ",")

  if(puntajes == TRUE){
    return(list(puntajes = indice1, indicadores = varex, cargas = cargas))}
  else {return(list(indicadores = varex, cargas = cargas))}

}

# x <- runif(1:100)
# y <- runif(1:100)
# df <- cbind.data.frame(x, y)
#
# pca_umc_reporte(df, 1, puntajes = FALSE)
# pca_umc_reporte(df, 1, puntajes = TRUE)

#***********************************************************************************************

#devuelve puntajes e insumos para el reporte de las escalas, segun pca o cfa
reporte_insumos <- function(data, tipo, nomvar, puntajes = TRUE){

  data2 <- data %>%
    mutate(across(everything(), as.numeric))

  if(tipo == "PCA"){
    list_insumos <- pca_umc_reporte(data2, 2, puntajes = puntajes)
  }else{
    list_insumos <- cfa_lavaan(data2, nomvar, puntajes = puntajes)
  }

  return(list_insumos)

}

#reporte_insumos(bd3[-1], tipo = "CFA",  nomvar = "aa", puntajes = FALSE)
#reporte_insumos(bd3[-1], tipo = "CFA",  nomvar = "aa", puntajes = TRUE)


#*********************************************************************************************************

#agrupa las funciones para el cfa
#debe estar cargado lavaan !

# bd3
#
# m <- paste(cod_escala[j], paste(names(bd3), collapse = '+'), sep = '=~')
# m_cfa <- cfa(m, data = bd3, ordered = names(bd3), mimic = "Mplus", estimator = "WLSMV")
#
# indicadores <-
#   fitMeasures(m_cfa, c("cfi","rmsea","tli","srmr")) %>%
#   round(4) %>% as.data.frame() %>%
#   rownames_to_column("Indicadores") %>%
#   rename(temp = 2) %>%
#   pivot_wider(names_from = Indicadores, values_from = temp) %>%
#   mutate(Variable = cod_escala[j],
#          Constructo = constructo[j]) %>%
#   select(Constructo, Variable, everything())
#
#
#
# dd <- round(fitMeasures(m_cfa, c("cfi","rmsea","tli","srmr")), 4) %>%
#   as.data.frame() %>%
#   rownames_to_column("Indicadores") %>%
#   pivot_wider(names_from = 1, values_from = .)
#
# aa <- round(fitMeasures(m_cfa, c("cfi","rmsea","tli","srmr")), 4)
# aa2 <- as.data.frame(aa)
# t(aa2)


# cfa_lavaan <- function(data, nomvar){
#
#   m <- paste(nomvar, paste(names(data), collapse = '+'), sep = '=~')
#   m_cfa <- cfa(m, data = data, ordered = names(data), mimic = "Mplus", estimator = "WLSMV")
#
#   #cargas factoriales
#   cargas <-
#     as_tibble(parameterEstimates(m_cfa, standardized = TRUE)) %>%
#     filter(op == "=~") %>%
#     mutate(stars = case_when(
#       pvalue < 0.001 ~ "***", pvalue < 0.01 ~ "**", pvalue < 0.05 ~ "*", TRUE ~ "")) %>%
#     select(Ítem = rhs, B = est, SE = se, Beta = std.all, sig. = stars) %>%
#     mutate(across(where(is.numeric), round, 3)) %>%
#     mutate(across(where(is.numeric), format, decimal.mark = ","))
#
#   #indicadores de ajuste
#   indicadores <-
#     fitMeasures(m_cfa, c("cfi","rmsea","tli","srmr")) %>%
#     round(4) %>% as.data.frame() %>%
#     rownames_to_column("Indicadores") %>%
#     rename(Índices = 2)
#
#   indicadores1 <- mutate(indicadores,
#                          across(where(is.numeric), format, decimal.mark = ","))
#
#   ajuste <-
#     indicadores[1,2] > 0.9 & indicadores[2,2] <= 0.1 &
#     indicadores[3,2] > 0.9 & indicadores[4,2] <= 0.09
#
#   puntajes <- as.data.frame(lavPredict(m_cfa))
#
#   return(list(cargas = cargas, indicadores = indicadores1, puntajes = puntajes, ajuste = ajuste))
# }

#********************************************************************************************

cargas_indicadores <- function(mod_cfa, puntajes = TRUE){

  #cargas factoriales
  cargas <-
    as_tibble(parameterEstimates(mod_cfa, standardized = TRUE)) %>%
    filter(op == "=~") %>%
    mutate(stars = case_when(
      pvalue < 0.001 ~ "***", pvalue < 0.01 ~ "**", pvalue < 0.05 ~ "*", TRUE ~ "")) %>%
    select(Item = rhs, B = est, SE = se, Beta = std.all, sig. = stars) %>%
    mutate(across(where(is.numeric), round, 3)) %>%
    mutate(across(where(is.numeric), format, decimal.mark = ","))

  #indicadores de ajuste
  indicadores1 <-
    fitMeasures(mod_cfa, c("cfi","rmsea","tli","srmr")) %>%
    round(4) %>% as.data.frame() %>%
    rownames_to_column("Indicadores") %>%
    rename(Valores = 2)

  if(puntajes == TRUE){
    puntajes1 <- as.data.frame(lavPredict(m_cfa))
  }

  if(puntajes == TRUE){
    return(list(cargas = cargas, indicadores = indicadores1, puntajes = puntajes1))}
  else {return(list(cargas = cargas, indicadores = indicadores1))}

}

# Función para sacar proporciones (funciona con una o con dos variables y múltiples veces) ----

proporcion_estrato2=function(data, var, var2=NULL,FUN=proporcion_estrato,...){

  proporcion_estrato <- function(data, grupo1, grupo2,label=NULL,peso=NULL,size_h=NULL,diseno=NULL){
    #Estimación de la proporción no pesada
    if (missing(peso)){
      b=data %>%
        group_by({{grupo1}},{{grupo2}}) %>%
        summarise(n=n()) %>%
        drop_na() %>%
        mutate(freq=round((n/sum(n)*100),2))
    }else{#Estimación de la proporción pesada
      b=data %>%
        group_by({{grupo1}},{{grupo2}}) %>%
        summarise(n=sum({{peso}},na.rm=T)) %>%
        drop_na() %>%
        mutate(freq=round((n/sum(n)*100),2))
    }

    if(!missing(label)){
      if(Hmisc::label(data[[as_label(enquo(grupo2))]])=="") stop("Hay una columna o columnas que no tiene(n) label(s)")

      c=as.data.frame(Hmisc::label(data[[as_label(enquo(grupo2))]])) %>%
        rename(label_opcion = 1)

      b=cbind(b,c)

    }

    #Estimación de la h de cohen
    if(!missing(size_h)){
      group_ <- enquo(grupo1)
      h_size=b %>% mutate(freq=freq/100) %>% select(-n) %>% pivot_wider(names_from =as_label(group_),
                                                                        values_from = "freq") %>%
        ungroup() %>%
        mutate(h=abs(2*asin(sqrt(.[[2]]))-2*asin(sqrt(.[[3]]))))

      b=left_join(b,h_size[,c(1,ncol(h_size))])
    }

    #Estimación de la significancia
    if(!missing(diseno)){
      if(!is.list(diseno)) stop("Debes meter un diseño de acuerdo a los parámetros de survey")
      options(survey.lonely.psu = "certainty") #opcion de survey
      group_ <- enquo(grupo1)
      # bd_design <- diseno
      labs <- enquo(grupo2)
      bd_design <- svydesign(data = data, id = as.formula(diseno[[1]]),
                             strata = as.formula(diseno[[2]]),
                             fpc = as.formula(diseno[[3]]), nest = TRUE, pps = "brewer")
      p_val=svychisq(as.formula(paste0("~",as_label(labs),"+",as_label(group_))),design=bd_design,na.rm=T,statistic="Chisq")$p.value
      b=cbind(b,p_val=p_val)
    }

    return(b)

  }

  if(missing(var2)){
    purrr::map(var,function(x)
      FUN(data,grupo1=.data[[x]],...)) %>%
      set_names(var) %>% #purrr::map(~set_names(.x, vari2)) %>% #nombres a las listas
      purrr::map(~rename(.x, opcion = 1)) %>% #nombre de la columna
      pega_lista("grupo") #juntamos todas las tablas
  }else{
    purrr::map(var,function(x)
      purrr::map(var2, function(y)
        FUN(data,.data[[x]],.data[[y]],...))) %>%
      set_names(var) %>% purrr::map(~set_names(.x, var2)) %>% #nombres a las listas
      map_depth(2, ~rename(.x, estrato = 1,opcion=2)) %>% #nombre de la columna
      purrr::map(~pega_lista(.x, "grupo")) %>% pega_lista("medida") #juntamos todas las tablas
  }

}

# Función para sacar múltiples significancias (funciona con una o con dos) ----

post_proporcion_estrato2 <- function(data, var, var2,FUN=post_proporcion_estrato,...){
  post_proporcion_estrato <- function(data, grupo1, grupo2,label=NULL,peso=NULL,size_h=NULL,diseno=NULL){

    nombres1=as.character(data %>% select({{grupo1}}) %>% distinct() %>% drop_na() %>% as_vector())
    nombres2=as.character(data %>% select({{grupo2}}) %>% distinct() %>% drop_na() %>% as_vector())

    combo1=map(1:ncol(combn(nombres1,2)),~combn(nombres1,2)[,.x])
    combo2=map(1:ncol(combn(nombres2,2)),~combn(nombres2,2)[,.x])

    gru1=map_chr(1:ncol(combn(nombres1,2)),~paste0(combn(nombres1,2)[,.x],collapse="_"))
    gru2=map_chr(1:ncol(combn(nombres2,2)),~paste0(combn(nombres2,2)[,.x],collapse="_"))


    if(length(nombres1)<=2&length(nombres2)<=2)
      stop("Los dos grupos que hay acá tienen solo dos categorías.
         Al menos uno de los grupos a comparar debe tener más de dos categorías")
    if(missing(diseno))
      stop("Debes colocar el diseño para sacar significancia")

    group_ <- enquo(grupo1)
    labs <- enquo(grupo2)

    if(length(nombres1)>2&length(nombres2)<3){

      a=map(combo1,~data %>% filter({{grupo1}} %in% .x) %>%
              mutate(estrx=fct_drop({{grupo1}}))) %>%
        set_names(gru1) %>%
        map(~svydesign(data = .x, id = as.formula(diseno[[1]]),
                       strata = as.formula(diseno[[2]]),
                       fpc = as.formula(diseno[[3]]), nest = TRUE, pps = "brewer"))

      f=a %>% map(~svychisq(as.formula(paste0("~",as_label(labs),"+estrx")),
                            design=.x,na.rm=T,statistic="Chisq")$p.value) %>%
        map(~as.data.frame(.x)) %>%
        pega_lista("comparacion1") %>% bind_cols("comparacion2"=gru2)

      rownames(f)=NULL
      colnames(f)=c("p_val","grupo_comparacion1","grupo_comparacion2")
      f$p_val=round(f$p_val,5)

    }else if(length(nombres1)<3&length(nombres2)>2){
      a=map(combo2,~data %>% filter({{grupo2}} %in% .x) %>%
              mutate(estrx=fct_drop({{grupo2}}))) %>%
        set_names(gru2) %>%
        map(~svydesign(data = .x, id = as.formula(diseno[[1]]),
                       strata = as.formula(diseno[[2]]),
                       fpc = as.formula(diseno[[3]]), nest = TRUE, pps = "brewer"))

      f=a %>% map(~svychisq(as.formula(paste0("~","estrx","+",as_label(group_))),
                            design=.x,na.rm=T,statistic="Chisq")$p.value) %>%
        map(~as.data.frame(.x)) %>%
        pega_lista("comparacion2") %>% bind_cols("comparacion1"=gru1)

      rownames(f)=NULL
      colnames(f)=c("p_val","grupo_comparacion2","grupo_comparacion1")
      f$p_val=round(f$p_val,5)
    }else{

      a=map(combo1,function(x)
        map(combo2,function(y)
          (data %>% filter({{grupo1}} %in% x,{{grupo2}} %in% y) %>%
             mutate(estrx1=fct_drop({{grupo1}}),
                    estrx2=fct_drop({{grupo2}}))))) %>%
        set_names(gru1) %>% purrr::map(~set_names(.x, gru2)) %>%
        map_depth(2,~svydesign(data = .x, id = as.formula(diseno[[1]]),
                               strata = as.formula(diseno[[2]]),
                               fpc = as.formula(diseno[[3]]), nest = TRUE, pps = "brewer"))

      f=a %>% map_depth(2,~svychisq(as.formula(paste0("~","estrx2","+","estrx1")),
                                    design=.x,na.rm=T,statistic="Chisq")$p.value) %>%
        map_depth(2,~as.data.frame(.x)) %>%
        purrr::map(~pega_lista(.x, "comparacion2")) %>% pega_lista("comparacion1")

      rownames(f)=NULL
      colnames(f)=c("p_val","grupo_comparacion2","grupo_comparacion1")
      f$p_val=round(f$p_val,5)

    }

    return(f)
  }

  purrr::map(var,function(x)
    purrr::map(var2, function(y)
      post_proporcion_estrato(data,.data[[x]],.data[[y]],...))) %>%
    set_names(var) %>% purrr::map(~set_names(.x, var2)) %>% #nombres a las listas
    #map_depth(2, ~rename(.x, estrato = 1,opcion=2)) %>% #nombre de la columna
    purrr::map(~pega_lista(.x, "grupo2")) %>% pega_lista("grupo1") #juntamos todas las tablas

}




####Matriz de correlaciones resumida

flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut]
  )
}

####Matriz de correlaciones resumida


cor_estrato = function(data,estrato=NULL,label=NULL){
  if(missing(estrato)&any(map_chr(data,~is.numeric(.x))==F)) stop("Yara causa, tienes variables que no son numéricas")

  if(!missing(estrato)){
    a=data %>%
      bind_cols("general"="General") %>%
      pivot_longer(cols=c("general",{{estrato}}),
                   names_to = "estrato",
                   values_to = "niveles")
  }else{
    a=data %>%
      bind_cols("general"="General") %>%
      pivot_longer(cols=c("general"),
                   names_to = "estrato",
                   values_to = "niveles")
  }

  a=a %>%
    filter(!niveles=="") %>%
    split(.$niveles) %>%
    # map(~select(.,M500_PIL_1S_2021_CIU,
    #             starts_with("EST"))) %>%
    map(~cor(.x %>% select(-estrato,-niveles),use="pairwise.complete.obs")) %>%
    map(~flattenCorrMatrix(.x)) %>%
    imap(~mutate(.x,estrato:=.y)) %>%
    bind_rows() %>%
    pivot_wider(names_from=estrato,
                values_from=cor)

  if(!missing(label)){
    labs=as.data.frame(Hmisc::label(data))
    names(labs)=c("nombre_constructo")
    labs=cbind(labs,cons=row.names(labs))
    a=left_join(a,labs,by=c("row"="cons")) %>%
      left_join(.,labs,by=c("column"="cons")) %>% rename("nombre_constructo_row"=nombre_constructo.x,
                                                         "nombre_constructo_column"=nombre_constructo.y)
  }
  return(a)
}



ajuste_poslabel2 <- function (data, posl, freq, xxn){

  data %>%
    mutate(dif = {{posl}} - lag({{posl}}, order_by = {{xxn}}),
           poslabel_aj = case_when(
             dif < 6 & {{freq}} >= 3 ~ {{posl}} + 3,
             dif < 6 & {{freq}} < 3 ~ {{posl}} + 5,
             dif < 6 & {{freq}} < 2 ~ {{posl}} + 4,
             dif < 6 & {{freq}} < 1.5 ~ {{posl}} + 3,
             TRUE ~ {{posl}}),
           poslabel_aj = ifelse(is.na(poslabel_aj), {{posl}}, poslabel_aj)) %>%
    mutate(dif2 = poslabel_aj - lag(poslabel_aj, order_by = {{xxn}}),
           poslabel_aj2  = case_when(
             dif2 < 3 ~ poslabel_aj + 2,
             TRUE ~ poslabel_aj))
}




mean_prop_estrato <- function(data, medida, peso = "NULL"){

  options(dplyr.summarise.inform = FALSE) #para que no aparezca el mensaje de agrupado

  if(!is_grouped_df(data)) stop("Los datos deben estar agrupados, 'dplyr::group_by()'")
  #se podria agregar el grupo como argumento en la funcion, pero se complica
  # si son varios porque hay que poner '...',

  f1 <- function(d) drop_na(d) %>%  mutate(freq = round((n/sum(n)*100), 1))
  f2 <- function(d) select(ungroup(drop_na(d)), media)

  if(rlang::ensym(peso) != "NULL"){ #con pesos
    #no estoy seguro si rlang::ensym() sea lo mas correcto, pero funciona U_U
    bind_cols(
      summarise(data,  n = sum({{peso}}, na.rm = TRUE)) %>% f1(),
      summarise(data, media = weighted.mean({{medida}}, w = {{peso}}, na.rm = TRUE)) %>% f2()
    )

  }else{ #sin pesos
    bind_cols(
      summarise(data, n = sum(n(), na.rm = TRUE)) %>% f1(),
      summarise(data, media = mean({{medida}}, na.rm = TRUE)) %>% f2()
    )
  }
}

# df %>%
#   group_by(gestion, sexo) %>%
#   mean_prop_estrato(medida)

# df %>%
#   group_by(gestion, sexo) %>%
#   mean_prop_estrato(medida, peso = peso)

#**************************************************************************************************************************

#media_estrato_dm
#calcula el rendimiento promedio de algun estrato con grupos/niveles/categorias

# media_estrato2(data, medida, peso, estrato, diseno, size_g)

#  data: bases de datos
#  medida: medida para calcular el promedio
#  peso: peso
#  estrato: grupo, caracteristica, por ej: sexo, gestion, likert colapsado, q1 y q4, solo dos grupos
#  diseno: si es TRUE, calcula pvalue usando el diseño muestral, tienen que estar todas las variables
#          asociadas (dni_est, cod_mod7, pikIE, Prob_alumno, gestion)
#  size_g: si es TRUE, calcula la g de hegdes [mmm, aqui deberiamos usar la varianza con dm?]

media_estrato_dm <- function(data, medida, peso, grupos, diseno = FALSE, size_g = FALSE){

  options(survey.lonely.psu = "certainty") #opcion de survey
  options(dplyr.summarise.inform = FALSE) #para que no aparezca el mensaje de agrupado

  d1 <- group_by(data, {{grupos}}) #datos agrupados

  temp <- d1 %>%
    summarise(media = weighted.mean({{medida}}, w = {{peso}}, na.rm = TRUE),
              VariW = Hmisc::wtd.var({{medida}}, w = {{peso}}, na.rm = TRUE),
              N = n()) %>%
    drop_na()

  d2 <- select(temp, 1:2) #tabla con el promedio

  if(size_g == TRUE){

    if(n_distinct(pull(data, {{grupos}})) > 2)
      stop("El grupo tiene más de dos categorias, no se puede calcular la g de hedges.
           En el futuro se podria implementar el eta (?)")

    a <- temp %>% #acomodaciones
      pivot_longer(col = c("media","VariW","N"), names_to = "var") %>%
      pivot_wider(names_from = 1, values_from = "value")

    #calculo de g
    v <- (a[1,2]-a[1,3])/sqrt(((a[3,2]-1)*a[2,2]+(a[3,3]-1)*a[2,3])/(a[3,2]+a[3,3]-2))

    d3 <- mutate(d2, size_g = pull(v)) #solo g
  }

  if(diseno == TRUE){

    if(n_distinct(pull(data, {{grupos}})) > 2)
      stop("Por el momento, para las pruebas de hipótesis, la función acepta estratos de dos grupos")

    vars_dis_m <- c("cod_mod7", "dni_est", "gestion", "pikIE", "Prob_alumno")
    if(!all(vars_dis_m %in% names(data)))
      stop("La base de datos no tiene las variables necesarias para implementar el diseño muestral,
            es necesario que esten presentes: 'cod_mod7', 'dni_est', 'estrato', 'pikIE' y 'Prob_alumno'")

    grupo <- names(attributes(d1)$groups)[1]
    medida_ <- enquo(medida)

    bd_design <- svydesign(data = data, id = ~cod_mod7+dni_est, strata = ~estrato,
                           fpc = ~pikIE+Prob_alumno, nest = TRUE, pps = "brewer")

    p_val <- svyttest(as.formula(paste(as_label(medida_), "~", grupo)), design = bd_design)$p.value

    d4 <- mutate(d2, p_val = p_val) #p.val sin g

    if(size_g == TRUE){d5 <- mutate(d3, p_val = p_val)} #p.val con g

  }

  if(diseno == TRUE & size_g == TRUE){return(d5)}
  else if (diseno == TRUE & size_g == FALSE){return(d4)}
  else if (diseno == FALSE & size_g == TRUE){return(d3)}
  else {return(d2)}

}

#ejemplos:
#media_estrato2(bd2, M500_EM_ESAE_6P_2020_CT, Peso_lectura,  sexo)

#*******************************************************************************************************************
# pairwise.svyttest -----
# lo mismo que pairwise.prop.test() pero con la informacion de svyttest
# ajusta los pvalues con el metodo de Bonferroni

#declaramos el diseño muestral
# md <- svydesign(data = fam_mat, id = ~cod_mod7+dni_est, strata = ~estrato, fpc = ~pikIE+Prob_alumno, nest = TRUE, pps = "brewer")
# pairwise.svyttest(md, "M500_EVA_4P_2021_MA", "p14_01")

# dm: diseño muestral [objeto de survey]
# var: medida
# g: variable con los grupos

pairwise.svyttest <- function(dm, var, g){

  categ <- levels(dm$variables[[g]])
  compara <- map_chr(1:length(categ), ~paste(categ[c(-.x)], collapse = "-"))
  f <- as.formula(paste0(var, "~", g))

  #aplicamos ttest para cada par
  tt <- suppressWarnings( map(categ, ~svyttest(f, subset(dm, get(g) != .x))) )

  #extraemos info
  pval <- map_dbl(tt, ~.x$p.value)
  pval_adj <- round(p.adjust(pval, method = "bonferroni"), 3)

  data <- data.frame(
    comparacion = compara,
    diff = map_dbl(tt, ~.x$estimate),
    pval_adj_bonf = pval_adj)

  return(data)

}
