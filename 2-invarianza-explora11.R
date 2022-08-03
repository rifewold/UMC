
library(tidyverse)
library(here)
library(lavaan)

# probemos invarianza


# (0) importamos bases de datos ----
lista = rio::import_list(Sys.glob(here("1-bases", "1-ffaa", "2-para-usar", "*.sav")), setclass = "tibble") %>%
  map(.,~rio::factorize(.))

bd <- lista$EVA2021_2Sdocente_EBRG1

bd1 <- select(bd, area, gestion2, starts_with("p17")) %>%
  mutate(across(starts_with("p17"), as.numeric),
         gestion2 = ifelse(gestion2 == "Estatal", 0, 1),
         area = ifelse(area == "Urbana", 0, 1))

mm <- paste0("V1=~", paste0(names(select(bd4, starts_with("p02"))), collapse = "+"))

m1_estrato <- cfa(mm, data = bd1, estimator = "WLSMV", group = "gestion2")
fitMeasures(m1_estrato, c("cfi","tli","rmsea","srmr"))

parameterEstimates(m1_estrato, standardized = TRUE) %>%
  filter(op == "=~") %>%
  select(Grupo = group, Ítem = rhs, Beta = std.all) %>%
  pivot_wider(names_from = Grupo, values_from = Beta)

# Equivalencia métrica
m1_estrato.Metrica <-cfa(mm, data=bd1, estimator="WLSMV", group="gestion2", group.equal=c("loadings"))
fitMeasures(m1_estrato.Metrica, c("cfi","tli","rmsea","srmr"))

# Equivalencia escalar
m1_estrato.Escalar <-cfa(mm, data=bd1, estimator="WLSMV", group="gestion2", group.equal=c("loadings","intercepts"))
fitMeasures(m1_estrato.Escalar, c("cfi","tli","rmsea","srmr"))

# Probar los modelos si son equivalentes.
comp.lect.invarianza <- anova(m1_estrato, m1_estrato.Metrica, m1_estrato.Escalar)

parameterTable(m_pool)

#**************************************************************************

#para uno

# cfap <- partial(cfa, estimator = "WLSMV")
# cfap(model = mm, data = bd1)

cfa_temp <- function(m, data, ...) cfa(model = m, data = data, estimator = "WLSMV", ...)

m_pool <- cfa_temp(mm, bd1)
m_conf <- cfa_temp(mm, bd1, group = "gestion2")
m_metr <- cfa_temp(mm, bd1, group = "gestion2", group.equal = c("loadings"))
m_esca <- cfa_temp(mm, bd1, group = "gestion2", group.equal = c("loadings", "intercepts"))

m_pool <- cfa(mm, bd1, estimator = "WLSMV")
m_conf <- update(m_pool, group = "gestion2")
m_metr <- update(m_conf, group.equal = c("loadings"))
m_esca <- update(m_conf, group.equal = c("loadings", "intercepts"))

list(m_pool, m_conf, m_metr, m_esca) %>%
  map_df(~fitMeasures(.x, c("cfi","tli","rmsea","srmr"))) %>%
  mutate_all(round, 4) %>%
  bind_cols(modelo = c("pooled", "configural", "metric", "scalar"), .)

#lo hacemos funcion

invarianza1 <- function(m, data, grupo){

  #m = string del modelo, "constructo=~item1+item2+..."
  #data = base de datos
  #grupo = grupo, "gestion2", "area", "sexo" o también puede ser c("gestion2","area)

  cfa_WLS <- function(...) cfa(estimator = "WLSMV", ...)

  m_pool <- map(grupo,~cfa_WLS(mm, data)) %>% set_names(grupo)
  m_conf <- map(grupo,~cfa_WLS(mm, data, group = .x)) %>% set_names(grupo)
  m_metr <- map(grupo,~cfa_WLS(mm, data, group = .x, group.equal = c("loadings"))) %>% set_names(grupo)
  m_esca <- map(grupo,~cfa_WLS(mm, data, group = .x, group.equal = c("loadings", "intercepts"))) %>% set_names(grupo)

  map_df(lst(m_pool,m_conf, m_metr, m_esca),
         function(x) map_df(x,~fitMeasures(.x, c("cfi","tli","rmsea","srmr")),.id="grupo"),.id="tip_inv") %>%
    as_tibble() %>%
    mutate(across(c(cfi:srmr),~round(as.numeric(.),4))) %>%
    pivot_longer(cols=c(cfi:srmr),names_to="indicadores",values_to="val") %>%
    pivot_wider(names_from =tip_inv,values_from = "val") %>%
    mutate(inv_metr = abs(round(m_metr - m_conf , 3)),
           inv_sca = abs(round(m_metr - m_esca, 3))) %>%
    mutate(inv_metr_t = case_when(
      indicadores %in% c("cfi", "tli") & inv_metr <= 0.020 ~ "cumple",
      indicadores %in% c("rmsea", "srmr") & inv_metr >= 0.030 ~ "cumple",
      TRUE~"no cumple"),
      inv_sca_t = case_when(
        indicadores %in% c("cfi", "tli") & inv_sca <= 0.010 ~ "cumple",
        indicadores %in% c("rmsea", "srmr") & inv_sca >= 0.010 ~ "cumple",
        TRUE~"no cumple"))
}

# invarianza2 <- function(m, data, grupo){
#
#   #m = string del modelo, "constructo=~item1+item2+..."
#   #data = base de datos
#   #grupo = grupo, "gestion2", "area", "sexo"
#
#   m_pool <- cfa(m, data, estimator = "WLSMV")
#   m_conf <- update(m_pool, group = grupo)
#   m_metr <- update(m_conf, group.equal = c("loadings"))
#   m_esca <- update(m_conf, group.equal = c("loadings", "intercepts"))
#
#   list(m_pool, m_conf, m_metr, m_esca) %>%
#     map_df(~fitMeasures(.x, c("cfi","tli","rmsea","srmr"))) %>%
#     mutate_all(round, 4) %>%
#     bind_cols(modelo = c("pooled", "configural", "metric", "scalar"), .)
#
# }

suppressWarnings(invarianza1(mm,cbind(bd4,gestion2=c("a","b"),area=c("b","a")),c("area")))



# Se evaluarán las soluciones configurales, métricas y escalares. Siguiendo la propuesta
# de Rutkowski & Svetina (2014), para la prueba de invarianza métrica, las diferencias
# deben ser de ΔCFI ≤ 0.020, ΔTLI ≤ 0.020, y ΔRMSEA ≥ 0.030. Para la prueba de
# varianza escalar, las diferencias entre los indicadores de ajuste deben ser de ΔCFI ≤
# 0.010, ΔTLI ≤ 0.010, y ΔRMSEA ≥ 0.010.

suppressMessages(

  list(m_pool, m_conf, m_metr, m_esca) %>%
    map_dfc(~fitMeasures(.x, indica)) %>%
    mutate_all(round, 4) %>%
    bind_cols(modelo = all_of(indica), .) %>%
    set_names(c("modelo", "pooled", "configural", "metric", "scalar")) %>%
    mutate(inv_metr = abs(round(configural - metric, 3)),
           inv_sca = abs(round(metric - scalar, 3))) %>%
    mutate(inv_metr_t = case_when(
      modelo %in% c("cfi", "tli") & inv_metr <= 0.020 ~ "ok",
      modelo %in% c("rmsea", "srmr") & inv_metr >= 0.030 ~ "ok"))
)

compareFit(m_conf, m_metr)

#funciona!

#para varios ?
