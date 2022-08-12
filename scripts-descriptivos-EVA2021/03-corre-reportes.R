rm(list=ls())

#no funciona -_-
#reporte con todos
# dd <- format(Sys.Date(), "%d%m%y")
# 
# rmarkdown::render(
#   input = here("3-reportes", "1-descriptivos", "02-genera-reporte.Rmd"),
#   output_file = here("3-reportes", "1-descriptivos", paste0("descriptivos-ffaa-general-", dd, ".pdf"))
#   )



# reporte por cuestionario -----

#load(here("3-reportes", "1-descriptivos", "01-descriptivos-ffaa.Rdata"))
load(here("3-reportes", "1-descriptivos", "2-con-pesos", "01-descriptivos-ffaa-pesos.Rdata"))

cuest <- unique(tabla3$Concatena1)

for (i in 1:length(cuest)) { #i=1 
  rmarkdown::render(
    input = here("3-reportes", "1-descriptivos", "02-genera-reporte-varios.Rmd"),
    params = list(cuest = cuest[i]),
    output_file = here("3-reportes", "1-descriptivos", "2-con-pesos", "1-reporte-por-cuest", paste0(cuest[i], ".pdf"))
    )
}


# reporte por grado? 







#intento purrr
# reportes <- tibble::tibble(
#   input = here("3-reportes", "1-descriptivos", "02-genera-reporte-varios.Rmd"),
#   output_file = here("3-reportes", "1-descriptivos", "1-reporte-por-cuest", paste0(cuest, ".pdf")),
#   params = map(cuest, ~list(cuest = .))
#   )
# 
# reportes %>% pwalk(rmarkdown::render)
# sale mal!

 

