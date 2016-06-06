# Importar de Google Drive
# options(httr_oob_default=TRUE) #Autenticación en RStudio Server
# https://support.rstudio.com/hc/en-us/articles/217952868

# importar
# Importa los datos de la hoja de cálculo de google.
#
# cargar = c("todos", "datos", "dano", "fecha")
# guardar = c("todos", "datos", "dano", "fecha", "ninguno")

importar <- function(cargar = "datos", guardar = FALSE) {
  suppressMessages(library(dplyr))
  library(googlesheets)

gap <- gs_title("Datos Consejo de Estado") # Nombre del documento de Sheets en G Drive

# Importar hoja de datos R
if (cargar == "todos" | cargar == "datos") {
  datos <- gap %>% gs_read(ws = "R-datos", range = "A1:R65")
  datos <- datos[-which(datos[,1] == 77), -c(11,12)] #Eliminar sentencias excluidas
  
  if (guardar == TRUE) save(datos, file = "dat/datos.Rda") #Datos
  
  return(datos)
}


# Importar hoja de datos Dano
if (cargar == "todos" | cargar == "dano") {
  datos.dano <- gap %>% gs_read(ws = "Dano", range = "A1:D67")
  datos.dano <- datos.dano[-which(datos.dano[,1] == 77), -c(2,3)]
  
  if (guardar == TRUE) save(datos, file = "dat/datos.dano.Rda") #Daño
  
  return(datos.dano)
}

# Importar hoja de datos Fechas
if (cargar == "todos" | cargar == "fechas") {
  datos.fechas <- gap %>% gs_read(ws = "R-fechas", range = "A1:I65")
  datos.fechas <- datos.fechas[-which(datos.fechas[,1] == 77), ]
  
  # Formatear clase fecha
  datos.fechas[, 3:9] <- lapply(datos.fechas[, 3:9], as.Date, "%m/%d/%Y")
  
  if (guardar == TRUE) save(datos, file = "dat/datos.fechas.Rda") #Fechas
  
  return(datos.fechas) 
}

}

#datos <- importar(cargar = "datos", guardar = T)

