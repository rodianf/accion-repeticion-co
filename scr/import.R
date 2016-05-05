# Importar de Google Drive
# options(httr_oob_default=TRUE) #Autenticación en RStudio Server
# https://support.rstudio.com/hc/en-us/articles/217952868

gap <- gs_title("Datos Consejo de Estado") # Nombre del documento de Sheets en G Drive

# Importar hoja de datos R
datos <- gap %>% gs_read(ws = "R-datos", range = "A1:R65")
datos <- datos[-which(datos[,1] == 77), -c(11,12)] #Eliminar sentencias excluidas

# Importar hoja de datos Dano
datos.dano <- gap %>% gs_read(ws = "Dano", range = "A1:D67")
datos.dano <- datos.dano[-which(datos.dano[,1] == 77), -c(2,3)]

# Importar hoja de datos Fechas
datos.fechas <- gap %>% gs_read(ws = "R-fechas", range = "A1:I65")
datos.fechas <- datos.fechas[-which(datos.fechas[,1] == 77), ]

# Formatear clase fecha
datos.fechas[, 3:9] <- lapply(datos.fechas[, 3:9], as.Date, "%m/%d/%Y")

# Guardar datos en formato .RDa
save(datos, file = "dat/datos.Rda") #Datos
save(datos.dano, file = "dat/datos.dano.Rda") #Daño
save(datos.fechas, file = "dat/datos.fechas.Rda") #Fechas
