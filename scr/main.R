# Setup
library(googlesheets)
suppressMessages(library(dplyr))
library(xtable)
library(ggplot2)
library(scales)
library(reshape2)

#Funciones
source("scr/functions.R")

# Cargar datos en formato Rda
load("dat/datos.Rda")
load("dat/datos.dano.Rda")



# Actualizar datos de Google Sheets
source("scr/import.R")

datos <- importar("datos") #Opción guardar = TRUE produce un archivo .Rda en la subcarpeta dat
dano <- importar("dano") #Daño


# Descriptivos -------------------------------------------------------------------------------------------
# Hechos -------------------------------------------------------------------------------------------------

## Region
tabla(datos, variable = "Región.Hechos", label = "reghec", colname = "Región",
      caption = "Número de sentencias según la región lugar de los hechos", 
      savet = TRUE)

## Departamento
tabla(datos, variable = "Departamento.Hechos", label = "dephec", colname = "Departamento",
      caption = "Número de sentencias según el departamento lugar de los hechos", 
      savet = TRUE, plot = TRUE, saveg = TRUE, height = 11, width = 13)

## Municipio
datos$munhec <- paste(datos$Municipio.Hechos, ", ", datos$Departamento.Hechos, sep = "")

munhec <- tabla(datos, variable = "munhec", label = "munhec", colname = "Municipio",
                caption = "Número de sentencias según el municipio lugar de los hechos",
                savet = FALSE)

munhec <- otros(munhec)

ggplot(munhec, aes(x = reorder(Municipio, Sentencias), y = Sentencias)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(x = "") +
  theme_bw()
ggsave("res/munhec.eps", height = 6, width = 10, units = "cm")

## Entidad estatal
tabla(datos, variable = "Entidad.Estatal", label = "entidad", colname = "Entidad estatal",
      caption = "Número de sentencias según la entidad estatal que incurrió en el daño", 
      savet = TRUE, plot = TRUE, saveg = TRUE, height = 13, width = 15.24)


## Daño
tabla(dano, variable = "Tipo.Dano", label = "dano", colname = "Tipo de daño",
      caption = "Número de sentencias según el tipo de daño", 
      savet = TRUE, plot = TRUE, saveg = TRUE, height = 8, width = 15)

## Víctima
tabla(datos, variable = "Víctima", label = "victima", colname = "Victima",
      caption = "Número de sentencias según la víctima", 
      savet = TRUE)

## Proceso de responsabilidad estatal -------------------------------------------------

# Región fallo
tabla(datos, variable = "Región.Fallo", label = "regfal", colname = "Región", 
      caption = "Número de sentencias según la región del fallo")
  
# Departamento fallo
tabla(datos, variable = "Departamento.Fallo", label = "depfal", colname = "Departamento",
      caption = "Número de sentencias según el departamento del fallo", plot = TRUE, 
      saveg = TRUE, savet = TRUE, height = 10, width= 15.24)

# Municipio fallo
datos$munfal <- paste0(datos$Municipio.Fallo, ", ", datos$Departamento.Fallo)
munfal <- tabla(datos, variable = "munfal", label = "munfal", colname = "Municipio",
      caption = "Número de sentencias según el municipio del fallo")

munfal <- otros(munfal)

ggplot(munfal, aes(x = reorder(Municipio, Sentencias), y = Sentencias)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(x = "") +
  theme_bw()
ggsave("res/munfal.eps", height = 6, width = 10, units = "cm")

## Indemnizacion
indem <- datos %>%
  summarise(Mínimo = min(Indemnizacion), "25%" = quantile(Indemnizacion, 0.25),
            Mediana = median(Indemnizacion), "75%" = quantile(Indemnizacion, 0.75),
            Máximo = max(Indemnizacion), Promedio = mean(Indemnizacion), 
            "Desviación estándar" = sd(Indemnizacion)) 
indem <- t(indem)
colnames(indem) <- "Indemnización"
indem <- as.data.frame(indem)
indem[,1] <- dollar_format()(indem[,1])

print(xtable(indem, caption = "Estadísticos del valor de la indemnización.", 
             label= "tab:indemnizacion", digits = 1), 
      include.rownames=TRUE, 
      file = "res/indemnizacion.tex", 
      caption.placement = "top", 
      table.placement = "H")


ggplot(datos, aes(x=factor(0), y=Indemnizacion)) +
  geom_boxplot() +
  scale_y_continuous(labels = dollar) +
  scale_x_discrete(breaks = NULL) +
  theme_bw() +
  labs(x = "", y = "\nIndemnización (COP)") +
  coord_flip()
ggsave("res/indemnizacion.eps", height = 5, width = 16, units = "cm")

## Acción de repetición -------------------------------------------------------
## Consejero ponente
tabla(datos, variable = "Consejero.Ponente", label = "consejero", colname = "Consejero ponente",
      caption = "Número de sentencias según el consejero ponente", savet = TRUE)

## Instancias
tabla(datos, variable = "Única.Instancia", label = "instancia", colname = "Instancias",
      caption = "Número de sentencias según instancias", savet = TRUE)

## Actor
actor <- datos %>%
  group_by(Actor) %>%
  summarise(Sentencias = n()) %>%
  mutate(Porcentaje = round((Sentencias / sum(Sentencias))*100, 1))
colnames(actor) <- c("Actor", "Sentencias", "Porcentaje")
print(xtable(actor, caption = paste0("Número de sentencias según el actor que incurrió en el daño. ", "\\textit{", "n=", "}", nrow(actor), "."), 
             label = "tab:actor", digits = 1, align = c("l","p{11cm}","r","r")), include.rownames=FALSE, file = "res/actor.tex", 
      caption.placement = "top", table.placement = "H", scalebox = 0.85)

actorot <- actor[ which(actor$Sentencias == 1), ]
actorto <- actor[ which(actor$Sentencias > 1), ]
actorto <- rbind(actorto, data.frame(Actor = "Otros", Sentencias = sum(actorot$Sentencias), 
                                     Porcentaje = round((sum(actorot$Sentencias)/nrow(datos))*100, 1)))

ggplot(actorto, aes(x = reorder(Actor, Sentencias), y = Sentencias)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(x = "") +
  theme_bw()
ggsave("res/actor.eps", height = 7, width = 15, units = "cm")


## Fallo
tabla(datos, variable = "Fallo.Culp", label = "fallo", colname = "Fallo",
      caption = "Número de sentencias según el fallo")

## Motivo improcedencia
datos.imp <- datos[!is.na(datos$Motivo.Improcedencia), ]
tabla(variable = "Motivo.Improcedencia", label = "improcedencia", colname = "Motivo de improcedencia",
      data = datos.imp, caption = "Número de sentencias según el motivo de improcedencia",
      plot = TRUE, saveg = TRUE)
rm(datos.imp)


## Reembolso
ggplot(datos, aes(x=factor(0), y=Valor.Reembolso)) +
  geom_boxplot() +
  scale_y_continuous(labels = dollar) +
  scale_x_discrete(breaks = NULL) +
  theme_bw() +
  labs(x = "", y = "\nReembolso (COP)") +
  coord_flip()
ggsave("res/reembolso.eps", height = 5, width = 16, units = "cm")


datos.reemb <- datos[!is.na(datos$Valor.Reembolso), ]
reemb <- datos.reemb %>%
  summarise(Mínimo = min(Valor.Reembolso), "25%" = quantile(Valor.Reembolso, 0.25),
            Mediana = median(Valor.Reembolso), "75%" = quantile(Valor.Reembolso, 0.75),
            Máximo = max(Valor.Reembolso), Promedio = mean(Valor.Reembolso), 
            "Desviación estándar" = sd(Valor.Reembolso)) 
reemb <- t(reemb)
colnames(reemb) <- "Reembolso"
reemb <- as.data.frame(reemb)
reemb[,1] <- dollar_format()(reemb[,1])

print(xtable(reemb, caption = "Estadísticos del valor del reembolso.", 
             label= "tab:reembolso", digits = 1), 
      include.rownames=TRUE, 
      file = "res/reembolso.tex", 
      caption.placement = "top", 
      table.placement = "H")
