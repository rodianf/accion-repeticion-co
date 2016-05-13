# Setup
library(googlesheets)
suppressMessages(library(dplyr))
library(xtable)
library(ggplot2)
library(scales)
library(reshape2)

#Funciones
source("scr/functions.R")

# Actualizar datos de Google Sheets
source("scr/import.R")

# Cargar datos en formato Rda
load("dat/datos.Rda")
load("dat/datos.dano.Rda")
load("dat/datos.fechas.Rda")

# Descriptivos -------------------------------------------------------------------------------------------
# Hechos -------------------------------------------------------------------------------------------------

## Fecha del hecho
fechas.m <- melt(datos.fechas[, -c(1,2)])
ggplot(fechas.m, aes(x = format(value, "%Y"))) +
  geom_bar() +
  facet_grid(variable ~ .)


## Region
reghec <- datos %>%
  group_by(Región.Hechos) %>%
  summarise(Sentencias = n()) %>%
  mutate(Porcentaje = round((Sentencias / sum(Sentencias))*100, 1))
colnames(reghec) <- c("Región", "Sentencias", "Porcentaje")
print(xtable(reghec, caption = paste0("Número de sentencias según la región lugar de los hechos. ", "\\textit{", "n=", "}", nrow(reghec), "."), 
             label= "tab:reghec", digits = 1), include.rownames=FALSE, file = "res/reghec.tex", caption.placement = "top", table.placement = "H")

## Departamento
dephec <- datos %>%
  group_by(Departamento.Hechos) %>%
  summarise(Sentencias = n()) %>%
  mutate(Porcentaje = round((Sentencias / sum(Sentencias))*100, 1))
colnames(dephec) <- c("Departamento", "Sentencias", "Porcentaje")
print(xtable(dephec, caption = paste0("Número de sentencias según el departamento lugar de los hechos. ", "\\textit{", "n=", "}", nrow(dephec), "."), label= "tab:dephec", digits = 1),
      include.rownames=FALSE, file = "res/dephec.tex", caption.placement = "top", table.placement = "H")

ggplot(dephec, aes(x = reorder(Departamento, Sentencias), y = Sentencias)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(x = "") +
  theme_bw()
ggsave("res/dephec.eps", height = 9, width = 13, units = "cm")

## Municipio
datos$munhec <- paste(datos$Municipio.Hechos, ", ", datos$Departamento.Hechos, sep = "")
munhec <- datos %>%
  group_by(munhec) %>%
  summarise(Sentencias = n()) %>%
  mutate(Porcentaje = round((Sentencias / sum(Sentencias))*100, 1))
colnames(munhec) <- c("Municipio", "Sentencias", "Porcentaje")
print(xtable(munhec, caption = paste0("Número de sentencias según el municipio lugar de los hechos. ", "\\textit{", "n=", "}", nrow(munhec), "."), 
             label= "tab:munhec", digits = 1), include.rownames=FALSE, file = "res/munhec.tex", caption.placement = "top", table.placement = "H")

munhecot <- munhec[ which(munhec$Sentencias == 1), ]
munhecto <- munhec[ which(munhec$Sentencias > 1), ]
munhecto <- rbind(munhecto, data.frame(Municipio = "Otros", Sentencias = sum(munhecot$Sentencias), 
                                       Porcentaje = round((sum(munhecot$Sentencias)/nrow(datos))*100, 1)))
ggplot(munhecto, aes(x = reorder(Municipio, Sentencias), y = Sentencias)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(x = "") +
  theme_bw()
ggsave("res/munhec.eps", height = 6, width = 10, units = "cm")

## Entidad estatal
entidad <- datos %>%
  group_by(Entidad.Estatal) %>%
  summarise(Sentencias = n()) %>%
  mutate(Porcentaje = round((Sentencias / sum(Sentencias))*100, 1))
colnames(entidad) <- c("Entidad estatal", "Sentencias", "Porcentaje")
print(xtable(entidad, caption = paste0("Número de sentencias por entidad estatal que incurrió en el daño. ", "\\textit{", "n=", "}", nrow(entidad), "."), 
             label = "tab:entidad", digits = 1), include.rownames=FALSE, file = "res/entidad.tex", caption.placement = "top", table.placement = "H")

ggplot(entidad, aes(x = reorder(`Entidad estatal`, Sentencias), y = Sentencias)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(x = "") +
  theme_bw()
ggsave("res/entidad.eps", height = 8, width = 15, units = "cm")

## Daño
dano <- datos.dano %>%
  group_by(Tipo.Dano) %>%
  summarise(Sentencias = n()) %>%
  mutate(Porcentaje = round((Sentencias / sum(Sentencias))*100, 1))
colnames(dano) <- c("Tipo de daño", "Sentencias", "Porcentaje")
print(xtable(dano, caption = paste0("Número de sentencias según el tipo de daño. ", "\\textit{", "n=", "}", nrow(dano), "."), 
             label = "tab:dano", digits = 1), include.rownames=FALSE, file = "res/dano.tex", caption.placement = "top", table.placement = "H")

ggplot(dano, aes(x = reorder(`Tipo de daño`, Sentencias), y = Sentencias)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(x = "") +
  theme_bw()
ggsave("res/dano.eps", height = 8, width = 15, units = "cm")

## Víctima
victima <- datos %>%
  group_by(Víctima) %>%
  summarise(Sentencias =n()) %>%
  mutate(Porcentaje = round((Sentencias / sum(Sentencias))*100, 1))
colnames(victima) <- c("Víctima", "Sentencias", "Porcentaje")
print(xtable(victima, caption = paste0("Número de sentencias según la víctima. ", "\\textit{", "n=", "}", nrow(victima), "."), 
             label = "tab:victima", digits = 1), include.rownames=FALSE, file = "res/victima.tex", caption.placement = "top", table.placement = "H")


## Acción de repetición
## Consejero ponente
consejero <- datos %>%
  group_by(Consejero.Ponente) %>%
  summarise(Sentencias = n()) %>%
  mutate(Porcentaje = round((Sentencias / sum(Sentencias))*100, 1))
colnames(consejero) <- c("Consejero ponente", "Sentencias", "Porcentaje")
print(xtable(consejero, caption = paste0("Número de sentencias por consejero ponente. ", "\\textit{", "n=", "}", nrow(consejero), "."), 
             label = "tab:consejero", digits = 1), include.rownames=FALSE, file = "res/consejero.tex", caption.placement = "top", table.placement = "H")

## Instancias
datos$Única.Instancia <- factor(datos$Única.Instancia, levels = c("No", "Si"), labels = c("Segunda", "Primera"))
instancia <- datos %>%
  group_by(Única.Instancia) %>%
  summarise(Sentencias = n()) %>%
  mutate(Porcentaje = (Sentencias / sum(Sentencias))*100)
colnames(instancia) <- c("Instancias", "Sentencias", "Porcentaje")
tabla(instancia, "Número de sentencias según instancias. ")
print(xtable(instancia, caption = "Número de sentencias por instancias.", label = "tab:instancia", digits = 1),
      include.rownames=FALSE, file = "res/instancia.tex", caption.placement = "top", table.placement = "H")

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
