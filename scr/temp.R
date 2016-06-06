# Análisis temporal del proceso

# Relaciones a realizar
#
# Condena - Indemnización
# Tribunal - Fallo tribunal
# Consejo de Estado - Fallo Consejo de Estado
# Fallo Tribunal - Fallo Consejo de Estado

# Paquetes
library(dplyr)
library(reshape2)
library(ggplot2)
library(xtable)

# Importar datos
datos.fechas <- importar("fechas")

# Cargar datos Rda
load("dat/datos.fechas.Rda")

datos.fechas[,3:9] <- as.Date(datos.fechas[,3:9])

# Eventos
eventos <- c(Hechos = "Hechos",
             Condena = "Condena",
             Pago.Indemnización = "Pago indemnización",
             Tribunal = "Tribunal",
             Fallo.Tribunal = "Fallo tribunal",
             Consejo.Estado = "Consejo de Estado",
             Fallo.Consejo.Estado = "Fallo Consejo de Estado")

## Frecuencia anual según el evento
fechas.m <- melt(datos.fechas[, -c(1,2)])

fechaslab <- levels(fechas.m$variable)

fechas.m$variable <- factor(fechas.m$variable,
                            levels = fechaslab,
                            labels = eventos)

ggplot(fechas.m, 
       aes(x = format(value, "%Y"))) +
  geom_bar() +
  scale_x_discrete(breaks = seq(1990,2015, by = 5)) +
  #scale_y_discrete() +
  facet_grid(variable ~ ., labeller = label_wrap_gen(width=15)) +
  theme_bw() +
  theme(strip.text.y = element_text(size = 10),
        axis.text.x = element_text(vjust = 0),
        axis.title.y = element_text(vjust = 1)) +
  labs(x = "Año", y = "Sentencias")

ggsave("res/eventos.eps", height = 9.5, width = 8.5, units = "in")

# Hechos ----------------------

dif <- datos.fechas %>%
  mutate(Hechos.Condena = Condena - Hechos,
         Hechos.Condena.Y = Hechos.Condena / 365,
         Hechos.Ind = Pago.Indemnización - Hechos,
         Hechos.Ind.Y = Hechos.Ind / 365,
         Hechos.Trib = Tribunal - Hechos,
         Hechos.Trib.Y = Hechos.Trib / 365,
         Hechos.FTrib = Fallo.Tribunal - Hechos,
         Hechos.FTrib.Y = Hechos.FTrib / 365,
         Hechos.FCE = Fallo.Consejo.Estado - Hechos,
         Hechos.FCE.Y = Hechos.FCE / 365)

dif2 <- select(dif, ends_with("Y"))
dif2 <- melt(dif2)

dif2$variable <- factor(dif2$variable, labels = eventos[c(2:5,7)])

ggplot(dif2, aes(x=factor(0), y=value)) +
  geom_boxplot() +
  coord_flip() +
  scale_y_continuous(breaks = seq(0, 25, 5)) +
  scale_x_discrete(breaks = NULL) +
  facet_grid(variable ~ ., labeller = label_wrap_gen(width=15)) +
  theme_bw() +
  theme(strip.text.y = element_text(size = 10)) +
  labs(x = "", y = "\nAños")
  
# --------------------------------------------------------------------
# Otros eventos

dif3 <- datos.fechas %>%
  mutate(Condena.Indemnizacion = Pago.Indemnización - Condena,
         Condena.Indemnizacion.Y = Condena.Indemnizacion / 365,
         Tribunal.FT = Fallo.Tribunal - Tribunal,
         Tribunal.FT.Y = Tribunal.FT / 365,
         FT.FCE = Fallo.Consejo.Estado - Fallo.Tribunal,
         FT.FCE.Y = FT.FCE / 365,
         CE.FCE = Fallo.Consejo.Estado - Consejo.Estado,
         CE.FCE.Y = CE.FCE / 365)

dif3 <- select(dif3, ends_with("Y"))
dif3 <- melt(dif3)

dif3$variable <- factor(dif3$variable, levels = c("Condena.Indemnizacion.Y",
                                                  "Tribunal.FT.Y",
                                                  "FT.FCE.Y",
                                                  "CE.FCE.Y"),
                        labels = c("Condena - Indemnización",
                                   "Tribunal - Fallo Tribunal",
                                   "Fallo Tribunal - Fallo Consejo de Estado",
                                   "Consejo de Estado - Fallo Consejo de Estado"))

ggplot(dif3, aes(x=factor(0), y=value)) +
  geom_boxplot() +
  coord_flip() +
  scale_y_continuous(breaks = seq(0, 25, 5)) +
  scale_x_discrete(breaks = NULL) +
  facet_grid(variable ~ ., labeller = label_wrap_gen(width=15)) +
  theme_bw() +
  theme(strip.text.y = element_text(size = 10)) +
  labs(x = "", y = "\nAños")

# ----------------------------------------------------------------------------------
# Tablas

tabdif <- dif3 %>%
  group_by(variable) %>%
  summarise(Mínimo = min(value, na.rm = TRUE),
            Máximo = max(value, na.rm = TRUE),
            Promedio = mean(value, na.rm = TRUE),
            Desviación = sd(value, na.rm = TRUE)) %>%
  rename(Eventos = variable)


print(xtable(tabdif, caption = "Estadísticos de la distribución del 
             tiempo en años entre eventos", label = "tab:eventos3", digits = 2),
      include.rownames = FALSE)


tabdifhechos <- dif2 %>%
  group_by(variable) %>%
  summarise(Mínimo = min(value, na.rm = TRUE),
            Máximo = max(value, na.rm = TRUE),
            Promedio = mean(value, na.rm = TRUE),
            Desviación = sd(value, na.rm = TRUE)) %>%
  rename(Evento = variable)

print(xtable(tabdifhechos, caption = "Estadísticos de la distribución del 
             tiempo en años entre los hechos y otro evento del proceso", label = "tab:hechos-evento", digits = 2),
      include.rownames = FALSE)
