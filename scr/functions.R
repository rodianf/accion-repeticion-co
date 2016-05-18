# Funciones
library(dplyr)
library(xtable)
library(ggplot2)
library(scales)
library(reshape2)

# xtabla
# Produce un archivo tex de la tabla generada por la función tabla

xtabla <- function(datos, caption, label) {
  library(xtable)
  
  caption <- paste0(caption, ". \\textit{", "n=", "}", nrow(datos), ".")
  print(xtable(datos, caption = caption, 
               label= paste0("tab:", label), digits = 1), 
        include.rownames=FALSE, 
        file = paste0("res/", label, ".tex"), 
        caption.placement = "top", 
        table.placement = "H")
}

#tabla(reghec, "Número de sentencias tal. ")

## tabla
## Produce una tabla de frecuencias a partir de una variable categórica
## 

tabla <- function(data = datos,
                  variable, 
                  label, 
                  colname = "Variable", 
                  caption = "Caption",
                  savet = FALSE,
                  plot = FALSE,
                  saveg = FALSE) {
  suppressMessages(library(dplyr))
  library(xtable)
  
  dat <- data %>%
    group_by_(variable) %>%
    summarise(Sentencias = n()) %>%
    mutate(Porcentaje = round((Sentencias / sum(Sentencias))*100, 1))
  colnames(dat) <- c(colname, "Sentencias", "Porcentaje")
  
  # Guardar tabla en formato .tex
  if (savet == TRUE) {
    xtabla(dat, caption, label)
  }
  
  # Mostrar gráfico y/o guardarlo
  if (plot == TRUE) {
    gbar(dat, saveg = saveg, label = label)
  }
  
  # Mostrar en pantalla o retornar la tabla en un objeto
  return(dat)
  print(dat)
}

#tabla(variable = "Región.Fallo", label = "bobo", colname = "Región", 
#       data = datos, caption = "Número de sentencias según la región del fallo",
#       plot = TRUE)


# gbar
# Produce un gráfico de barras con ggplot a partir de una tabla generada con la función tabla

gbar <- function(data, saveg = FALSE, height = NA, width = NA, label) {
  library(ggplot2)
  
  data <- as.data.frame(data)
  
  # Organizar las sentencias de mayor a menor
  data[,1] <- factor(data[,1], levels = data[ order(data[,2]), colnames(data)[1]])
  
  cols <- colnames(data)
  cols <- ggname(cols)
  # Generacion grafico en ggplot
  print(ggplot(data, aes_string(x = cols[1], y = cols[2])) +
          geom_bar(stat = "identity") +
          coord_flip() +
          labs(x = "") +
          theme_bw())
  
  if (saveg == TRUE) {
    ggsave(paste0("res/", label, ".eps"), height = height, width = width, units = "cm")
  }
  
}

gbar(dephec, saveg = TRUE, label = "rosa")


# ggname
# Añade tildes inversas a los nombres de las variables para ggplot2
ggname <- function(x) {
  if (class(x) != "character") {
    return(x)
  }
  y <- sapply(x, function(s) {
    if (!grepl("^`", s)) {
      s <- paste("`", s, sep="", collapse="")
    }
    if (!grepl("`$", s)) {
      s <- paste(s, "`", sep="", collapse="")
    }
  }
  )
  y 
}

# ------------------------------------------------------------
# Sandbox



