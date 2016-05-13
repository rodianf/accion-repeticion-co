# Funciones

tabla <- function(datos, caption) {
  caption <- paste0(caption, "\\textit{", "n=", "}", nrow(datos), ".")
  print(xtable(datos, caption = caption, 
               label= paste0("tab:", deparse(substitute(datos))), digits = 1), 
        include.rownames=FALSE, 
        file = paste0("res/", deparse(substitute(datos)), ".tex"), 
        caption.placement = "top", 
        table.placement = "H")
  
}

#tabla(reghec, "Número de sentencias tal. ")
