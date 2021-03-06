## Funktionen 2 bzw Helferfunktionen

hilfe_vis <- function(x,...){
  
  barplot(table(x),...)
}


# Hilfsfunktion fur boxplots
bp <- function(a,b,...){
  boxplot(a ~ b, col = "lightblue",...)
  for (i in 1:length(levels(b))) {
    points(i, mean(a[as.numeric(b)== i], na.rm = T), pch = 19, 
           col = "red", cex = 1)  
  }
}

