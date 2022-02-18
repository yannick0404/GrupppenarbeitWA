## Funktionen 2 bzw Helferfunktionen

hilfe_vis <- function(x){
  if(!is.factor(x)){
    stop("x is not a factor")
  }
  barplot(table(x))
}

hilfe_vis2 <- function(x){
if(!is.factor(x)){
  stop("x is not a factor")
}
boxplot(x)
}

# Hilfsfunktion fur boxplots
bp <- function(a,b,...){
  boxplot(a ~ b, col = "lightblue",...)
  for (i in 1:length(levels(b))) {
    points(i, mean(a[as.numeric(b)== i], na.rm = T), pch = 19, 
           col = "red", cex = 1)  
  }
}

