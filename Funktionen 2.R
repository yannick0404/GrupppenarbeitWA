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
