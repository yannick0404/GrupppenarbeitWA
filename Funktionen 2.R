## Funktionen 2 bzw Helferfunktionen

quants <- function(x, probs){
  return(as.vector(quantile(x, type = 1, probs = probs)))
}

hilfe_vis <- function(x){
if(!is.factor(x)){
  stop("x is not a factor")
}
barplot(table(x))
}
