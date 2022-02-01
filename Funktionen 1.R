## Funktionen 1

# a
deskrstatmetr <- function(x, na.rm = FALSE, ...){
  if(!is.numeric(x)){
    stop("x is not numeric")
  }
  return(list(summary(x, na.rm, ...), "sd" = sd(x, na.rm, ...), 
              "10%-getrimmtes Mittel" = mean(x, trim = .1)))
}

# b
# Mache ich spaeter fertig

deskrstatkat <- function(x){
  if(!is.factor(x)){
    stop("x is not a factor")
  }
  return( list( Anzahl = length(x), "Modalwert" = max(table(x)) ) )
}

# c

deskrstatbivar_kat <- function(x, y){
  
}

# d

deskrstatbivar_metr_dich <- function(x, y){
  
}

# e

kategoriesierung <- function(x){
  
}

# f

visualisierung <- function(a, b, c, d){
  
}