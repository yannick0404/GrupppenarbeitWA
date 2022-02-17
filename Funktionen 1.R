## Funktionen 1

# install.packages("fabricatr")
library(fabricatr)

# a
# Funktion fuer deskriptive Statistiken fuer metrische Variablen
deskrstatmetr <- function(x, na.rm = FALSE, ...){
  #pruefen ob x numerisch ist
  if(!is.numeric(x)){
    stop("x is not numeric")                                  
  }
  return(list(summary(x, na.rm, ...), "sd" = sd(x, na.rm, ...), 
              "10%-getrimmtes Mittel" = mean(x, trim = .1)))
}

# b

deskrstatkat <- function(x){
  if(!is.factor(x)){
    stop("x is not a factor")
  }
  return( list( Anzahl = length(x), "Modalwert" = max(table(x)) ) )
}

# c
# Funktion fuer deskriptive bivariate Statistiken fuer kategorielle Variablen
deskrstatbivar_kat <- function(x, y){
  if(!(is.factor(x) && is.factor(y))){
    stop("either x or y is not a factor")
  }
  return(list("Korrelation" = cor(x,y), "Kovarianz" = cov(x,y)))
}

# d

deskrstatbivar_metr_dich <- function(x, y, na.rm = T){
  if(!(is.numeric(x)|| is.numeric(y))){
    stop("no numeric input")
  }
  if(!(is.factor(x)|| is.factor(y))){
    stop("no factor input")
  }
  if(is.numeric(x) == is.numeric(y)){
    stop("x and y cannot be of the same type")
  }
  ## So tauschen, dass x immer numeric ist
  if(!is.numeric(x)){
    dummy = y
    y = x
    x = dummy
  }
  mean0 = mean(x[levels(y)[1]])
  mean1 = mean(x[levels(y)[2]])
  sd1 = sd(x[levels(y)[1]], na.rm = na.rm)
  sd2 = sd(x[levels(y)[2]], na.rm = na.rm)
  return(list("Mittel"=list(mean0, mean1), "Standardabweichungen" = list(sd0, sd1)))
}

# e
# kategorisierung - kategorisiert die Daten quantilsbasiert in die drei Kategorien
# "niedrig", "mittel" und "hoch"
# Input: x - mindestens ordinalskalierte Daten
# Output: Indizes mit Level

kategorisierung <- function(x){
  index <- split_quantile(x, 3)
  levels(index) <- c("niedrig", "mittel", "hoch")
  return(index)
}

# f

visualisierung <- function(a, b, c, d){
    par(mfrow= c(2,2))
  
hilfe_vis(a)
hilfe_vis(b)
hilfe_vis(c)
hilfe_vis(d)

}
