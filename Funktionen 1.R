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

deskrstatbivar_metr_dich <- function(x, y){
  
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
  
}
