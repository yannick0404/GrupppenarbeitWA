## Funktionen 1

# install.packages("fabricatr")
library(fabricatr)

# a

# deskrstatmetr - gibt deskriptive Statistiken zu einem metrischen Merkmal aus
# Input: x - metrisch skaliertes Merkmal
# Output: Liste mit der Standardabweichung, der R-internen Summary und dem 
# 10%-getrimmten Mittel

deskrstatmetr <- function(x, na.rm = FALSE, ...){
  #pruefen ob x numerisch ist
  if(!is.numeric(x)){
    stop("x is not numeric")                                  
  }
  return(list(summary(x, na.rm, ...), "sd" = sd(x, na.rm, ...), 
              "10%-getrimmtes Mittel" = mean(x, trim = .1)))
}

# b

# deskrstatkat - gibt deskriptive Statistiken zu einem kategoriellen Merkmal aus
# Input: x - kategoriell skaliertes Merkmal
# Output: Liste mit Anzahl Elementen und dem Modalwert

modalwert <- function(x){
  for(i in 1:length(table(x))){
    if(table(x)[i] == max(table(x))){
      return(table(x)[i])
    }
  }
}
deskrstatkat <- function(x){
  if(!is.factor(x)){
    stop("x is not a factor")
  }
  return( list( Anzahl = length(x), "Modalwert" = modalwert(x) ) )
}

# c

# deskrstatbivar_kat - gibt deskriptive Statistiken zu zwei kategoriellen Variablen aus
# Input: x,y - kategoriell skaliertes Merkmal
# Output: Liste mit der Korrelation nach Kendall und der Kovarianz beider Merkmale

deskrstatbivar_kat <- function(x, y){
  if(!(is.factor(x) && is.factor(y))){
    stop("either x or y is not a factor")
  }
  return(list("Korrelation" = cor(x,y,method = "kendall"), "Kovarianz" = cov(x,y)))
}

# d

# deskrstatbivar_metr_dich - gibt deskriptive Statistiken zu einem metrischen und einem dichotomen Merkmal aus
# Input: x - metrisches Merkmal
# y - binaere Variable als Faktor
# Output: Liste mit den beiden Mittelwerten (aufgeteilt nach der dichotomen Variable)
# und Standardabweichungen und der gemeinsamen Korrelation

deskrstatbivar_metr_dich <- function(x, y, na.rm = T){
  if(!(is.numeric(x)|| is.numeric(y))){
    stop("no numeric input")
  }
  if(!(is.factor(x)|| is.factor(y))){
    stop("no factor input")
  }
  
  mean0 = mean(x[which(y == levels(y)[1])])
  mean1 = mean(x[which(y == levels(y)[2])])
  sd1 = sd(x[which(y == levels(y)[1])], na.rm = na.rm)
  sd2 = sd(x[which(y == levels(y)[2])], na.rm = na.rm)
  cor = cor(x, as.numeric(y), method = "pearson")
  return(list("Mittel"=c(mean0, mean1), "Standardabweichungen" = c(sd1, sd2),
              "Korrelation"= cor))
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

#Visualisierung mittels Barplot
# Ueber ... kann etwas an die barplot Funktion uebergeben werden

visualisierung <- function(a,...){
  hilfe_vis(a,...)
}


# Visualisierung mittels Boxplots, wobei a metrisch und b kategoriell.
# Ueber ... kann etwas an die boxplot Funktion uebergeben werden.
visualisierung2 <- function(a,b,...){
  bp(a, b,...)
}


#Visualisierung mittels Corrplot
#numerische Matrix noetig

#install.packages("corrplot")
library(corrplot)

ma <- function(a, b, c){
  matrix <- data.frame(a = a, b = b, c = c)
  return(matrix)
}
matrix<-ma(a, b, c)

cor_ma <- cor(matrix)
corrplot(cor_ma, method="number")
