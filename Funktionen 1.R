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
# Mache ich später fertig

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

kategoriesierung <- function(x, type = 1, ...){
  if(!is.ordered(x) && !is.numeric(x)){
    stop("x is not a factor or numeric")
  }
  quants <- quants(x, c(0, .25, .75, 1))
  
  return(list(paste("niedrig: zw. ", quants[1], " und ", quants[2]),
              paste("mittel: zw. ", quants[2], " und ", quants[3]),
              paste("hoch: zw. ", quants[3], " und ", quants[4])
              )
         )
}

# f

visualisierung <- function(a, b, c, d){
  
}