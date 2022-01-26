## Funktionen 1

# a
  deskrstatmetr <- function(x, na.rm = FALSE, ...){
    if(!is.numeric(x)){
      stop("x is not numeric")
    }
    return(c(summary(x, na.rm, ...), "sd" = sd(x, na.rm, ...)))
  }

# b
# Mache ich später fertig

diskrstatkat <- function(x){
  return(list(Summe = sum(x)))
}
  
  
