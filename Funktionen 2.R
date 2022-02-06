quants <- function(x, probs){
  return(as.vector(quantile(x, type = 1, probs = probs)))
}