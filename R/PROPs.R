PROPs <-
function(X, dummy=TRUE, pos=NULL){
  if (dummy == TRUE){
    X = as.matrix(X[,c(pos)])
    salida = (colMeans(X))*100
  } else {
    salida = "At least one qualitative independent variable are needed (excluding the intercept)"
  }
  return(salida)
}
