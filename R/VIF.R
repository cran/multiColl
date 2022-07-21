VIF <-
function(X, dummy=FALSE, pos=NULL){
  X = as.matrix(X)
  if (dim(X)[2] == 2){
   salida = "At least 3 independent variables are needed (including the intercept)"
  } else {
    x = as.matrix(X[,-1])
    vifs = diag(solve(cor(x)))
    if (dummy == TRUE){
      salida = vifs[-(pos-1)]
    } else {
      salida = vifs
    }
  }
  return(salida)
}
