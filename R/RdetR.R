RdetR <-
function(X, dummy=FALSE, pos=NULL){
  X = as.matrix(X)
  if (dim(X)[2] == 2){
    salida = "At least 3 independent variables are needed (including the intercept)"
  } else {
    if (dummy == TRUE){
      x = as.matrix(X[,-c(1,pos)])
      if ((dim(x)[2] == 0) || (dim(x)[2] == 1)){
        salida = "At least 2 quantitative independent variables are needed (excluding the intercept)"
      } else {
        R = cor(x)
        detR = det(R)
        salida = list(R, detR)
        names(salida) = c("Correlation matrix", "Correlation matrix's determinant")
      }
    } else {
      x = as.matrix(X[,-1])
      R = cor(x)
      detR = det(R)
      salida = list(R, detR)
      names(salida) = c("Correlation matrix", "Correlation matrix's determinant")
    }
  }
  return(salida)
}
