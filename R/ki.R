ki <-
function(X, dummy=FALSE, pos=NULL)
{
  if (dummy == TRUE){X = as.matrix(X[,-c(pos)])}
  X = as.matrix(X)
  if (dim(X)[2] == 1){
    salida = "At least 2 quantitative independent variables are needed (excluding the intercept)"
  } else {
    ki = array(,dim(X)[2])
    for (i in 1:dim(X)[2]){
      ki[i] = crossprod(X[,i])/(crossprod(X[,i])-t(X[,i])%*%X[,-i]%*%solve(crossprod(X[,-i]))%*%t(X[,-i])%*%X[,i])
    }
    if (dim(X)[2] == 2){
      salida = ki
    } else {
      porc1 = (VIF(X)/ki[-1])*100
      porc2 = 100 - porc1
      salida = list(ki, porc1, porc2)
      names(salida) = c("Stewart index", "Proportion of essential collinearity in i-th independent variable (without intercept)", "Proportion of non-essential collinearity in i-th independent variable (without intercept)") 
    }
  }
  return(salida)
}
