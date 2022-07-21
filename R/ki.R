ki <-
function(X, dummy=FALSE, pos=NULL)
{
  X = as.matrix(X)
  ki = array(,dim(X)[2])
  for (i in 1:dim(X)[2]){
    ki[i] = crossprod(X[,i])/(crossprod(X[,i])-t(X[,i])%*%X[,-i]%*%solve(crossprod(X[,-i]))%*%t(X[,-i])%*%X[,i])
  }
  if (dim(X)[2] == 2){
    if (dummy == T){ salida = "At least one quantitative independent variable is needed (excluding the intercept)"}
    else { salida = ki }
  } else {
      if (dummy==TRUE){ki = ki[-pos]}
      porc1 = (VIF(X, dummy, pos)/ki[-1])*100
      porc2 = 100 - porc1
      salida = list(ki, porc1, porc2)
      names(salida) = c("Stewart index", "Proportion of essential collinearity in i-th independent variable (without intercept)", "Proportion of non-essential collinearity in i-th independent variable (without intercept)") 
  }
  return(salida)
}
