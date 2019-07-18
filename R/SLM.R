SLM <-
function(X, dummy=FALSE){
  X = as.matrix(X)
  if (dim(X)[2] == 2){
    if (dummy == TRUE){
      prop = mean(X[,2])*100
      nc = CN(X)
      salida = list(prop,nc)
      names(salida) = c("Proportion of ones in the dummy variable", "Condition Number")
    } else {
      cv = CV(X[,2])
      FIVs = 1
      nc = CN(X)
      k = ki(X)
      salida = list(cv,FIVs,nc,k)
      names(salida) = c("Coeficient of Variation" ,"Variance Inflation Factor", "Condition Number", "Stewart index")
    }
  } else {
    salida = "Only 2 independent variables are needed (including the intercept)"
  }
  return(salida)
}
