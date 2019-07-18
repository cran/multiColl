CNs <-
function(X){
  X = as.matrix(X)
  if (dim(X)[2] == 2){
    salida = CN(X)
  } else {
    nc1 = CN(X[,-1])
    nc2 = CN(X)
    incremento = ((nc2-nc1)/nc2)*100
    salida = list(nc1, nc2, incremento)
    names(salida) = c("Condition Number without intercept", "Condition Number with intercept", "Increase (in percentage)")
  }
  return(salida)
}
