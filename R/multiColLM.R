multiColLM <-
function(y, X, dummy=FALSE, pos1=NULL, n, mu, dv, tol=0.01, pos2=NULL, graf=TRUE){
  x = as.matrix(X[,-1])
  reg = lm(y~x)
  datos = cbind(y,X)
  p.n = perturb.n(datos, n, mu, dv, tol, pos2)
  cuantiles = c(quantile(p.n[,2], prob=0.025), quantile(p.n[,2], prob=0.975))
  mc = multiCol(X, dummy, pos1, graf)
  salida = list(summary(reg), cuantiles, mc)
  names(salida) = c("Linear Model", "Perturbation", "multiCol")
  return(salida)
}
