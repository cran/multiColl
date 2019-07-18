perturb.n <-
function(data, n, mu, dv, tol=0.01, pos=NULL){
  y = data[,1]
  x = as.matrix(data[,-c(1,2)])
  #
  reg = lm(y~x)
  summary(reg)
  beta = as.double(reg$coefficients)
  #
  tols = array(, n)
  normas = array(, n)
  for (j in 1:n){
    x.p = x
    for (i in pos){
      x.p[,i] = perturb(x[,i], mu, dv, tol)
    }
    reg.p = lm(y~x.p)
    summary(reg.p)
    beta.p = as.double(reg.p$coefficients)
    #
    tole = matrix(, 1, pos)
    for (i in pos){
      tole[i] = (norm(x[,i]-x.p[,i],"2")/norm(x[,i],"2"))*100
    }
    tols[j] = mean(tole)
    #
    normas[j] = (norm(beta-beta.p,"2")/norm(beta,"2"))*100
  }
  ##
  return(cbind(tols,normas))
}
