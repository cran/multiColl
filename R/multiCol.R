multiCol <-
function(X, dummy=FALSE, pos=NULL){
  X = as.matrix(X)
  if (dim(X)[2] == 2){
    salida = SLM(X, dummy)
  } else {
    cvs = CVs(X, dummy, pos)
    props = PROPs(X, dummy, pos)
    R.detR = RdetR(X, dummy, pos)
    fivs = VIF(X, dummy, pos)
    ncs = CNs(X)
    k = ki(X, dummy, pos) 
    salida = list(cvs,props,R.detR,fivs,ncs,k)
    names(salida) = c("Coeficients of Variation" ,"Proportion of ones in the dummys variable", "R and det(R)", "Variance Inflation Factors", "CN", "ki")
  }
  return(salida)
}
