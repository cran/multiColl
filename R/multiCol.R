multiCol <-
function(X, dummy=FALSE, pos=NULL, graf=TRUE){
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
    if (graf==TRUE){
      plot(cvs, fivs, col="blue", lwd=3, xlim=c(0, max(cvs)), ylim=c(0, max(fivs)+10), xlab="Coefficient of Variation", ylab="Variance Inflation Factor")
      abline(h=10, col="red", lwd=2, lty=2)
      abline(v=0.1002506, col="red", lwd=2, lty=3)
      text(0.07, 2, "A", pos=2)
      text(0.07, (10+max(fivs)+10)/2, "B", pos=2)
      text((0.01+max(cvs))/2, (10+max(fivs)+10)/2, "C")
      text((0.01+max(cvs))/2, 2, "D")
      etiquetas = c()
      for (i in 1:length(cvs)){etiquetas = c(etiquetas, i+1)}
      text(cvs, fivs, labels = etiquetas, pos=1)
    }
    salida = list(cvs,props,R.detR,fivs,ncs,k)
    names(salida) = c("Coeficients of Variation" ,"Proportion of ones in the dummys variable", "R and det(R)", "Variance Inflation Factors", "CN", "ki")
  }
  return(salida)
}
