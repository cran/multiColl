CVs <-
function(X, dummy=FALSE, pos=NULL){
  if (dummy == T){X = as.matrix(X[,-pos])}
  x = as.matrix(X[,-1])
  if (dim(x)[2] >= 1){
    cvs = array( , dim(x)[2])
    for(i in 1:dim(x)[2]){
      cvs[i] = CV(x[,i])	
    }
    salida = cvs
  }
  else {
    salida = "At least one quantitative independent variable are needed (excluding the intercept)"
  }
  return(salida)
}
