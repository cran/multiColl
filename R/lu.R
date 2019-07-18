lu <-
function(X){
  X = as.matrix(X)
  Xlu = matrix( , nrow=dim(X)[1], ncol=dim(X)[2])
  for(i in 1:dim(X)[2]){
    Xlu[,i] = X[,i]/norm(X[,i],"2")	
  }
  return(Xlu)
}
