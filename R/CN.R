CN <-
function(X){
  X = as.matrix(X)
  X = lu(X)
  XX = crossprod(X)
  landas = eigen(XX)[[1]]
  nc = sqrt(max(landas)/min(landas))
  return(nc)
}
