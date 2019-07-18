perturb <-
function(x, mu, dv, tol=0.01){
  p = rnorm(length(x), mu, dv)
  x.p = x + p*tol*(norm(x,"2")/norm(p,"2"))
  return(x.p)
}
