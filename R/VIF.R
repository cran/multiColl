VIF <- function(X, dummy=FALSE, pos=NULL) {
  X <- as.matrix(X)
  if (ncol(X) == 2) {
    return("At least 3 independent variables are needed (including the intercept)")
  }
  x <- X[, -1, drop = FALSE]
  vifs <- tryCatch(
    diag(solve(cor(x))),
    error = function(e) {
      message("System exactly/computationally singular. Modify the design matrix before running the code.")
      return(NULL)
    }
  )
  if(!is.null(vifs)) {
    if (dummy) {
      vifs[-(pos - 1)]
    } else {
      vifs
    }  
  }
}