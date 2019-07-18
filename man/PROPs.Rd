\encoding{latin1} 
\name{PROPs}
\alias{PROPs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Proportions}
\description{The functions returns the proportion of ones in the dummy variables existing in a matrix.}
\usage{PROPs(X, dummy = TRUE, pos = NULL)}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{A numeric matrix that should contain more than one regressor (intercept included).}
  \item{dummy}{A logical value that indicates if there are dummy variables in the matrix \code{X}. By default \code{dummy=TRUE}.}
  \item{pos}{A numeric vector that indicates the position of the dummy variables, if these exist, in the matrix \code{X}. By default \code{pos=NULL}.}
}

\author{R. Salmerón (\email{romansg@ugr.es}) and C. García (\email{cbgarcia@ugr.es}).}
 
\seealso{\code{\link{multiCol}}.}
\examples{
# random
x1 = sample(1:50, 25)
x2 = sample(1:50, 25)
x3 = sample(cbind(array(1,25), array(0,25)), 25)
x4 = sample(cbind(array(1,25), array(0,25)), 25)
x = cbind(x1, x2, x3, x4)
head(x)
PROPs(x, TRUE, pos = c(3,4))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Proportions}% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
