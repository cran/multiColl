\encoding{latin1} 
\name{CVs}
\alias{CVs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Coeficients of Variation}
\description{The function returns the Coeficient of Variation (CV) of a matrix with quantitative columns.}
\usage{CVs(X, dummy = FALSE, pos = NULL)}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{A numeric design matrix that should contain more than one regressor (intercept included).}
  \item{dummy}{A logical value that indicates if there are dummy variables in the design matrix \code{X}. By default \code{dummy=FALSE}.}
  \item{pos}{A numeric vector that indicates the position of the dummy variables, if these exist, in the design matrix \code{X}. By default \code{pos=NULL}.}
}
\details{
  Due to the calculation of the CV only makes sense for quantitative data, other kind of data should be ignored in the calculation. For this reason, it is necessary to indicate if there are non-quantitative variables and also its position in the matrix. 
} 
\value{The CV of each column of \code{X}.}

\author{R. Salmerón (\email{romansg@ugr.es}) and C. García (\email{cbgarcia@ugr.es}).}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{CV}}.}
\examples{
# random

cte = array(1, 50)
x1 = sample(1:50, 25)
x2 = sample(1:50, 25)
Z = cbind(cte, x1, x2)
head(Z)
CVs(Z)

x3 = sample(c(array(1,25), array(0,25)), 25)
W = cbind(Z, x3)
head(W)
CVs(W, dummy=TRUE, pos = 4)

x0 = sample(c(array(1,25), array(0,25)), 25)
Y = cbind(cte, x0, x1, x2, x3)
head(Y)
CVs(Y, dummy=TRUE, pos=c(2,5))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Coefficient of Variation}% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
