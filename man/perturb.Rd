\encoding{latin1} 
\name{perturb} 
\alias{perturb}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Perturbation}
\description{The function modifies a set of quantitative data.}
\usage{perturb(x, mu, dv, tol = 0.01)}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{A numeric quantitative vector.}
  \item{mu}{Any real number.}
  \item{dv}{Any real positive number.}
  \item{tol}{A value between 0 and 1. By default \code{tol=0.01}.}
}
\details{The vector of data set is modified a \code{tol}\% by following the procedure presented by Belsley (1982).}
\value{The vector \code{x} modified a \code{tol}\%.}
\references{
  D. Belsley (1982). Assessing the presence of harmfull collinearity and other forms of weak data throught a test for signal-to-noise. Journal of Econometrics, 20, 211-253.
  
  L. R. Klein and A.S. Goldberger (1964). An economic model of the United States, 1929-1952. North Holland Publishing Company, Amsterdan.
  
  H. Theil (1971). Principles of Econometrics. John Wiley & Sons, New York.
}

\author{R. Salmerón (\email{romansg@ugr.es}) and C. García (\email{cbgarcia@ugr.es}).}

\seealso{\code{\link{perturb.n}}.}
\examples{
# Henri Theil's textile consumption data modified
data(theil)
head(theil)
consume.p1 = perturb(theil[,2], 3, 4, 0.01)
consume.p2 = perturb(theil[,2], 50, 10, 0.01)
x = cbind(theil[,2], consume.p1, consume.p2)
head(x)

# Klein and Goldberger data on consumption and wage income
data(KG)
head(KG)
farm.income.p1 = perturb(KG[,4], -3, 40, 0.01)
farm.income.p2 = perturb(KG[,4], 10, 8, 0.01)
x = cbind(KG[,4], farm.income.p1, farm.income.p2)
head(x)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Perturbation}% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
