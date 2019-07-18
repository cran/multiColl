\encoding{latin1} 
\name{perturb.n} 
\alias{perturb.n}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Perturbation and estimation in a multiple linear model}
\description{The function quantifies the variations in the estimations of the coefficients of a multiple linear regression  when a perturbation is introduced in the quantitative data set.}
\usage{perturb.n(data, n, mu, dv, tol = 0.01, pos = NULL)}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{Data set \code{(y, X)} where \code{y} and \code{X}  contain, respectively, the observations of the dependent variable and independients variables (intercept included) of the multiple linear regression.}
  \item{n}{Number of times that perturbation is performed.}
  \item{mu}{Any real number.}
  \item{dv}{Any real positive number.}
  \item{tol}{A value between 0 and 1. By default \code{tol=0.01}.}
  \item{pos}{A numeric vector that indicates the position of the independent variables to disturb once you eliminate in \code{data} the dependent variable and the intercept. By default \code{pos=NULL}.}
}

\value{
  \item{tols}{A vector presenting the percentage of disturbance induced in the variables indicated in each iteration.}
  \item{norms}{A vector presenting the percentage of variation in the estimations of the coefficients in each iteration. }
}
\references{
  D. Belsley (1982). Assessing the presence of harmfull collinearity and other forms of weak data throught a test for signal-to-noise. Journal of Econometrics, 20, 211-253.
  
  L. R. Klein and A.S. Goldberger (1964). An economic model of the United States, 1929-1952. North Holland Publishing Company, Amsterdan.
  
  H. Theil (1971). Principles of Econometrics. John Wiley & Sons, New York.
}

\author{R. Salmerón (\email{romansg@ugr.es}) and C. García (\email{cbgarcia@ugr.es}).}

\note{\code{tols} must be a constant vector equal to \code{tol}. It is obtained to check if data have been correctly perturbed.}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{perturb}}.}
\examples{
tol = 0.01
mu = 10
dv = 10

# Henri Theil's textile consumption data modified
data(theil)
head(theil)
cte = array(1,length(theil[,2]))
theil.y.X = cbind(theil[,2], cte, theil[,-(1:2)])
head(theil.y.X)

iterations = 5

perturb.n.T = perturb.n(theil.y.X, iterations, mu, dv, tol, pos = c(1,2))
perturb.n.T
mean(perturb.n.T[,1])
mean(perturb.n.T[,2])
c(min(perturb.n.T[,2]), max(perturb.n.T[,2]))

# Klein and Goldberger data on consumption and wage income
data(KG)
head(KG)
cte = array(1,length(KG[,1]))
KG.y.X = cbind(KG[,1], cte, KG[,-1])
head(KG.y.X)

iterations = 1000

perturb.n.KG = perturb.n(KG.y.X, iterations, mu, dv, tol, pos = c(1,2,3))
mean(perturb.n.KG[,1])
mean(perturb.n.KG[,2])
c(min(perturb.n.KG[,2]), max(perturb.n.KG[,2]))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Perturbation}% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
