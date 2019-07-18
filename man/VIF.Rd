\encoding{latin1} 
\name{VIF}
\alias{VIF}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Variance Inflation Factor}
\description{The function returns the Variance Inflation Factors (VIFs) of the independent variables of the multiple linear regression model.}
\usage{VIF(X, dummy = FALSE, pos = NULL)}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{A numeric design matrix that should contain more than one regressor (intercept included).}
  \item{dummy}{A logical value that indicates if there are dummy variables in the design matrix \code{X}. By default \code{dummy=FALSE}.}
  \item{pos}{A numeric vector that indicates the position of the dummy variables, if these exist, in the design matrix \code{X}. By default \code{pos=NULL}.}
}
\details{
The function returns the VIFs from the main diagonal of the inverse of the matrix of correlations of the independent variables of the multiple linear regression. Due to the VIF is only calculated for the independent variables, it only allows to detect the essential collinearity.  In addition, the VIF is not adequate for dummy variables since it is obtained from the matrix of simple correlations.
}  
\value{Variance Inflation Factor of each independent variable excluded the intercept.}
\references{
  D. Marquardt and R. Snee (1975). Ridge regression in practice. The American Statistician, 1 (29), 3--20.
  
  L. R. Klein and A.S. Goldberger (1964). An economic model of the United States, 1929-1952. North Holland Publishing Company, Amsterdan.
  
  H. Theil (1971). Principles of Econometrics. John Wiley & Sons, New York.
}

\author{R. Salmerón (\email{romansg@ugr.es}) and C. García (\email{cbgarcia@ugr.es}).}

\note{Values of VIF that exceed 10 indicate near essential multicolinearity.}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{RdetR}}, \code{\link{ki}}.}
\examples{
# Henri Theil's textile consumption data modified
data(theil)
head(theil)
cte = array(1,length(theil[,2]))
theil.X = cbind(cte,theil[,-(1:2)])
VIF(theil.X, TRUE, pos = 4)

# Klein and Goldberger data on consumption and wage income
data(KG)
head(KG)
cte = array(1,length(KG[,1]))
KG.X = cbind(cte,KG[,-1])
VIF(KG.X)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Variation Inflation Factors}% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
