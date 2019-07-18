\encoding{latin1} 
\name{CNs}
\alias{CNs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Condition Number with and without intercept}
\description{This function returns the Condition Number (CN) of the independent variables of a multiple linear model considering the intercept and without considering it. It also returns the increase produced by going from not taking into account the intercept to having it.
}
\usage{CNs(X)}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{A numeric design matrix that should contain more than one regressor (intercept included).}
}
 
\value{
  \item{CN1}{Condition Number without intercept.}
  \item{CN2}{Condition Number with intercept.}
  \item{increment}{Increase (in percentage) in the CN from CN1 to CN2.}
}
\references{
  D. A. Belsley (1991). Conditioning diagnostics: collinearity and weak data in regression. John Wiley & Sons, New York.

  L. R. Klein and A.S. Goldberger (1964). An economic model of the United States, 1929-1952. North Holland Publishing Company, Amsterdan.
  
  H. Theil (1971). Principles of Econometrics. John Wiley & Sons, New York.
}

\author{R. Salmerón (\email{romansg@ugr.es}) and C. García (\email{cbgarcia@ugr.es}).}

\seealso{\code{\link{lu}}, \code{\link{CN}}.}
\examples{
# Henri Theil's textile consumption data modified
data(theil)
head(theil)
cte = array(1,length(theil[,2]))
theil.X = cbind(cte,theil[,-(1:2)])
CNs(theil.X)

# Klein and Goldberger data on consumption and wage income
data(KG)
head(KG)
cte = array(1,length(KG[,1]))
KG.X = cbind(cte,KG[,-1])
CNs(KG.X)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Condition Number}% use one of  RShowDoc("KEYWORDS")

