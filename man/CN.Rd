\encoding{latin1} 
\name{CN}
\alias{CN}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Condition Number}
\description{This function returns the Condition Number (CN) of the independent variables in a multiple linear regression.}
\usage{CN(X)}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{A numeric design matrix that should contain more than one regressor (intercept included).}
}
\details{
  Due to the CN takes into account the intercept, it allows to detect not only the essential but also the non-essential collinearity. It also allows to consider non-quantitative independent variables. 
  
  Its calculation is obtained from the function \code{lu}, contrary to the function \code{kappa}.
}
\value{The condition number of a matrix, that is, the maximum condition index.}
\references{
  D. A. Belsley (1991). Conditioning diagnostics: collinearity and weak dara in regression. John Wiley & Sons, New York.

  L. R. Klein and A.S. Goldberger (1964). An economic model of the United States, 1929-1952. North Holland Publishing Company, Amsterdan.
   
  H. Theil (1971). Principles of Econometrics. John Wiley & Sons, New York.
}
 
\author{R. Salmeron (\email{romansg@ugr.es}) and C. Garcia (\email{cbgarcia@ugr.es}).}

\note{Values of CN between 20 and 30 indicate near moderate multicollinearity while values higher than 30 indicate near worrying collinearity.}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{lu}}, \code{\link{kappa}}, \code{\link{CNs}}.}
\examples{
# Henri Theil's textile consumption data modified
data(theil)
head(theil)
cte = array(1,length(theil[,2]))
theil.X = cbind(cte,theil[,-(1:2)])
CN(theil.X)

# Klein and Goldberger data on consumption and wage income
data(KG)
head(KG)
cte = array(1,length(KG[,1]))
KG.X = cbind(cte,KG[,-1])
CN(KG.X)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Condition Number}% use one of  RShowDoc("KEYWORDS")
\keyword{Unit Length}% __ONLY ONE__ keyword per line
