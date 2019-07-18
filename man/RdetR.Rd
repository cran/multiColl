\encoding{latin1} 
\name{RdetR}
\alias{RdetR}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Correlation matrix and it's determinat}
\description{
 The function returns the matrix of simple linear correlations between the independent variables of a multiple linear model and its determinant.  
}
\usage{RdetR(X, dummy = FALSE, pos = NULL)}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{A numeric design matrix that should contain more than one regressor (intercept included).}
  \item{dummy}{A logical value that indicates if there are dummy variables in the design matrix \code{X}. By default \code{dummy=FALSE}.}
  \item{pos}{A numeric vector that indicates the position of the dummy variables, if these exist, in the design matrix \code{X}. By default \code{pos=NULL}.}
} 
\details{
  The measures calculated by this function ignore completelly the role of the intercept in the linear relations between the independent variables. Thus, these measures only detect the near essential multicollinearity. Although the simple correlations only quantify relation between pairs of variables, the determinant of the matrix of correlations is able to detect broader relations.  Due to the coefficients of simple linear regression are calculated for quantitative variables, if the model contains other kinds of variables (such as dummy variables), they should be omitted in the analysis by using the arguments \code{dummy} and \code{pos}.
}
\value{
  \item{R}{Correlation matrix of the independent variables of the multiple linear regression model.}
  \item{detR}{Determinant of \code{R}.}
}
\references{
  C. García, R. Salmerón and C. B. García (2019). Choice of the ridge factor from the correlation matrix determinant. Journal of Statistical Computation and Simulation, 89 (2), 211-231.

  D. Marquardt and R. Snee (1975). Ridge regression in practice. The American Statistician, 1 (29), 3--20.
  
  L. R. Klein and A.S. Goldberger (1964). An economic model of the United States, 1929-1952. North Holland Publishing Company, Amsterdan.
  
  H. Theil (1971). Principles of Econometrics. John Wiley & Sons, New York.
}

\author{R. Salmerón (\email{romansg@ugr.es}) and C. García (\email{cbgarcia@ugr.es}).}

\note{
Values of the coefficient of simple linear correlation higher than 0.9487 imply worrying near essential multicollinearity between pairs of variables. 

Values of the determinant of \code{R} lower than  \code{0.1013 + 0.00008626*n - 0.01384*k}, where \code{n} is the number of observations and \code{k} the number of indepedent variables (intercept included), indicate worrying near essential multicollinearity.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{VIF}}.}
\examples{
# Henri Theil's textile consumption data modified
data(theil)
head(theil)
cte = array(1,length(theil[,2]))
theil.X = cbind(cte,theil[,-(1:2)])
RdetR(theil.X, TRUE, pos = 4)

# Klein and Goldberger data on consumption and wage income
data(KG)
head(KG)
cte = array(1,length(KG[,1]))
KG.X = cbind(cte,KG[,-1])
RdetR(KG.X)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Correlation matrix}% use one of  RShowDoc("KEYWORDS")
\keyword{Correlation matrix's determinant}% __ONLY ONE__ keyword per line
