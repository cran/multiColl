\encoding{latin1} 
\name{ki}
\alias{ki}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Stewart's index}
\description{The function returns the index of Stewart of the independent variables in the multiple linear regession model.}
\usage{ki(X, dummy = FALSE, pos = NULL)}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{A numeric design matrix that should contain more than one regressor (intercept included).}
  \item{dummy}{A logical value that indicates if there are dummy variables in the design matrix \code{X}. By default \code{dummy=FALSE}.}
  \item{pos}{A numeric vector that indicates the position of the dummy variables, if these exist, in the design matrix \code{X}. By default \code{pos=NULL}.}
}
\details{
  The index of Stewart allows to detect the near essential and non-essential multicollinearity existing in a multiple linear regression model. In addition, due to its relation with the Variance Inflation Factor (VIF), it allows to calculate the proportion of essential and non-essential multicollinearity  in each independent variable (intercept excluded). The Stewart's index for the intercept indicates the degree of non-essential multicollinearity existing in the model.
   
  The relation of the the VIF with the index of Stewart implies that it should not be calculated for non-quantitative variables.
}
\value{
  \item{ki}{Stewart's index for each independent variable.}
  \item{porc1}{Proportion of essential multicollinearity in the i-th independent variable (without intercept).}
  \item{porc2}{Proportion of non-essential multicollinearity in the i-th independent variable (without intercept).}
}
\references{
  G. Stewart (1987). Collinearity and least squares regression. Statistical Science, 2 (1), 68-100.

  L. R. Klein and A.S. Goldberger (1964). An economic model of the United States, 1929-1952. North Holland Publishing Company, Amsterdan.
  
  H. Theil (1971). Principles of Econometrics. John Wiley & Sons, New York.
}

\author{R. Salmerón (\email{romansg@ugr.es}) and C. García (\email{cbgarcia@ugr.es}).}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{VIF}}.}
\examples{
# Henri Theil's textile consumption data modified
data(theil)
head(theil)
cte = array(1,length(theil[,2]))
theil.X = cbind(cte,theil[,-(1:2)])
ki(theil.X, TRUE, pos = 4)

# Klein and Goldberger data on consumption and wage income
data(KG)
head(KG)
cte = array(1,length(KG[,1]))
KG.X = cbind(cte,KG[,-1])
ki(KG.X)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Stewart index}% use one of  RShowDoc("KEYWORDS")
\keyword{Variation Inflation Factors}% __ONLY ONE__ keyword per line