\encoding{latin1} 
\name{SLM}
\alias{SLM}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Simple linear regression model and multicollinearity}
\description{The function analyzes the presence of near worrying multicollinearity in the Simple Linear Model (SLM).}
\usage{SLM(X, dummy = FALSE)}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{A numeric design matrix that should contain two independent variables (intercept included).}
  \item{dummy}{A logical value that indicates if there are dummy variables in the design matrix \code{X}. By default \code{dummy=FALSE}.}
}
\details{
 The analysis of the presence of near worrying multicolllinearity in the SLM has been systematically ignored in some existing statistical softwares.  However, it is possible to find worrying non essential multicollinearity in the SLM. In this case, the linear relation will be given by a second variable of \code{X} with very little variablity. For this reason, the coeficient of variation is calculated when the variable is quantitative and the proportion of ones if the variable is non-quantitative. 
}  
\value{
  If \code{dummy=TRUE}:
    \item{Prop}{Proportion of ones in the dummy variable.}
    \item{CN}{Condition Number of \code{X}.}
  If \code{dummy=FALSE}:
    \item{CV}{Coeficient of variation of the second variable in \code{X}.}
    \item{VIF}{Variance Inflation Factor.}
    \item{CN}{Condition Number of \code{X}.}
    \item{ki}{Stewart's index of \code{X}.}
}
\references{
  R. Salmerón, C. B. García and J. García (2018). Variance Inflation Factor and
Condition Number in multiple linear regression. Journal of Statistical Computation and Simulation, 88 (12), 2365-2384.

  L. R. Klein and A.S. Goldberger (1964). An economic model of the United States, 1929-1952. North Holland Publishing Company, Amsterdan.
  
  H. Theil (1971). Principles of Econometrics. John Wiley & Sons, New York.
}

\author{R. Salmerón (\email{romansg@ugr.es}) and C. García (\email{cbgarcia@ugr.es}).}

\note{The VIF only detects the near essential multicollinearity and for this reason it is not appropriate to detect multicollinearity in the SLM. Indeed, in this case, the VIF will be always equal to 1.}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{PROPs}}, \code{\link{CV}}, \code{\link{CN}}, \code{\link{ki}}.}

\examples{
# Henri Theil's textile consumption data modified
data(theil)
head(theil)
cte = array(1,length(theil[,2]))
theil.X = cbind(cte,theil[,-(1:2)])
SLM(theil.X, TRUE)

# Klein and Goldberger data on consumption and wage income
data(KG)
head(KG)
cte = array(1,length(KG[,1]))
KG.X = cbind(cte,KG[,-1])
SLM(KG.X)

# random
x1 = array(1,25)
x2 = sample(1:50,25)
x = cbind(x1,x2)
head(x)
SLM(x)

# random
x1 = array(1,25)
x2 = rnorm(25,100,1)
x = cbind(x1,x2)
head(x)
SLM(x)

# random
x1 = array(1,25)
x2 = sample(cbind(array(1,25),array(0,25)),25)
x = cbind(x1,x2)
head(x)
SLM(x, TRUE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Variation Inflation Factors}% use one of  RShowDoc("KEYWORDS")
\keyword{Coefficient of Variation}% __ONLY ONE__ keyword per line
\keyword{Condition Number}
\keyword{Stewart index}
