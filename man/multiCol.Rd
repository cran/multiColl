\encoding{latin1} 
\name{multiCol}
\alias{multiCol}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Collinearity detection in a linear regression model}
\description{The function collects all existing measures to detect worrying multicollinearity  in the package \code{multiCol}.}
\usage{multiCol(X, dummy = FALSE, pos = NULL)}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{A numeric design matrix that should contain more than one regressor (intercept included).}
  \item{dummy}{A logical value that indicates if there are dummy variables in the design matrix \code{X}. By default \code{dummy=FALSE}.}
  \item{pos}{A numeric vector that indicates the position of the dummy variables, if these exist, in the design matrix \code{X}. By default \code{pos=NULL}.}
}

\value{
  If X contains two independent variables (intercept included) see \code{SLM} function. 
   
  If X contains more than two independent variables (intercept included):
    \item{CV}{Coeficients of variation of quantitative variables in \code{X}.}
    \item{Prop}{Proportion of ones in the dummy variables.}
    \item{R}{Matrix correlation of the quantitative variables in \code{X}.}
    \item{detR}{Determinant of the matrix correlation of the quantitative variables in \code{X}.}
    \item{VIF}{Variance Inflation Factors of the quantitative variables in \code{X}.}
    \item{CN}{Condition Number of \code{X}.}
    \item{ki}{Stewart's index of the quantitative variables in \code{X}.}
}
\references{
  L. R. Klein and A.S. Goldberger (1964). An economic model of the United States, 1929-1952. North Holland Publishing Company, Amsterdan.
  
  H. Theil (1971). Principles of Econometrics. John Wiley & Sons, New York.
}

\author{R. Salmerón (\email{romansg@ugr.es}) and C. García (\email{cbgarcia@ugr.es}).}

\note{For more detail, see the help of the functions in \code{See Also}.}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{SLM}}, \code{\link{CV}}, \code{\link{PROPs}}, \code{\link{RdetR}}, \code{\link{VIF}}, \code{\link{CN}}, \code{\link{ki}}.}
\examples{
# Henri Theil's textile consumption data modified
data(theil)
head(theil)
cte = array(1,length(theil[,2]))
theil.X = cbind(cte,theil[,-(1:2)])
multiCol(theil.X, TRUE, pos = 4)

# Klein and Goldberger data on consumption and wage income
data(KG)
head(KG)
cte = array(1,length(KG[,1]))
KG.X = cbind(cte,KG[,-1])
multiCol(KG.X)

# random
x1 = array(1,25)
x2 = rnorm(25,100,1)
x = cbind(x1,x2)
head(x)
multiCol(x)

# random
x1 = array(1,25)
x2 = sample(cbind(array(1,25),array(0,25)),25)
x = cbind(x1,x2)
head(x)
multiCol(x, TRUE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Simple Linear Model}% use one of  RShowDoc("KEYWORDS")
\keyword{Coefficient of Variation}% __ONLY ONE__ keyword per line
\keyword{Proportions}
\keyword{Matrix corelation}
\keyword{Determinant of the matrix correlation}
\keyword{Variation Inflation Factors}
\keyword{Condition Number}
\keyword{Stewart index}
