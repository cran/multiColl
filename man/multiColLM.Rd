\encoding{latin1} 
\name{multiColLM}
\alias{multiColLM}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{All detection measures}
\description{
The functions collects all the measure to detect near worrying multicollinearity existing in the package \code{multiCol}. In adddition, it provides the estimations by ordinary least squares (OLS) of the multiple linear regession model and the variations in the estimations of the coefficients as a consequence of changes in the observed data.
}
\usage{
multiColLM(y, X, dummy = FALSE, pos1 = NULL, n, mu, dv, tol = 0.01, pos2 = NULL)
} 
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{Observations of the dependent variable of the model.}
  \item{X}{Observations of the independent variables of the model (intercept included).}
  \item{dummy}{A logical value that indicates if there are dummy variables in the design matrix \code{X}. By default \code{dummy=FALSE}.}
  \item{pos1}{A numeric vector that indicates the position of the dummy variables, if these exist, in the design matrix \code{X}. By default \code{pos=NULL}.}
  \item{n}{Number of times that the perturbation is performed.}
  \item{mu}{Any real number.}
  \item{dv}{Any real positive number.}
  \item{tol}{A value between 0 and 1. By default \code{tol=0.01}.}
  \item{pos2}{A numeric vector that indicates the position of the independent variables to disturb once you eliminate in \code{data} the dependent variable and the intercept. By default \code{pos=NULL}.}
}

\value{
  The estimation by OLS of the linear regression model. 
  
  Percentiles 2.5 and 97.5 of the proportion of the variations in the estimations of the coefficients obtained from a perturbation of \code{tol}\% in the quantitative variables of \code{X}.
  
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

\seealso{\code{\link{SLM}}, \code{\link{CV}}, \code{\link{PROPs}}, \code{\link{RdetR}}, \code{\link{VIF}}, \code{\link{CN}}, \code{\link{ki}}, \code{\link{multiCol}}, \code{\link{perturb}}, \code{\link{perturb.n}}.}
\examples{
# Henri Theil's textile consumption data modified
data(theil)
head(theil)
cte = array(1,length(theil[,2]))
theil.X = cbind(cte,theil[,-(1:2)])
head(theil.X)
multiColLM(theil[,2], theil.X, dummy = TRUE, pos1 = 4, 5, 5, 5, tol=0.01, pos2 = 1:2)

# Klein and Goldberger data on consumption and wage income
data(KG)
head(KG)
cte = array(1,length(KG[,1]))
KG.X = cbind(cte,KG[,-1])
head(KG.X)
multiColLM(KG[,1], KG.X, n = 500, mu = 5, dv = 5, tol=0.01, pos2 = 1:3)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Detection}% use one of  RShowDoc("KEYWORDS")
\keyword{Multicollinearity}% __ONLY ONE__ keyword per line
