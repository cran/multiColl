\encoding{latin1} 
\name{lu}
\alias{lu}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Unit length data}
\description{The function transforms the matrix \code{X} so that each column has unit length, it is to say, a module equal to 1.}
\usage{lu(X)}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{A numeric matrix that should contain more than one column.}
}

\value{Original matrix transformed so that each column has a module equal to 1.}
\references{
  R. Salmerón, C. B. García and J. García (2018). Variance Inflation Factor and
Condition Number in multiple linear regression. Journal of Statistical Computation and Simulation, 88 (12), 2365-2384.

  L. R. Klein and A.S. Goldberger (1964). An economic model of the United States, 1929-1952. North Holland Publishing Company, Amsterdan.
  
  H. Theil (1971). Principles of Econometrics. John Wiley & Sons, New York.
}
 
\author{R. Salmerón (\email{romansg@ugr.es}) and C. García (\email{cbgarcia@ugr.es}).}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{CN}}, \code{\link{CNs}}.}
\examples{
# Henri Theil's textile consumption data modified
data(theil)
head(theil)
cte = array(1,length(theil[,2]))
theil.X = cbind(cte,theil[,-(1:2)])
lu(theil.X)

# Klein and Goldberger data on consumption and wage income
data(KG)
head(KG)
cte = array(1,length(KG[,1]))
KG.X = cbind(cte,KG[,-1])
lu(KG.X)

# random
x1 = sample(1:10,5)
x2 = sample(1:10,5)
x = cbind(x1, x2)
x
norm(x[,1],"2")
norm(x[,2],"2")
x.lu = lu(x)
x.lu
norm(x.lu[,1],"2")
norm(x.lu[,2],"2")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Unit Length}% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
