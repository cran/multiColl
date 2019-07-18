\encoding{latin1} 
\name{theil}
\alias{theil}
\docType{data}
\title{Henri Theil data}
\description{Henri Theil's textile consumption data modified.}
\usage{data("theil")}
\format{
  A data set with 17 observations in relation to the following five variables:
  \describe{
    \item{\code{obs}}{Year.}
    \item{\code{consume}}{Volume of textile consumption per capita (base 1925=100).}
    \item{\code{income}}{Real Income per capita (base 1925=100).}
    \item{\code{relprice}}{Relative price of textiles (base 1925=100).}
    \item{\code{twentys}}{Dummy variable that differentiates between the twenties and thirties.}
  }
}
\details{This data set is developed based on the original Henri Theil's textile consumption data. With the goal of showing the treatment of the detection of collinearity when non-quantitative variables exists in the multiple linear regression, a new dummy variable has been incorporated distinguishing between the twenties and thirties.}
 
\references{H. Theil (1971). Principles of Econometrics. John Wiley & Sons, New York.}
\examples{
data(theil)
head(theil)
}
\keyword{datasets}
