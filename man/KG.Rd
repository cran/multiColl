\encoding{latin1} 
\name{KG}
\alias{KG}
\docType{data}
\title{Klein and Goldberger data}
\description{Klein and Goldberger data on consumption and wage income.}
\usage{data("KG")}
\format{
  A data frame with 14 observations in relation to the following four variables:
  \describe{
    \item{\code{consumption}}{Domestic consumption.}
    \item{\code{wage.income}}{Wage income.}
    \item{\code{non.farm.income}}{Non-wage-non-farm income.}
    \item{\code{farm.income}}{Farm income.}
  }
}
\details{Data for the years 1942 to 1944 are not available for the war.}
 
\references{L. R. Klein and A.S. Goldberger (1964). An economic model of the United States, 1929-1952. North Holland Publishing Company, Amsterdan.}
\examples{
data(KG)
head(KG)
}
\keyword{datasets}