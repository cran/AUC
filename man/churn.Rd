\name{churn}
\alias{churn}
\docType{data}
\title{ Churn data
}
\description{
    \code{churn} contains three variables: the churn predictions (probabilities) of two models, and observed churn
}
\usage{data(churn)}
\format{
  A data frame with 1302 observations, and 3 variables: \code{predictions}, \code{predictions2}, \code{churn}.
 }
 \author{
  Authors: Michel Ballings and Dirk Van den Poel,
  Maintainer: \email{Michel.Ballings@UGent.be}
}
\references{
  Ballings, M., Van den Poel, D., Threshold Independent Performance Measures for Probabilistic Classifcation Algorithms, Forthcoming.
}
\examples{
data(churn)
str(churn)
}
\keyword{datasets}
