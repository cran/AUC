\name{roc}
\alias{roc}
\title{Compute the receiver operating characteristic (ROC) curve.}
\usage{
  roc(predictions, labels)
}
\arguments{
  \item{predictions}{A numeric vector of classification
  probabilities (confidences, scores) of the positive
  event.}

  \item{labels}{A factor of observed class labels
  (responses) with the only allowed values \{0,1\}.}
}
\value{
  A list containing the following elements:
  \item{cutoffs}{A numeric vector of threshold values}
  \item{fpr}{A numeric vector of false positive rates
  corresponding to the threshold values} \item{tpr}{A
  numeric vector of true positive rates corresponding to
  the threshold values}
}
\description{
  This function computes the receiver operating
  characteristic (ROC) curve required for the \code{auc}
  function and the \code{plot} function.
}
\examples{
data(churn)

roc(churn$predictions,churn$labels)
}
\author{
  Authors: Michel Ballings and Dirk Van den Poel,
  Maintainer: \email{Michel.Ballings@UGent.be}
}
\references{
  Ballings, M., Van den Poel, D., Threshold Independent
  Performance Measures for Probabilistic Classifcation
  Algorithms, Forthcoming.
}
\seealso{
  \code{\link{sensitivity}}, \code{\link{specificity}},
  \code{\link{accuracy}}, \code{\link{roc}},
  \code{\link{auc}}, \code{\link{plot}}
}

