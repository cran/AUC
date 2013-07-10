\name{AUC-package}
\alias{AUC-package}
\alias{AUC}
\docType{package}
\title{
Threshold independent performance measures for probabilistic classifiers.
}
\description{
Summary and plotting functions for threshold independent performance measures for probabilistic classifiers.
}
\details{
This package includes functions to compute the area under the curve (function \code{auc}) of selected measures: The area under 
the sensitivity curve (AUSEC) (function \code{sensitivity}), the area under the specificity curve
(AUSPC) (function \code{specificity}), the area under the accuracy curve (AUACC) (function \code{accuracy}), and
the area under the receiver operating characteristic curve (AUROC) (function \code{roc}). The curves can also be 
visualized using the function \code{plot}. Support for partial areas is provided.
  
Auxiliary code in this package is adapted from the \code{ROCR} package. The measures available in this package are not available in the 
ROCR package or vice versa (except for the AUROC). As for the AUROC, we adapted the \code{ROCR} code to increase computational speed 
(so it can be used more effectively in objective functions). As a result less funtionality is offered (e.g., averaging cross validation runs). 
Please use the \code{ROCR} package for that purposes.
}
\author{
Michel Ballings and Dirk Van den Poel, Maintainer: \email{Michel.Ballings@UGent.be}

}
\references{
Ballings, M., Van den Poel, D., Threshold Independent Performance Measures for Probabilistic Classifcation Algorithms, Forthcoming.
}
\seealso{\code{\link{sensitivity}}, \code{\link{specificity}}, \code{\link{accuracy}}, \code{\link{roc}}, \code{\link{auc}}, \code{\link{plot}}  
}
\examples{
data(churn)

auc(sensitivity(churn$predictions,churn$labels))
auc(specificity(churn$predictions,churn$labels))
auc(accuracy(churn$predictions,churn$labels))
auc(roc(churn$predictions,churn$labels))

plot(sensitivity(churn$predictions,churn$labels))
plot(specificity(churn$predictions,churn$labels))
plot(accuracy(churn$predictions,churn$labels))
plot(roc(churn$predictions,churn$labels))

}

