% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/combine_AIC.R
\name{combine.AIC}
\alias{combine.AIC}
\title{Combining AICs}
\usage{
combine.AIC(AIC)
}
\arguments{
\item{AIC}{A vector of AICs to combine with length equal to the number of models completed (i.e. K).}
}
\value{
The overall fit of a model across all imputation models: the mean AIC +/- the standard error. Saved as a character.
}
\description{
Combines individual AIC estimates of separate  models to get a sense of overall model fit.
}
\details{
Used in stage 3 when combining WQS model fits used on different completely observed datasets.  Similar to combining WQS parameter estimates, the mean of individual AIC estimates is taken as the central tendency estimate of WQS model fit. The standard deviation of individual AIC estimates is the estimate how the
WQS model fit differs between the imputed datasets.

A vector of AICs may be generated from \code{\link{do.many.wqs}}.
}
\note{
If AIC is a vector with one element, the AIC is returned as a character rounded to the nearest whole number with a warning that AIC cannot be combined.
}
\examples{
#AICs from do.many.wqs() example are as follows.
bayes.AIC <- c(1295.380, 1295.669)
combine.AIC( bayes.AIC)

}
\concept{wqs pool}
