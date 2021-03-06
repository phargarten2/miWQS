% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make_quantile_matrix_WQS.R
\name{make.quantile.matrix}
\alias{make.quantile.matrix}
\title{Making Quantiles of Correlated Index}
\usage{
make.quantile.matrix(X, n.quantiles, place.bdls.in.Q1 = if (anyNA(X))
  TRUE else FALSE, ..., verbose = FALSE)
}
\arguments{
\item{X}{A numeric matrix. Any missing values are indicated by NA's.}

\item{n.quantiles}{An integer to specify the number of quantiles to be used categorizing the columns of X, e.g. in quartiles (q = 4), deciles (q = 10), or percentiles (q = 100). Default: 4L.}

\item{place.bdls.in.Q1}{Logical; if TRUE or X has any missing values, missing values in X are placed in the first quantile of the weighted sum.  Otherwise, the data is complete (no missing data) and the data is split equally into quantiles.}

\item{...}{Further arguments passed to or from other methods. Currently has no effect.}

\item{verbose}{Logical; if TRUE, prints more information. Useful to check for errors in the code. Default: FALSE.}
}
\value{
A matrix of quantiles with rows = nrow(X) and with columns = n.quantiles.
}
\description{
Scores quantiles from a numeric matrix. If the matrix has values missing between zero and some threshold, say the detection limit, all these missing values  (indicated by NA) go into the first quantile.
}
\details{
Produces sample quantiles for a matrix \emph{X} using \code{\link[stats]{quantile}}() function. Names are kept and the 7th quantile algorithm is used. As ties between quantiles may exist, \code{\link[base]{.bincode}}() is used.

When there is missing data (as indicated by NA's), \code{make.quantile.matrix} places all of the censored data into the first quantile. The remaining quantiles are evenly spread over the observed data. A printed message is displaced what the function does.
}
\note{
Developed as an accessory function for \code{estimate.wqs()}.
}
\examples{
# Example 1: Make quantiles for first nine chemicals using complete chemical data
data(simdata87)
q <- make.quantile.matrix(simdata87$X.true[, 1:9], 4)
q <- apply(q, 2, as.factor)
summary(q)

# Example 2: Place missing values of first nine chemicals in first quantiles
q2 <- make.quantile.matrix(simdata87$X.bdl[, 1:9], 4, verbose = TRUE)
summary(q2)
}
\seealso{
\code{\link[stats]{quantile}}

Other wqs: \code{\link{analyze.individually}},
  \code{\link{coef.wqs}}, \code{\link{do.many.wqs}},
  \code{\link{estimate.wqs}}, \code{\link{plot.wqs}},
  \code{\link{print.wqs}}
}
\concept{wqs}
\keyword{wqs}
