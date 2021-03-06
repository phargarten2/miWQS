% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_wqs_ggplot2_helper.R
\name{plot.wqs}
\alias{plot.wqs}
\title{Histograms of the Weights, Beta1, and WQS using \code{\link[ggplot2]{ggplot2}}}
\usage{
\method{plot}{wqs}(x, filename = "myfile", ...)
}
\arguments{
\item{x}{An object of class WQS, from \code{\link{estimate.wqs}}}

\item{filename}{DEFUNCT; argument not used. Files are no longer saved. Please save manually using ggsave().
name of file (without png extension) to save plots. Suggested Name is WQS_Plot.}

\item{...}{DEFUNCT.  Arguments passed to \code{\link[ggplot2]{ggsave}} for all plots. Changing the
filename, path, or device will have no effect.}
}
\value{
A list of histograms  \describe{
 \item{hist.weights}{A list of ggplot2 histogram of weights across the bootstrap. Each component consists of a histogram with a weight estimate}
 \item{hist.beta1}{A histogram of the overall chemical mixture effect. This parameter is constrained to be all positive if the b1.pos argument in estimate.wqs() is true. }
 \item{hist.WQS}{A histogram of the overall chemical sum, WQS. Due to constraints, it is always between 0 and 3.}
 }
}
\description{
Plots a WQS object as three histograms of the weights, the overall chemical effect, and WQS
 across bootstraps. These histograms are returned as ggplot2 objects.
}
\details{
Three histograms are produced using \code{\link[ggplot2]{geom_histogram}} with ten bins.  Plots may be saved as PNG files in the working directory using \code{\link[ggplot2]{ggsave}}().

Once a Weighted Quantile Sum (WQS) regression is run, the hist.weights is a panel of histograms. These are distributions of the weight estimates to determine which chemicals are important in the mixture. Each weight is between 0 and 1 and sum to 1. The individual bootstrapped weight estimates were used to construct the overall chemical index, WQS.

The hist.beta1 is the distribution of the overall effect of the mixture on the outcome across bootstraps in the training dataset. Due to the constraint in WQS regression, these estimates are either all positive or all negative as dictated by *b1.pos()* argument in \code{estimate.wqs}. The patterns detected here might be helpful in adjusting the signal function, which is controlled by *signal.fn()* argument in \code{estimate.wqs}.

The third histogram shows the range of overall chemical index, or WQS, across each bootstrap. Due to constraints, this always is between 0 and 3. This shows the range of average chemical mixtures considered in the analysis. This is returned as element hist.WQS or by looking at the file saved using ggsave().
}
\examples{
#Use simulated dataset and set seed for reproducibility.
data(simdata87)
set.seed(23456)
Wa <- estimate.wqs( y = simdata87$y.scenario, X = simdata87$X.true[ , 1:9],
                  B = 10, family = "binomial")
plot(Wa, filename = file.path(tempdir(), "WQS_Plot"))
}
\seealso{
Other wqs: \code{\link{analyze.individually}},
  \code{\link{coef.wqs}}, \code{\link{do.many.wqs}},
  \code{\link{estimate.wqs}},
  \code{\link{make.quantile.matrix}},
  \code{\link{print.wqs}}
}
\concept{wqs}
\keyword{wqs}
