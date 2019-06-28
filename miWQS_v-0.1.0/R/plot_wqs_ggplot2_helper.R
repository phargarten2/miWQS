#' Histograms of the Weights, Beta1, and WQS using \code{\link[ggplot2]{ggplot2}}
#'
#' @family wqs
#' @keywords wqs
#'
#' @description  Plots a WQS object as three histograms of the weights, the overall chemical effect, and WQS
#'  across bootstraps. These histograms are returned as \pkg{ggplot2} objects.
#'
#' @details
#' Three histograms are produced using \code{\link[ggplot2]{geom_histogram}} with ten bins.
#'
#' Once a Weighted Quantile Sum (WQS) regression is run, the \strong{hist.weights} is a panel of histograms. These are distributions of the weight estimates to determine which chemicals are important in the mixture. Each weight is between 0 and 1 and sum to 1. The individual bootstrapped weight estimates were used to construct the overall chemical index, WQS.
#'
#' The \strong{hist.beta1} is the distribution of the overall effect of the mixture on the outcome across bootstraps in the training dataset. Due to the constraint in WQS regression, these estimates are either all positive or all negative as dictated by \emph{b1.pos()} argument in \code{estimate.wqs}. The patterns detected here might be helpful in adjusting the signal function, which is controlled by \emph{signal.fn()} argument in \code{estimate.wqs}.
#'
#' The third histogram, \strong{hist.wqs}, shows the range of overall chemical index, or WQS, across each bootstrap. Due to constraints, this always is between 0 and \emph{n.quantiles - 1}.
#'
#' Plots no longer saved automatically; please save manually using \code{\link[ggplot2]{ggsave}}().
#'
#' @param x  An object of class "wqs", usually as a result of \code{\link{estimate.wqs}}.
#' @param filename  DEFUNCT; argument not used; files are no longer saved. Suggested Name is WQS_Plot.
#  #'@param hist.weight.max    DEFUNCT.  maximum weight estimate for x axis of histogram of weights.  Should be
#  #'                          same across all chemicals. Defaults to 1 to show the whole range.
#  #'@param no.weight.bins     Number of bins used to create weight histogram. Defaults to 15.
#  #'@param hist.freq.max      DEFUNCT. maximum frequency for histogram of weights. Should be same across all chemicals.
#' @param ...                  DEFUNCT. Arguments no longer passed to ggsave(). This argument currently has no effect.
#'
#' @return A list of histograms  \describe{
#'  \item{hist.weights}{A list of \pkg{ggplot2} histogram of weights across the bootstrap. Each component consists of a histogram with a weight estimate}
#'  \item{hist.beta1}{A histogram of the overall chemical mixture effect. This parameter is constrained to be all positive if the b1.pos argument in estimate.wqs() is TRUE.; otherwise, it is FALSE.}
#'  \item{hist.WQS}{A histogram of the overall chemical sum, WQS. Due to constraints, it is always between 0 and \emph{n.quantiles-1}.}
#'  }
#'
# TEMP #'@importFrom makeJournalTables is.integer is.wholenumber multiplot
#' @import ggplot2
#' @import grid
#' @importFrom tidyr gather
#' @export
#'
#' @examples
#' # Use simulated dataset and set seed for reproducibility.
#' data(simdata87)
#' set.seed(23456)
#' Wa <- estimate.wqs(y = simdata87$y.scenario, X = simdata87$X.true[, 1:9],
#'                   B = 10, family = "binomial")
#' plot(Wa)
plot.wqs <- function(x, filename = "myfile",  ...) {   # hist.weight.max = 1, hist.freq.max = 80,
    W <- x      # Let W be WQS object.
    if (length(grep(".png", filename)) > 0) {stop("Remove \".png \" from filename")}

    # Plot 1: histogram of bootstrap weights
      train.wts  <- as.data.frame(W$train.estimates[, -(1:6)])
      to.plot <- tidyr::gather(train.wts, key = "chemical", value = "estimate", factor_key = TRUE)
      hist.weights <- ggplot(to.plot, aes(to.plot$estimate, fill = to.plot$chemical)) +  theme_bw() +
        geom_histogram(bins = 10, color = "black")   +
        facet_wrap(~chemical, scales = 'fixed', dir = "h") +
        xlab("weight") + ggtitle("Chemical Weight Estimates Histogram") +
        guides(fill = FALSE)  # remove legend
      # ggsave(filename  = paste0(filename, "_weight.png"), plot = hist.weights, path = NULL, ... )

    # Plot 2: Beta 1.
      beta1 <- data.frame(beta1 = W$train.estimates$beta_1)
      hist.beta1 <- ggplot(data = beta1, aes(beta1)) + theme_bw() +
        geom_histogram(fill = "white", color = "black", bins = 10) +
        xlab("beta1") + ggtitle("Overall Mixture Effect (Beta1)") +
        xlim(0, ceiling(max(beta1))) #+ ylim(0, hist.freq.max)
      # ggsave( filename = paste0(filename, "_Beta1.png"), plot = hist.beta1, path = NULL, ...)

    # Plot 3: Distribution of WQS --Works but don't know if I want it.
      WQS <- data.frame(WQS = W$WQS)
      hist.WQS <- ggplot(data = WQS, aes(WQS)) + theme_bw() +
        geom_histogram(fill = "white", color = "black", bins = 10) +
        xlab("WQS") + ggtitle("WQS Histogram") +
        xlim(0, ceiling(max(beta1))) #+ ylim(0, hist.freq.max)
      # ggsave(filename =  paste0(filename, "_WQS.png"), plot = hist.WQS, path = NULL, ...)

   # cat("## Plots are saved in ", getwd(), "/", filename, ".png \n", sep ="")

  return(list(hist.weights = hist.weights, hist.beta1 = hist.beta1, hist.WQS = hist.WQS))
}





# c <- W$c   #Number of chemicals
# Plot 1 old code.
# If c <=9, create one plot.
# if( c < 10){
#
# } else { #Create multiple plots; one each for a multiple of 9.
#   times<-ceiling(c/9);
#   hist.weights <- vector( mode = "list", length = times)
#   for(j in 1:times){
#     start<- (9*(j-1)+1)   #Start at Number 1,10,1 ...
#     end<- min( 9*j , c)   #End at multiple of 9 (or c if not exact multiple)
#     #Plot
#     to.plot <- tidyr::gather( train.wts[ , start:end, drop = FALSE])
#     hist.weights[[j]] <- ggplot( to.plot, aes(to.plot$value)) +
#       geom_histogram(bins = 10, fill = "black") + theme_bw() +
#       facet_wrap(~ key, scales = 'fixed') +
#       xlab( "weight") + ggtitle( "Chemical Weight Estimates Histogram")
#     #Save Plot
#     ggsave( filename  = paste0(filename, "_weight-", LETTERS[j], ".png"), path = NULL) #, ...)
#     #Create new plot
#     grid::grid.newpage()
#   }
# }
#
