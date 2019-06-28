#########################################################################################################################
##  Univariate Bayesian MI : \impute.univariate.bayesian.mi  function
##  Date Created:
##  Last Updated: April 6, 2018

## Purpose of Document: A function to execute Bayesian MI in univariate setup.
## Notes: T = 5000 is sufficient. Burnin: 1000 (Geweke standard) ???
   # Thining does not sem like it matters here as I will just be doing random draws from posterior predictive distribution.
   # If chain failed to converge,

## Updates
# 3/11: * Reduced # of objects to be used in finding initial2.
#       * Within the function: Changed object name x.miss.initial  to log.x.miss.initial.

#########################################################################################################################

#' Univariate Bayesian Imputation

#' @family imputation Bayesian
#' @keywords imputation

#' @description
#' Given interval-censored data between 0 and different detection limits (\emph{DL}), \code{impute.univariate.bayesian.mi} generates K complete datasets using Univariate Bayesian Imputation.
#'
#' @details
#' This is the Univariate Bayesian Imputation approach. Only one chemical is imputed at a time. Both the observed and missing data are assumed to follow
#'  \deqn{ log( X_{ij} )  \sim^{indep} Norm(\mu_j , \sigma^2_j) ,   i=1,...n ; j=1,...C }
#' Subjects and chemicals are assumed to be independent. Jeffery's priors are  placed on mean and
#' variance for each chemical. Posterior simulation uses data augmentation approach. Initial values were
#' selected as convergence is
#' checked using Gelman-Rubin statistics. Given sample convergence, the K sets of posterior missing
#' values come from the burned Markov chains thinned by K. The imputed values are then substituted
#' for the missing data, forming K complete datasets.
#'
#' Each of the posterior parameters from MCMC chain--mu.post, sigma.post, and log.x.miss--is saved as a list of mcmc objects (in \pkg{coda}) of length # of chemicals. (A list was chosen since the number of missing values n0 might be different from chemical to chemical).
#'
#' @note  No seed is set in this function. Because bootstraps and MCMC are random, a seed should be set before every use.
#' @inheritParams impute.multivariate.bayesian
#' @return Returns a list that contains: ** Most important. \describe{
#'    \item{X.imputed}{** An array of n subjects x C chemicals x K imputed sets on the normal scale.}
#'    \item{mu.post}{A list with length equal to the number of chemicals,  where each element of list (or for each chemical) is the posterior MCMC chain of the mean is saved as T x 1 \code{\link[coda]{mcmc}} object (in \pkg{coda}).}
#'    \item{sigma.post}{A list with length equal to the number of chemicals, where each element of list (or for each chemical) is the posterior MCMC chain of the standard deviation, sigma, saved as T x 1 \strong{coda::mcmc} object.}
#'    \item{log.x.miss}{A list with length equal to the number of chemicals, where each element of list is a T x \eqn{n_{0j}} matrix of the log of the  imputed missing values, saved as \strong{coda::mcmc} object. \eqn{n_{0j}} is the # of missing values for the jth chemical.}
#'    \item{convgd.table}{A data-frame summarizing convergence with C rows and columns of the Gelman-Rubin statistic and whether the point estimate is less than 1.1. Summary is also printed to the screen.}
#'    \item{number.no.converged}{A check and summary of convgd.table. Total number of parameters that fail to indicate convergence of MCMC chains using Gelman-Rubin statistic. Should be 0.}
#'    \item{indicator.miss}{ A check; a vector of indicator variables where the number of missing values are above detection limit. Should be a vector of 0's.}
#' }

#' @examples
#' # Example 1: 10% BDLs Example -------------------------
#' # Sample Dataset 87, using 10% BDL Scenario
#' data(simdata87)
#' set.seed(472195)
#' result.imputed <- impute.univariate.bayesian.mi(
#'    X = simdata87$X.bdl[, 1:6], DL = simdata87$DL[1:6],
#'    T = 1000, n.burn = 50,  K = 2)
#' # Did the MCMC converge? A summary of Gelman Statistics is provided.
#' summary(result.imputed$convg.table)
#' # Summary of Impouted Values
#' apply(result.imputed$X.imputed, 2:3, summary)
 # To show examples for the accessory functions, save the dataset.
 # save( result.imputed, l.data, file = "./data/result_imputed.RData")

# TEMP  #'@importFrom makeJournalTables is.integer
#' @import coda
#    #MCMC analysis
## @import lattice
#' @importFrom utils head
#' @importFrom truncnorm rtruncnorm
    # Use of truncated normal in simulating posterior missing values. Used in sample.univariate.bayesian.
#' @importFrom invgamma rinvgamma
    # Use of inverse gamma prior in simulating posterior standard deviation values. Used in sample.univariat.bayesian.
#' @export

impute.univariate.bayesian.mi <- function(X, DL, T = 1000L, n.burn = 1L, K = 5L, verbose = FALSE) {
    ## Check for proper execution
    check <- check_imputation(X, DL, Z = NULL, T, n.burn, K, verbose)
    X  <- check$X
    DL <- check$DL
    # Z  <- check$Z  #No covariates in this function.
    K  <- check$K

  ## General Parameters
    # Split data into missing and observed
    x.list <- split(X, rep(1:ncol(X), each = nrow(X)))
    names(x.list) <- colnames(X)

    # These values are used in the sub-functions, but to avoid passing, they are just redefined within each sub-function.
    x.miss.index <-  apply(X, 2, function(x) { which(is.na(x))})  # a list
    C <- ncol(X)
    n <- nrow(X)
    n0 <- apply(X, 2, function(x) { sum(is.na(x)) }) # a vector
    chemical.name <- colnames(X)

  ## (1a) Sample posterior values from univariate Bayesian MI
    message("#> Start MCMC Data Augmentation Algorithm...")

    # Initial Values: (a) Mean of Log Observed Concentrations, (b) Standard Deviation of Substituted Concentrations; (C) log missing values come from uniform prior between 0 and detection limit. See generate.log.
    X.sub <- impute.sub(X, DL)
    log.x.mean <- colMeans(log(X), na.rm = TRUE)
    log.x.sigma <-  sqrt(diag(cov(log(X.sub),  method = "spearman")))
    # log.x.sigma <- log.x.sigma <-  sqrt( diag( cov(log(X), use="pairwise.complete.obs", method = "pearson") ) )
    log.uniform.initial <- generate.log.xmiss.prior (x.miss.type = "uniform", n0 = n0, DL = DL)
    initial <- list(mean.initial = as.list(log.x.mean),
                     sd.initial = as.list(log.x.sigma),
                     log.x.miss.initial =  log.uniform.initial)

    # Run MCMC
    posterior.samp <- sample.univariate.bayesian.mi(X, DL, T = T,   initial)
    if (verbose) { print(utils::head(posterior.samp$mu.post[[1]])) }

  ## (1b) Check for convergence ...
  message("#> Checking for convergence with 2nd chain ...")
    p <- 2 * C + sum(n0)           # total number of parameters
      # Create initial values that are more than twice the spread away. The spread is estimated naively using the
      # standard deviation of the chain, assuming observations are independent.
      f <- function(x) { x[[T]] + 2 * sd(window(x, start = n.burn)) }
      initial2 <- list(mean.initial = lapply(posterior.samp$mu.post, f),
                        sd.initial = lapply(posterior.samp$sigma.post, f),
                        log.x.miss.initial = generate.log.xmiss.prior(x.miss.type = "vague", n0 = n0, DL = DL))
      # The second chain
      chain2 <- sample.univariate.bayesian.mi(X, DL, T = T, initial2)

    # message("   Calculating Gelman Statistics ..."	)
      gelman.stat <- NA
      for (k in 1:3) { # For each parameter (mean, std, missing values)
        two.chain <- mapply(coda::mcmc.list, posterior.samp[[k]], chain2[[k]], SIMPLIFY = FALSE)     # Make MCMC Lists
        l <- lapply(two.chain, function(x) { coda::gelman.diag(x)$psrf[, 1]})    # Calculate Gelman Statistic
        gelman.stat <- c(gelman.stat, unlist(l, use.names = TRUE))
      }

    # A summary of gelman statistics
      gelman.summary <- data.frame(gelman.stat = gelman.stat[-1], is.converge = gelman.stat[-1] < 1.1)
      print(summary(gelman.summary))

    # Count the number of times convergence was achieved.
      number.converged <- sum(gelman.summary$is.converge)
      stopifnot(number.converged >= 0)  # error if total # of chains converged is negative!

    # Convergence occurred if number.converged = p. A nice message:
      if (number.converged  == p) {
        cat("#> Evidence suggests that all",  number.converged, "parameters have converged. \n")
        failed <- ""
      } else { # Implied: if( 0 < number.converged & number.converged < p){
        cat("#> The univariate imputation may have failed to converge;", number.converged,
            " chains of the total", p, "parameters have converged. \n"
            )
          # Print out the chains that did not converge
          failed <- gelman.summary[ !gelman.summary$is.converge, ]
          print(failed)
     }

    ## (1c) Remove burn-in using the first chain
         mu.post    <- lapply(posterior.samp$mu.post,    window, start = n.burn)
         sigma.post <- lapply(posterior.samp$sigma.post, window, start = n.burn)
         log.x.miss <- lapply(posterior.samp$log.x.miss, window, start = n.burn)

    ## (1d) Form K imputed complete datasets using the posterior predictive distribution of log.miss | log.obs..
    ##  Starting from a burned Markov chain, K missing values are taken every 10. The missing values from
    ##  these states replace the NAs in X.
      X.imputed <-  draw.imputed.samples(x.list, log.x.miss, K, T, n.burn, verbose)

    ## (1e) Check: Indicator of # of missing values above detection limit
      indicator.miss <- mapply(function(l1, l2) {  sum(l1 > log(l2)) },
                                log.x.miss,
                                DL
                                )
      cat("#> Check: Indicator of # of missing values above detection limit \n");
      print(sum(indicator.miss))

    return(list(
      X.imputed = X.imputed,
      mu.post = mu.post,
      sigma.post = sigma.post,
      log.x.miss = log.x.miss,
      convg.table = gelman.summary,
      number.no.converged = p - number.converged,
      indicator.miss = sum(indicator.miss)
    )
  )
}

#  warning("\n The seed is not set; the data generated is random. To ensure reproducibility, set the seed before execution of this function \n")
#  set.seed ( seed )
#

## Update the Markov Chain by doing more iterations.
## May not be worth it to have Markov Chain of different lengths for different dataset.
# Take the last values in the chain as inital values
# mu.initial <- unlist( lapply(mu.post, function(x) { x[ T] }) )
# sigma.initial <- unlist( lapply(sigma.post, function(x) { x[ T ] }) )
# Xmiss.initial <-   lapply(sigma.post, function(x) { x[ T ] })

## Run the iterations
# chain.update <- sample.univariate.bayesian.mi (X, DL, T = 5000,
#                               initial = list(mu.initial, sigma.initial, Xmiss.initial),
#                                 verbose = FALSE)

## Combine the samples.
# mu.post [[j]] <- as.mcmc( c( posterior.samp$mu.post[[j]], chain.update$mu.post [[j]]) )
# sigma.post [[j]]  <- as.mcmc( c( posterior.samp$sigma.post [[j]], chain.update$sigma.post[[j]]) )
# log.x.miss [[j]] <- as.mcmc( rbind( posterior.samp$log.x.miss [[j]],  chain.update$log.x.miss[[j]] ) )
# ```
#########################################################################################################################
##  Univariate Bayesian MI : (1a - ii) Sample Univariate Bayesian MI Posterior Functions
##  Date Created:
##  Last Updated: April 6, 2018

## Purpose: Given a likihood and a prior,  the sample.... functions generate a posterior sample using data augmentation techinque. See below for specifics.

# Log
# \item 10/17/17 : Tidied up code, ready. Two different functions for nonregression and regression approach.
# \item 5/7/18: SENT to David Wheeler, PhD. Made code neat.
#########################################################################################################################

# Accessory Function: MCMC Sampling for Univariate Bayesian Imputation Model
#
# #@family imputation
#
# @description Accessory Function: Sampling using Univariate Bayesian MI Likelihood.
#        Assuming
#        non-informative prior  for the mean and standard deviation for each chemical associated with likelihood
#        \deqn{log(X_{ij}) ~ iid Norm(\mu_{j}, \sigma^2_{j})},
#        this function undergoes the data augmentation technique.
#
# @details
# For internal use only.
#
# If  each chemical is assumed to come from a lognormal
# \deqn{  X_{ij}  \sim^{iid} Log-Norm(\mu_j , \sigma^2_{j}) ,   i=1,...n ; j=1,...C }
# with non-informative improper priors for the mean and standard deviation for each chemical
# \deqn{  \pi(\mu_{j}, \sigma^2_{j}) = \frac{1}{\sigma^2} }
# and a prior on the missing data given by \code{generate.log.xmiss.prior}, then the posterior is
# simulated using a data augmentation algorithm.
#  For details see: see the paper (citation here).
#
# The parameter \emph{initial} is a named list of lists: \describe{
#  \item{mean.initial}{Initial list of the mean parameter}
#  \item{sd.initial}{Initial list of standard deviation parameter}
#  \item{x.miss.initial}{Initial list of missing data. Probably called from \code{generate.log.xmiss.prior}.}   }
#
# @note  Values passed from impute.univariate.bayesian.mi needed for this function to work but are recalculated:
#        C, n, chemical.name, x.list, x.miss.index, n0.
#
# @references
# TBD
#
# @inheritParams impute.univariate.bayesian.mi
# @param initial A named list of initial values for MCMC chain. See details.
#
# @return The posterior parameters from MCMC chain is saved as a list of coda::mcmc objects of length # of chemicals.
#        (A list was chosen since the number of missing values n0 might be different from chemical to chemical)
#        \describe{
# \item{mu.post}{For each element of list (or for each chemical): the posterior MCMC
#                chain of the mean, saved as T x 1 coda::mcmc object.}
# \item{sigma.post}{For each element of list (or for each chemical): the posterior MCMC
#                chain for the standard deviation, sigma, saved as T x 1 coda::mcmc object.}
# \item{log.x.miss}{For each element of list, T x n0 matrix of the log of the imputed
#                 missing values is saved as coda::mcmc object.}
# }

# @importFrom truncnorm rtruncnorm
# @importFrom invgamma rinvgamma
# @import coda
# TEMP #@importFrom makeJournalTables my.summary
# #@export

sample.univariate.bayesian.mi <- function(X, DL, T = 8000, initial) {
  # require(truncnorm);  #Use of truncated normal to sample missing values.
  # require(invgamma);   #Use of inverse gamma prior
  # require(coda);       #Save as coda objects.

  # Info
  C <- ncol(X)
  n <- nrow(X)
  chemical.name <- colnames(X)

  # Making a list
  x.list <- split(X, rep(1:ncol(X), each = nrow(X)))
  names(x.list) <- colnames(X)

  # Set up posterior quantities for for loop.
  log.x.miss <- vector(mode = "list", length = C)
  mu.post <- vector(mode = "list", length = C) #--new ---
  sigma.post <- vector(mode = "list", length = C)
  names(log.x.miss) <- colnames(X)
  names(mu.post) <- paste0(colnames(X), ".mu")
  names(sigma.post) <- paste0(colnames(X), ".sigma")

  # For each chemical:
  for (j in 1:C) {
    # print(paste0("Start MCMC for chemical ", j))   #may be too much to print to screen.

    ## Set-up log of each chemical
    log.x <- log(x.list[[j]])
    x.miss.index <- which(is.na(x.list[[j]]))  # index of missing values
    n0 <- length(x.miss.index)  # number of missing values

    ## Set-up the Gibbs sampler.
    mu.post[[j]]   <- rep(NA, T + 1)
    sigma.post[[j]] <- rep(NA, T + 1)
    log.x.miss[[j]] <- matrix(NA, nrow = T + 1, ncol = n0,
                              dimnames = list(NULL, paste0("X.", j, ".miss.", x.miss.index))
    )

    ## Initial Values
    mu.post[[j]][1]    <-  initial$mean.initial[[j]]
    sigma.post[[j]][1] <-  initial$sd.initial[[j]]
    log.x.miss[[j]][1, ] <- initial$log.x.miss.initial[[j]]

    for (t in 1:T) {
      ### Form a complete dataset at the t^th iteration.
      log.x [x.miss.index] <-  log.x.miss[[j]] [t, ]

      ### Given complete data, I can calculate
      xbar <- mean(log.x)
      s2 <- var(log.x)

      ### Posterior of the standard deviation given complete data
      sigma.post[[j]][t + 1] <- sqrt(
        invgamma::rinvgamma(1, shape = (n - 1) / 2, rate = (n - 1) / 2 * s2)
        )
      # or, equivalently in terms of inverse-chi-sq
      # sqrt( (s2[t]*(n-1))*rinvchisq(1,df=n-1) )
      mu.post[[j]][t + 1] <- rnorm(1, xbar, sigma.post[[j]][t + 1] / sqrt(n))

      ### Imputation of x.miss via mixed accept -reject sampling
      log.x.miss[[j]][t + 1, ] <- t(
        log(
          truncnorm::rtruncnorm(n0,
                                a = 0,
                                b = DL[j],
                                mean  = mu.post[[j]][t + 1],
                                sd = sigma.post[[j]][t + 1]
          )
        )
      )

    } # end MCMC loop
  } # end chemical loop

  # Save as mcmc objects
  mu.post    <- lapply(mu.post, coda::mcmc)
  sigma.post <- lapply(sigma.post, coda::mcmc)
  log.x.miss <- lapply(log.x.miss,  coda::mcmc)

  return(list(mu.post = mu.post,
                 sigma.post = sigma.post,
                 log.x.miss = log.x.miss
  )
  )

} # end sample.univariate.bayesian.mi()

#########################################################################################################################
##  Univariate Bayesian MI : 1d_draw_imputed_samples
##  Date Created:
##  Last Updated: April 6, 2018

## Lastly  I draw K Multiple Imputed Datasets from the posterior predictive distribution

# Log
# \item 7/22/17: Tided up code to perserve order of imputed values. This is important for the WQS analysis.
# \item 11/9/17: Changed step so that those selected to be imputed decrease by 10. Confirmed through autocorr.diag and autocorr.plot that this is appriopriate. Made verbose more readable.
# \item 5/7/18: SENT to David Wheeler, PhD. Made code neat.
#########################################################################################################################


### (II) Accessory function: draw.imputed.samples  -----------------------------------

# Accessory function: Drawing Imputed Datasets
#
# #@family imputation
#
# @description  Creates a complete imputed dataset(s) from the posterior predictive distribution.
#
# Starting
#   from a burned Markov chain, K missing values are taken every 10th from end. The missing
#   values from these states replace the NAs in the original missing data matrix, X.
#
# @details
# For internal use only.If you would like to select different states than what \code{impute.univariate.bayesian.mi} selects,
# please see this function.
#
#  The K independent draws from the posterior predictive density justifies the multiple imputation approach.
#  Draws from the MCMC can be approximately independent by sampling the posterior missing values every
#  10th from the end of chain (T):
#  \deqn{t_{k}=T-(k-1)*10}   for k=1,2,…K imputations


# @param x.list      The missing data in list form. Passed from \code{impute.univariate.bayesian.mi}.
#                      Structure used to re-impute values
# @param log.x.miss  The processed MCMC posterior sample of the log(missing data) generated by a
#                       sample.XXX function. The sample has already been burned.
# @inheritParams impute.univariate.bayesian.mi
#  K, T, n.burn, verbose: passed from impute.bayesian.mi.
#  T, n.burn: Needed to find length of log.x.miss.
#  Passed from impute.bayesian.mi, these values are needed for this function to work but are recomputed to avoid passing them:
#  C, n, chemical.name, x.miss.index

# @return X.imputed:  An array of n subjects x C chemicals x K imputed sets on the normal scale.

# @examples

# ### Already have burned MCMC chains obtained from sample.univariate functions and
# ###    impute.univariate.bayesian.mi example. Used result_imputed ...
# ## If need to rerun:
# ##  data(simdata87)
# ## set.seed(472195)
# ## result.imputed <- impute.univariate.bayesian.mi( X = simdata87$X.bdl, DL = simdata87$DL,
#                                                T = 6000, n.burn = 400,  K = 3)
#
# print("Draw Single Imputed Set")
#  X.imputed1 <- draw.imputed.samples(x.list, result.imputed$log.x.miss,
#                                   K = 1, T = 6000, n.burn = 400,  verbose = TRUE)
# print("Draw Many Imputed Sets")
# X.imputed2 <- draw.imputed.samples(x.list, result.imputed$log.x.miss,
#                                   K = 2, T = 6000, n.burn = 400,  verbose = FALSE)
#  X.imputed3 <- draw.imputed.samples(x.list, result.imputed$log.x.miss,
#                                    K = 2, T = 6000, n.burn = 400,  verbose = TRUE)
#
# #10? Why not?
# #X.imputed <- draw.imputed.samples(x.list, result.imputed$log.x.miss,
#                                     K = 10, T = 6000, n.burn = 400,  verbose = TRUE)
# #Generalizing the loop
# # for(k in 1:K){
# #  log.x <- lapply(x.list, log)
# #  x.miss.index <- lapply(x.list, function(x){ which(is.na(x))} )
# #}

#
# Helper Function not called by user #'@export


draw.imputed.samples <- function(x.list, log.x.miss, K, T,
                               n.burn, verbose = FALSE) {

  ## Info
  C <- length(x.list)
  n <- length(x.list[[1]])
  # n0 <- unlist ( lapply(x.list, function(x){ sum(is.na(x) ) } ) )  #number non-detects. Not needed now but may in future.
  chemical.name <- names(x.list)

  ## Length of burned/thinned posterior missing values.
  mcmc.length <-   T - n.burn     # or nrow(log.x.miss[[1]])
  step <- 10                       # verified via looking at autocorr #trunc( mcmc.length / K )
  state  <- seq(mcmc.length, mcmc.length - (K - 1) * step, length = K)
  cat("#> Draw",  K, "Multiple Imputed Set(s) from states \n")
  print(n.burn + state)

  ## Draw ...
  X.imputed <- array(NA,  dim = c(n, C, K),  dimnames = list(NULL, chemical.name,  paste0("Imputed.", 1:K)))
  for (k in 1:K) {
    for (j in 1:C) {
      log.x <- log(x.list[[j]])                # Start with the log of observed data.
      x.miss.index <- which(is.na(x.list[[j]]))  # Find where the missing values are
      # Impute the missing values from the "state"th step of log(x.miss) from MCMC.
      log.x [x.miss.index] <-  log.x.miss[[j]] [ state[k], ]
      X.imputed[, j, k] <- exp(log.x)            # return on normal scale.

      # If the posterior of log.x.miss is just a posterior predictive distribution ....
      if (verbose) { print(paste0("chemical ", j, "; imputed draw ", k, ";", state[k])) }
    }
  }

  if (verbose) {
    cat("\n Imputed Means \n")
    A <-  apply(X.imputed, 2:3, mean)
    observed.means <- unlist(lapply(x.list, mean, na.rm = TRUE))
    print (data.frame(observed = observed.means, A))

    cat(" \n Number of NAs \n")
    print(as.data.frame(apply(X.imputed, 2:3, function(x) { sum(is.na(x)) })))
  }

  return(X.imputed)  # return on normal scale.
}
