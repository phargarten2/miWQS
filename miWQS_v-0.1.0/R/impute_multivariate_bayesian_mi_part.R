
#' @title Multivariate Bayesian Imputation
#' @family imputation
#' @keywords imputation Bayesian

#'@description Function is in works. Included to collect all imputation arguments in one place.

## Arguments & Values -------------------------------------------------------------------------------
#' @param X   A numeric vector, matrix, or data-frame of chemical concentration levels with n subjects and C chemicals to be imputed. Missing values are indicated by NA's.  Ideally, a numeric matrix.
#' @param DL  The detection limit for each chemical as a numeric vector with length equal to C chemicals. Vector must be complete (no NA's); any chemical that has a missing detection limit is not imputed. If DL is a data-frame or matrix with 1 row or 1 column, it is forced as a numeric vector.
#' @param Z Any covariates used in imputing the chemical concentrations.  Ideally, a numeric matrix; however, Z can be a factor, vector, or data-frame. Assumed to be complete; observations with missing covariate variables are ignored in the imputation, with a warning printed. If none, enter NULL.
#' @param prior.coeff.mean The prior mean of number of covariates (p) x C coefficient matrix. The default, entered as NULL, will be a matrix of 1's, given by \code{\link[matrixNormal]{J}}.
#' @param prior.cov.mean  The prior mean of covariance matrix. The default, entered as NULL, is an identity matrix with size equal to the number of chemicals. Given by \code{\link[matrixNormal]{I}}.
#' @param initial An optional list of initial values to be specified for the Gibbs Sampler. The default is NULL, which means initial values are generated automatically. See details. If supplied, the list consists of three elements: (1) the coefficient matrix p x C Gamma.initial, (2) the covariance matrix, C x C Sigma.initial, and (3) vec.log.X.initial, a vector of initial log imputed values, which is vectorized by subject, n0C x T. (n0 is total # of missing values.)
# Like this better.
#   #' @param initial List of initial values in this order,   \describe{
#    \item{Gamma.initial}{coefficient matrix p x C}
#   \item{Sigma.initial}{covariance matrix, C x C}
#    \item{vec.log.X.initial}{vector of initial log imputed values, which is vectorized by subject, n0C x T}
#' @param T Number of total iterations for the Gibbs Sampler. Defaults: 1000L.
#' @param n.burn  The burn-in, which is the number of initial iterations to be discarded. Generally, the burn-in can be quite large as the imputed chemical matrices, X.imputed, are formed from the end of the chain -- the lowest state used is \eqn{T - 10*K}. Default is 1L (no burn-in).
#' @param K  A natural number of imputed datasets to generate. Defaults: 5L.
#' @inheritParams estimate.wqs

#'@return  nothing -- currently there is no function here.

impute.multivariate.bayesian <- function(X, DL, Z, prior.coeff.mean, prior.cov.mean, initial, T, n.burn, K, verbose){
  return()
}



