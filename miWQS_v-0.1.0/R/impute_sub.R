#' Imputing by Substitution
#'
#' @family imputation
#' @keywords imputation
#'
#' @description Imputes the values below the detection limit with 1/sqrt(2) of that's chemical's detection limit.
#'
#' @details A matrix of components \emph{X} are interval-censored between zero and different detection limits \emph{DL}. Although \emph{X} may refer to a variable with no obvious \emph{DL}, we consider chemical concentrations \emph{X} with each being partially observed.
#'
#' @inheritParams impute.multivariate.bayesian
#' @return A n x C matrix where the BDL values of each chemical are substituted by its detection limit/sqrt(2).
#'
#' @export

#' @examples
#' data("simdata87")
#' X.sub <- impute.sub(X = simdata87$X.bdl, DL = simdata87$DL, verbose = TRUE)
#' apply(X.sub, 2, quantile, c(0, 0.02, 0.04, 0.09, 0.25))
#' # Compare against X.true
#' round(apply(simdata87$X.true, 2, quantile, c(0.01, 0.05, 0.09, 0.25, 0.5, 0.8, 1)), 5)
impute.sub <- function(X, DL, verbose = FALSE) {
    X.sub <- X   # Copy BDL Matrix
    C <- ncol(X)

    for (j in 1:C) {
      X.sub[, j] <- ifelse(is.na(X.sub)[, j], (DL / sqrt(2))[j], X.sub[, j])
    }

    if (verbose) {
      cat("Detection Limits/sqrt(2) \n")
      print(DL / sqrt(2))
      # Check: First three should be DL/sqrt(2); the last shouldbe differnt
      cat("Quantiles for X.sub \n")
      print(round(apply(X.sub, 2, quantile, c(0, 0.1, 0.25, 0.5, 0.8, 1)), 5))
     }

    return(X.sub)
}

# Easiest way --> but is there a faster way?? YEs: use purrr::map instead.
# Other THoughts how to impute faster
## Old Code is incorrect
## Impute
# X.sub <- l.data$X.bdl   # Copy BDL Matrix
# X.sub[is.na(X.sub)] <- l.data$DL/sqrt(2)  #Substitute
#

# #Toy Example
# X.sub <- matrix(c(NA, 1, 2, 1, NA, 3), nrow = 3, ncol = 2)
# DL <-  l.data$DL[ ,1:2]/sqrt(2)  #Substitute

# library(dplyr)

# #perhaps: replace_na()
# X.sub %>%
# select_if(function(x) any(is.na(x)))
# %>%
# mutate_at( replace = DL)
# # X.sub[ is.na(X.sub[ ,j]) ] <-
