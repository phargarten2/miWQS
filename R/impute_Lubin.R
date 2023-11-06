#' Lubin et al. 2004: Bootstrapping Imputation for One Chemical
#
#'
#' @family imputation
#' @keywords imputation
#'
#' @description
#'  Softly DEPRECATED. Use  impute.boot instead.
#'
#'  For one chemical, this function creates an imputed dataset using a bootstrap procedure as
#' described in Lubin et al. 2004.
#'
#' @param chemcol    A numeric vector, the chemical concentration levels of length C. Censored values are indicated by  NA. On original scale.
#' @param dlcol      The detection limit of the chemical. A value or a numeric vector of length C. Must be complete; a missing detection limit is ignored.
#' @inheritParams impute.multivariate.bayesian
#'
#' @return A list of: \describe{
#' \item{X.imputed}{A matrix with n subjects and K imputed datasets is returned.}
#' \item{bootstrap_index}{A n x K matrix of bootstrap indices selected for the imputation. Each column is saved as a factor.}
#' \item{indicator.miss}{A check; the sum of imputed  missing values above detection limit,
#'          which should be 0.}
#' }
#'
#' @examples
#   ###Example 2: Simulation
#'   # Apply to an example simulated dataset.
#'   # A seed of 202 is executed before each run for reproducibility.
#'   data(simdata87)
#'
#'   # No Covariates
#'   set.seed(202)
#'   results_Lubin <- impute.Lubin(chemcol = simdata87$X.bdl[, 1], dlcol = simdata87$DL[1],
#'                                 K = 5, verbose = TRUE)
#'   str(results_Lubin)
#'   summary(results_Lubin$imputed_values)
#'
#'   # 1 Covariate
#'   set.seed(202)
#'   sim.z1 <- impute.Lubin(simdata87$X.bdl[, 1], simdata87$DL[1],
#'                          K = 5, Z = simdata87$Z.sim[, 1], verbose = TRUE)
#'   summary(sim.z1$imputed_values)
#'
#'   # 2 Covariates
#'   set.seed(202)
#'   sim.z2 <- impute.Lubin(simdata87$X.bdl[, 1], simdata87$DL[1],
#'             K = 5, Z = simdata87$Z.sim[, -2])
#'   summary(sim.z2$imputed_values)
#'   summary(sim.z2$bootstrap_index)
#' @import survival
#' @import utils
#' @import stats
#' @export impute.Lubin

impute.Lubin <- function (chemcol, dlcol, Z = NULL, K = 5L, verbose = FALSE){
  is_Z_null <- is.null(Z)
  l <- check_function.Lub(chemcol, dlcol, Z, K, verbose)
  dlcol <- l$dlcol
  Z <- l$Z   #NULL if default - intercept-only model
  K <- l$K
  n <- length(chemcol)
  chemcol2 <- ifelse(is.na(chemcol), 1e-05, chemcol)
  event <- ifelse(is.na(chemcol), 3, 1)

  if(is_Z_null){
    Z <- matrix(rep(1, length(chemcol)), ncol = 1)
  }
    fullchem_data <- na.omit(
        data.frame(
          id = seq(1:n),
          chemcol2 = chemcol2,
          event = event,
          LB = rep(0, n),
          UB = rep(dlcol, n),
          Z = Z
        )
      )

  n <- nrow(fullchem_data)
  if (verbose) {
    cat("Dataset Used in Survival Model \n")
    print(head(fullchem_data))
  }
  #added error - 11/4/2023
  if(ncol(fullchem_data) <= 4) stop("Something is wrong with dataset used to bootstrap. Could a covariate be missing? Rerun function with verbose = TRUE.")

  bootstrap_data <- matrix(0, nrow = n, ncol = K, dimnames = list(NULL,
                                                                  paste0("Imp.", 1:K)))
  beta_not <- NA
  std <- rep(0, K)
  unif_lower <- rep(0, K)
  unif_upper <- rep(0, K)
  imputed_values <- matrix(0, nrow = n, ncol = K, dimnames = list(NULL,
                                                                  paste0("Imp.", 1:K)))
  for (a in 1:K) {
    bootstrap_data[, a] <- as.vector(sample(1:n, replace = TRUE))
    data_sample <- fullchem_data[bootstrap_data[, a], ]
    freqs <- as.data.frame(table(bootstrap_data[, a]))
    freqs_ids <- as.numeric(as.character(freqs[, 1]))
    my.weights <- freqs[, 2]

    final <- fullchem_data[freqs_ids, ]
    my.surv.object <- survival::Surv(
      time = final$chemcol2,
                                     time2 = final$UB,
      event = final$event,
      type = "interval"
      )
    if(is_Z_null){
      model <- survival::survreg(
        my.surv.object ~ . -1,   #surv includes duplicate of 1's (what's used for no covariates) so the intercept has to be removed.
        data = final[ , "Z", drop = FALSE], #equivalent to final[, -(1:5), drop = FALSE]
        weights = my.weights,
        dist = "lognormal",
        x = TRUE
      )
    } else if(!is_Z_null){
      model <- survival::survreg(
        my.surv.object ~ .,
        data = final[, -(1:5), drop = FALSE],
        weights = my.weights,
        dist = "lognormal",
        x = TRUE
      )
    }
    beta_not <- rbind(beta_not, model$coefficients)
    std[a] <- model$scale
    Z3 <- model$x

    mu <- Z3 %*% beta_not[a + 1, ]
    unif_lower <- plnorm(1e-05, mu, std[a])
    unif_upper <- plnorm(dlcol, mu, std[a])
    u <- runif(n, unif_lower, unif_upper)

    imputed <- qlnorm(u, mu, std[a])
    imputed_values[, a] <- ifelse(fullchem_data$chemcol2 ==
                                    1e-05, imputed, chemcol)
  }
  x.miss.index <- ifelse(chemcol == 0 | is.na(chemcol), TRUE,
                         FALSE)
  indicator.miss <- sum(imputed_values[x.miss.index, ] > dlcol)
  if (verbose) {
    # Check: ADDED MODEL, Z, BETA_NOT, STD. - 11/4/2023 TO BE PRINTED
    print(model)

    cat("Z: \n"); print(head(Z))
    cat("Beta: \n"); print(beta_not[a + 1, ])
    cat("Std Devs: (survival object) \n"); print (head(std[a]))


    beta_not <- beta_not[-1, , drop = FALSE]
    cat("\n ## MLE Estimates \n")
    A <- round(cbind(beta_not, std), digits = 4)
    colnames(A) <- c(names(model$coefficients), "stdev")
    print(A)
    cat("## Uniform Imputation Range \n")
    B <- rbind(format(range(unif_lower), digits = 3, scientific = TRUE),
               format(range(unif_upper), digits = 3, nsmall = 3))
    rownames(B) <- c("unif_lower", "unif_upper")
    print(B)
    cat("## Detection Limit:", unique(dlcol), "\n")
  }
  bootstrap_data <- apply(bootstrap_data, 2, as.factor)
  return(list(imputed_values = imputed_values, bootstrap_index = bootstrap_data,
              indicator.miss = indicator.miss))
}