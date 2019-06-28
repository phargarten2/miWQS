#' Bootstrapping Imputation for Many Chemicals
#'
#' @family imputation
#' @keywords imputation models
#'
#' @description
#' If many chemicals have values below the detection limit, this function creates an imputed dataset using a bootstrap procedure as described in Lubin et al. 2004. It repeatedly invokes \code{\link{impute.Lubin}}().
#'
#' @note
#' Note #1: Code was adapted from Erin E. Donahue's original translation of the SAS macro developed from the paper.
#'
#'       Note #2: No seed is set. Please set seed so the same bootstraps are selected.
#'
#'       Note #3: If the length of the DL parameter is greater than the number of components, the smallest value is assumed to be a detection limit. A warning is printed to screen.
#'
#        Note #4: For debugging, see impute.Lubin() and turn verbose = TRUE.
#'
#' @references
#' Lubin, J. H., Colt, J. S., Camann, D., Davis, S., Cerhan, J. R., Severson, R. K., … Hartge, P. (2004).
#' Epidemiologic Evaluation of Measurement Data in the Presence of Detection Limits. Environmental Health Perspectives,
#'  112(17), 1691–1696. https://doi.org/10.1289/ehp.7199

######## Parameters #################
#' @inheritParams impute.multivariate.bayesian
#' @return A list of: \describe{
#'      \item{X.imputed}{A number of subjects (n) x number of chemicals (c) x K array of imputed X values.}
#'      \item{bootstrap_index}{A n x K matrix of bootstrap indices selected for the imputation.}
#'      \item{indicator.miss}{A check; the sum of imputed  missing values above detection limit,
#'          which should be 0.}
#' }
#'
#' @examples
#' data("simdata87")
#' # Impute using one covariate.
#' l <- impute.boot(X = simdata87$X.bdl, DL = simdata87$DL, Z = simdata87$Z.sim[, 1], K = 2)
#' apply(l$X.imputed, 2:3, summary)
#' @export impute.boot


impute.boot <- function(X, DL, Z = NULL, K = 5L) {
  # ptm <- proc.time()    # Start the clock!(data)

  ### Check for proper execution: T & n.burn are not needed so it could be anything.
  check <- check_imputation(X, DL, Z, T = 5, n.burn = 4, K)
  X  <- check$X
  DL <- check$DL
  Z  <- check$Z
  K  <- check$K

  # Extract Parameters
  n <- nrow(X)
  c <- ncol(X)
  chemical.name <- colnames(X)

  # Run loop for each component in X
  results_Lubin <- array(dim = c(n, c, K), dimnames = list(NULL, chemical.name, paste0("Imp.", 1:K)))
  bootstrap_index <- array(dim = c(n, c, K), dimnames = list(NULL, chemical.name, paste0("Imp.", 1:K)))
  indicator.miss <- rep(NA, c)
  for (j in 1:c) {
    # set.seed(impute.seed)
    answer <-  impute.Lubin(chemcol = X[, j],  dlcol = DL[j],
                            Z = Z, K = K)
    results_Lubin[, j, ] <- as.array(answer$imputed_values, dim = c(n, j, K))

    # bootstrap_index[ ,j, ] <- array(answer$bootstrap_index, dim = c(n, 1, K) )
    indicator.miss[j] <- answer$indicator.miss
  }

  total.miss <- sum(indicator.miss)
  if (total.miss == 0) {
    message("#> Check: The total number of imputed values that are above the detection limit is ",
          sum(indicator.miss), ".")
  } else {
    print(indicator.miss)
    stop("#> Error: Incorrect imputation is performed; some imputed values are above the detection limit. \n",
         "#> Could some of `X` have complete data under a lower bound (`DL`)?",
         call. = FALSE)
  }



  # time.impute <- (proc.time() - ptm)[3]    #time!

  return(list(
    X.imputed = results_Lubin,
    bootstrap_index = answer$bootstrap_index,
    indicator.miss = total.miss
  )
  )
}
