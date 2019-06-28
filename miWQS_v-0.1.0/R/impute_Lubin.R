#' Lubin et al. 2004: Bootstrapping Imputation for One Chemical
#'
#' @family imputation
#' @keywords imputation
#'
#' @description
#'  For one chemical, this function creates an imputed dataset using a bootstrap procedure as
#' described in Lubin et al. 2004.
#'
#' @param chemcol    A numeric vector, the chemical concentration levels of length C. Censored values are indicated by  NA. On original scale.
#' @param dlcol      The detection limit of the chemical. A value or a numeric vector of length C. Must be complete; a missing detection limit is ignored.
#' @inheritParams impute.multivariate.bayesian
#'
#' @return A list of: \describe{
#' \item{X.imputed}{A matrix with n subjects and K imputed datasets is returned.}
#' \item{bootstrap_index}{A n x K matrix of bootstrap indices selected for the imputation.}
#' \item{indicator.miss}{A check; the sum of imputed  missing values above detection limit,
#'          which should be 0.}
#' }
#'
#' @details
#'   Lubin et al. (2004) evaluate several imputation approaches and show that a multiple imputation procedure
#'   using bootstrapping creates unbiased estimates and nominal confidence intervals unless the proportion of
#'   missing data is extreme. The authors coded the multiple imputation procedure in a SAS macro that
#'   is currently available. We converted the SAS macro into R code.
#'
#'   A single chemical with missing values is imputed. The distribution for the interval-censored data \emph{chemcol} is assumed to be   lognormal and censored between 0 and \emph{DL}. After bootstrapping, the values BDL are imputed  using the inverse transform method. In other words, generate \eqn{u_i \sim Unif( 0.0001, dlcol)} and assign value \eqn{F^{-1}(u)} to \eqn{x_{i}} for \eqn{i = 1,...n_{0}} subjects with chemical values BDL.
#'
#' \code{impute.Lubin} performs the following:
#' \enumerate{
#'   \item Input arguments
#'   \item Obtain bootstrap samples.
#'   \item Generate weights vector.
#'   \item Use \code{\link[survival]{Surv}} function from Survival package to obtain survival object.
#'   \item Use \code{\link[survival]{survreg}} function from Survival package to obtain survival model.
#'   \item Sample from lognormal distribution with beta and variance from survival model as the parameters
#'         to obtain upper and lower bounds.
#'   \item Randomly generate value from uniform distribution between the previously obtained upper
#'         and lower bounds.
#'   \item Sample from the lognormal distribution to obtain the imputed data value associated with
#'         the above uniform value.
#'   \item Repeat for all observations. }
#'
#'
#' @examples
#' #  Using abind::abind --possibly to impute multiple chemicals
#' #   ###Example 2: Simulation
#'   # Apply to an example simulated dataset.
#'   # A seed of 202 is executed before each run for reproducibility.
#'   data(simdata87)
#'   # Impute: 1 chemical
#'   # No Covariates
#'   set.seed(202)
#'   results_Lubin <- impute.Lubin(chemcol = simdata87$X.bdl[, 1], dlcol = simdata87$DL[1], K = 5)
#'   str(results_Lubin)
#'   summary(results_Lubin$imputed_values)
#'
#'   # 1 Covariate
#'   set.seed(202)
#'   sim.z1 <- impute.Lubin(simdata87$X.bdl[, 1], simdata87$DL[1],
#'                          K = 5, Z = simdata87$Z.sim[, 1])
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

impute.Lubin <- function(chemcol, dlcol,  Z = NULL, K = 5L, verbose = FALSE) {
    # require(survival)
    # set.seed(seed)

    ## Checks and Verifications --> Any modifications are returned: pmh, added
       l <- check_function.Lub(chemcol, dlcol, Z, K, verbose)
       dlcol <- l$dlcol
       Z <- l$Z
       K <- l$K
       #   if(verbose){
       #      cat("## First few covariates ... \n")
       #      print( head(Z) )
       #  }

    ## creating dataset to be used in survival model.
      n <- length(chemcol)  # pmh, added since I removed it as parameter.
      # chemcol2: The observed concentration or the lower bound of the interval (i.e. 0.0001. The "time"; see survival::Surv.
      chemcol2 = ifelse(is.na(chemcol), 0.00001, chemcol)
      # event:  Status indicator; 1=event at time, 3=interval censored. See Survival::surv.
      event =  ifelse(is.na(chemcol), 3, 1)      # equivalent to ifelse(chemcol2 == 0.0001, )
      fullchem_data <-
        na.omit(
          data.frame(
            id = seq(1:n),
            chemcol2 = chemcol2,
            event = event,
            LB = rep(0, n),
            UB = rep(dlcol, n),
            Z
          )
        )
      n <- nrow(fullchem_data)  # redefine after removing missing values.
      if (verbose) { print(head(fullchem_data)) }

    ## For loop to obtain bootstrap samples, weights vector, parameter estimates, and imputed values
    ## obtain parameter estimates for beta and sigma squared based on bootstrap sample using survreg.
    ## Impute analyte values based on sampling from lognormal(Beta, sigmasq)

    ## creating the empty matrices and vectors to be used in the for loop
      bootstrap_data <- matrix(0, nrow = n, ncol = K, dimnames = list(NULL, paste0("Imp.", 1:K)))
      # data_sample<-matrix(0,nrow = n, ncol = K)
      #  freqs<-matrix(0,nrow = n, ncol = 10)
      beta_not <- NA
      std <- rep(0, K)
      unif_lower <- rep(0, K)
      unif_upper <- rep(0, K)
      imputed_values <- matrix(0, nrow = n, ncol = K, dimnames = list(NULL, paste0("Imp.", 1:K)))

    for (a in 1:K) {
       ## bootstrap
         # generate bootstrap samples by sampling row ID with replacement (indences )
         bootstrap_data[, a] <- as.vector(sample(1:n, replace = T))
         # select the data values to the bootstrap sample
         data_sample <- fullchem_data[bootstrap_data[, a], ]

      ## obtain weights
        # Number of times each observation selected
        freqs <- as.data.frame(table(bootstrap_data[, a]))
        # A vector of unique row IDs that are included in the sample
        freqs_ids <- as.numeric(as.character(freqs[, 1]))
        # create weights vector
        my.weights <- freqs[, 2]

      ## survival analysis
        # ph-edited (removed my.weights) create dataset with observed rows and their weights
        final <-  fullchem_data[freqs_ids, ]
        # creates response variable for the survreg function, includes z-values that are observed and intervals for those that are BDL
        my.surv.object <- survival::Surv(time = final$chemcol2, time2 = final$UB,
                                          event = final$event, type = "interval")
        # pmh: fits parameteric survival regression model (includes covariates)
        model <- survival::survreg(my.surv.object ~ .,
                                   data = final[, -(1:5), drop = FALSE],
                                   weights = my.weights, dist = "lognormal",
                                   x = TRUE)
        beta_not  <- rbind(beta_not, model$coefficients)    # store the betas of each bootstrap sample
        std[a] <- model$scale                                # store the std devs of each bootstrap sample
        Z <- model$x                                        # rename Z to be the model matrix.

        ## Step 2 -- Impute the analyte valutes (pmh edited)
        mu <- Z %*% beta_not[a + 1, ]
        unif_lower <-  plnorm(0.00001, mu, std[a])   # n x 1 vector
        unif_upper <-  plnorm(dlcol,      mu, std[a])   # n x 1 vector
        u <- runif(n,  unif_lower, unif_upper)
        imputed <-  qlnorm(u, mu, std[a])          # impute
        # Fully observed data: Replace the missing values with imputed; observed values should be same.
        imputed_values[, a] <- ifelse(fullchem_data$chemcol2 == 0.00001,  # if missing
                                        imputed,        # replace BDLs with imputed values
                                        chemcol         # observed values.
        )

    }
     ## Check: Sum of values imputed that are above detection limit (upper limit; should be 0)
      x.miss.index <- ifelse(chemcol == 0 | is.na(chemcol), TRUE, FALSE)
      indicator.miss <- sum(imputed_values[x.miss.index,  ] > dlcol)

      if (verbose) {        # ph added, useful for debugging and checking.
        # Check: print(model)
        beta_not <- beta_not[-1, , drop = FALSE]  # remove missing row
        cat("\n ## MLE Estimates \n")
        A <- format(cbind(beta_not, std), digits = 4, nsmall = 2)
        colnames(A) <- c(names(model$coefficients), "stdev")
        print(A)
        cat("## Uniform Imputation Range \n")
        B <- rbind(format(range(unif_lower),  digits = 3, scientific = TRUE),
                    format(range(unif_upper), digits = 3, nsmall = 3)
                    )
        rownames(B) <- c("unif_lower", "unif_upper")
        print(B)
        cat("## Detection Limit:", unique(dlcol), "\n")
      }

  return(list(
    imputed_values = imputed_values,  # the imputed values.
      # possibly return also:
    bootstrap_index = bootstrap_data,   # list of bootstrap indices
    indicator.miss = indicator.miss  # Check: Sum of imputed  missing values above detection limit (should be 0):
   )
  )
}


### Save image
# ```{r save_stuff }
# save.image("~/VCU Biostatistics/as work/Literature Papers/04_Lubin Detection Limit/impute_Lubin.RData")
