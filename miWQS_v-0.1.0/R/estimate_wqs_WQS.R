#' Weighted Quantile Sum (WQS) Regression
#'
#' @name estimate.wqs
#' @family wqs
#' @keywords imputation wqs
#'
#' @description
#' Performs weighted quantile sum (WQS) regression model for continuous, binary, and count outcomes that was extended from \code{\link[wqs]{wqs.est}} (author: Czarnota) in the \pkg{wqs} package. By default, if there is any missing data, the missing data is assumed to be censored and placed in the first quantile.  Accessory functions (print, coefficient, plot) also accompany each WQS object.
#'
#' @details
#' The \cite{\link[Rsolnp]{solnp}} algorithm, or a nonlinear optimization technique using augmented Lagrange method, is used to estimate the weights in the training set. If the log likelihood evaluated at the current parameters is too large (NaN), the log likelihood is reset to be 1e24. We have discovered no issue with this reset. A data-frame with object name \emph{train.estimates} that summarizes statistics from the nonlinear regression is returned; it consists of these columns:
#' \describe{
#'   \item{beta1}{estimate using solnp}
#'   \item{beta1_glm, SE_beta1, test_stat, pvalue}{estimates of WQS parameter in model using glm2.}
#'   \item{convergence}{logical, if TRUE the solnp solver has converged. See \cite{\link[Rsolnp]{solnp}}.}
#'   \item{weight estimates}{estimates of weight for each bootstrap.}
#' }
#' This package uses the \cite{\link[glm2]{glm2}} function in the \pkg{glm2} package to fit the validation model.
#'
#' Signal functions allow the user to adjust what bootstraps are used in calculating the mean weight. Looking at a histogram of the overall mixture effect, which is an element after plotting a WQS object, may help you to choose a signal function. The \emph{signal.fn} argument allows the user to choose between four signal functions:
#'  \describe{
#'     \item{signal.none}{Uses all bootstrap-estimated weights in calculating average weight.}
#'     \item{signal.converge.only}{Uses the estimated weights for the bootstrap samples that converged.}
#'     \item{signal.abs}{Applies more weight to the absolute value of test statistic for beta1, the overall mixture effect.}
#'     \item{signal.test stat}{Applies more weight to the absolute value of test statistic for beta1, the overall mixture effect.}
#'     }
#'
#' Rates can be modelled using the offset. The \emph{offset} argument of \code{estimate.wqs()} function is on the normal scale, so please do not take a logarithm.  The objective function used to model the mean rate of the \emph{ith} individual \eqn{\lambda_i} with the offset is:
#' \deqn{ \lambda_i = offset * exp(\eta) }
#' , where \eqn{\eta} is the linear term of a regression.
#'
#' The object is a member of the" \emph{"wqs"} class; accessory functions include \code{coef}(), \code{print}(), and \code{plot}().

#' @note No seed is set in this function. Because bootstraps and splitting is random,  a seed should be set  before every use.
#'
#' @references
#' Carrico, C., Gennings, C., Wheeler, D. C., & Factor-Litvak, P. (2014). Characterization of Weighted Quantile Sum Regression for Highly Correlated Data in a Risk Analysis Setting. Journal of Agricultural, Biological, and Environmental Statistics, 20(1), 100–120. https://doi.org/10.1007/s13253-014-0180-3
#'
#' Czarnota, J., Gennings, C., Colt, J. S., De Roos, A. J., Cerhan, J. R., Severson, R. K., … Wheeler, D. C. (2015). Analysis of Environmental Chemical Mixtures and Non-Hodgkin Lymphoma Risk in the NCI-SEER NHL Study. Environmental Health Perspectives, 123(10), 965–970.  https://doi.org/10.1289/ehp.1408630
#'
#' Czarnota, J., Gennings, C., & Wheeler, D. C. (2015). Assessment of Weighted Quantile Sum Regression for Modeling Chemical Mixtures and Cancer Risk. Cancer Informatics, 14, 159–171. https://doi.org/10.4137/CIN.S17295
# APA format

########################################  Arguments and Returns  ########################################
#' @param y  Outcome: numeric vector or factor. Assumed to follow an exponential family distribution.
#' @param X  Components/chemicals to be combined into an index; a numeric matrix or data-frame.
#' @param Z  Any covariates used. Ideally, a numeric matrix, but Z can be a factor, vector or data-frame. Assumed to be complete; observations with missing covariate values are ignored with a warning printed. If none, enter NULL.
#' @param proportion.train  The proportion of data between 0 and 1 used to train the model. If proportion.train = 1L, all the data is used to both train and validate the model. Default: 1L.
#' @param n.quantiles An integer to specify the number of quantiles to be used categorizing the columns of X, e.g. in quartiles (q = 4), deciles (q = 10), or percentiles (q = 100). Default: 4L.
#' @param place.bdls.in.Q1  Logical; if TRUE or X has any missing values, missing values in X are placed in the first quantile of the weighted sum.  Otherwise, the data is complete (no missing data) and the data is split equally into quantiles.
# If place.bdls.in.Q1 is null, any missing values in X are automatically placed into the first quantile; otherwise, if complete X values are observed, it is false.  Default: NULL.
#' @param B  Number of bootstrap samples to be used in estimating the weights in the training dataset. In order to use WQS without bootstrapping, set B = 1. However, Carrico et al 2014 suggests that bootstrap some large number (like 100 or 1000) can increase component selection. In that spirit, we set the default to 100.
#' @param b1.pos Logical; TRUE if the mixture index is expected to be positively related to the outcome (the default). If mixture index is expected to be inversely related to the outcome, put FALSE.
#' @param signal.fn  A character value indicating which signal function is used in calculating the mean weight. See details.
#' @param family The distribution of outcome y. A character value:
#'      if equal to "gaussian" a linear model is implemented;
#'      if equal to "binomial" a logistic model is implemented;
#'      if equal to "poisson", a log-link (rate or count) model is implemented.
#'      See \code{\link[stats]{family}} in stats package. Passed to \pkg{glm2}. Default: "gaussian".
#' @param offset The at-risk population used as a numeric vector of length equal to the number of subjects when modeling rates in Poisson regression. Passed to \pkg{glm2}.  Default: If there is no offset, enter NULL.
#' @param verbose  Logical; if TRUE, prints more information. Useful to check for errors in the code. Default: FALSE.
#'
#' @return  \code{estimate.wqs} returns an object of class "wqs". A list with the following items: (** important) \describe{
#'   \item{call}{The function call, processed by \pkg{rlist}.}
#'   \item{C}{The number of chemicals in mixture, number of columns in X.}
#'   \item{n}{The sample size. }
#'   \item{train.index}{Vector, The numerical indices selected to form the training dataset. Useful to do side-by-side comparisons.}
#'   \item{q.train}{Matrix of quantiles used in training data. }
#'   \item{q.valid}{Matrix of quantiles used in validation data. }
#'   \item{train.comparison}{Data-frame that compares the training and validation datasets to validate equivalence }
#'   \item{initial}{Vector: Initial values used in WQS }
#'   \item{train.estimates}{Data-frame with rows = B. Summarizes statistics from nonlinear regression in training dataset. See details.}
#'  \item{processed.weights}{** A C x 2 matrix, mean bootstrapped weights (and their standard errors) after filtering using signal function. Used in calculating the WQS index. }
#'  \item{WQS}{Vector of the weighted quantile sum estimate based on the processed weights. }
#'  \item{fit}{** glm2 object of the WQS model fit to validation data. See \code{\link{glm2}{glm2}}.}
#'  \item{boot.index}{Matrix of bootstrap indices used in training dataset to estimate weights. Its dimension is the length of training dataset with number of columns = B.}
#' }

########################################  Examples  ########################################
#' @examples
#' # Example 1: Binary outcome using the example simulated dataset in this package.
#'  data(simdata87)
#'  set.seed(23456)
#'  W.bin4  <- estimate.wqs(
#'                   y = simdata87$y.scenario, X = simdata87$X.true[, 1:9],
#'                   B = 10, family = "binomial"
#'                   )
#'  W.bin4
#'
#' # Example 2: Continuous outcome. Use WQSdata example from wqs package.
#'  if (requireNamespace("wqs", quietly = TRUE)) {
#'   library(wqs)
#'   data(WQSdata)
#'   set.seed(23456)
#' #    W <- wqs::wqs.est(y = WQSdata$y, X = WQSdata[,1:9], B = 10)
#'   Wa <- estimate.wqs (y = WQSdata$y, X = WQSdata[, 1:9], B = 10)
#'   Wa
#'  } else {
#'   message("you need to install the package wqs for this example")
#'  }
#' ## More examples are found 02_WQS_Examples.
#' ## Also checked vs. Jenna's code, as well as thesis data, to verify results.
#'
########################################   Importing  & Export  ########################################
#' # TEMP #' @importFrom makeJournalTables  make.descriptive.table  my.summary
   # functions used in print commands.
#' @importFrom utils head
#' @importFrom Hmisc format.pval
#' @import Rsolnp
     # estimate the training set parameters (nonlinear regression) using solnp.
#' @import glm2
      # estimate the validation set parameters
#' @importFrom rlist list.merge
     # update call with defaults.
#' @export estimate.wqs

# There may be a warning from solnp: "NaN detected in function call...check your function". This means that "the log likelihood evaluated at the current parameters is too large (NaN); value is reset to be 1e24." Currently, we have discovered no issue with this reset.

#------------------------------------------------------------------------------------------------

estimate.wqs <- function(
    y,  X,  Z = NULL,
    proportion.train = 1L, n.quantiles = 4L,
    place.bdls.in.Q1 = if (anyNA(X)) TRUE else FALSE,
    B = 100L, b1.pos = TRUE,
    signal.fn = c("signal.none", "signal.converge.only", "signal.abs", "signal.test.stat"),
    family = c("gaussian", "binomial", "poisson"), offset = NULL,
    verbose = FALSE
    ) {

    # warning("The seed is not set; The data generated is random and not reproducible. To ensure reproducibility,
    #        set the seed before execution of this function")

    # Check (Totally re-editted)
    family <- tolower(family)
    family <- match.arg(family)
    signal.fn <- tolower(signal.fn)
    signal.fn <- match.arg(signal.fn)
    l <- check_function(X, y, Z, proportion.train, n.quantiles, place.bdls.in.Q1,
                        B, b1.pos, signal.fn, family, offset, verbose)
    X <- l$X
    y <- l$y
    Z <- l$Z
    n <- dim(X)[1]   # Sample Size
    C <- dim(X)[2]   # Number of chemicals
    # chemical.name <- colnames(X)
    offset <- if (is.null(offset)) rep(1, n) else offset

    # ------------------- Divide into training and valid sets (EDITED)
    split <- randomize.train(y, X, Z, offset, proportion.train)
    y.train <- split$y.train
    X.train <- split$X.train
    Z.train <- split$Z.train
    offset.train <- split$offset.train
    y.valid <- split$y.valid
    X.valid <- split$X.valid
    Z.valid <- split$Z.valid
    offset.valid <- split$offset.valid

    # Find train.index
    train.index = sort(split$train)

    # Divide X into quantiles
    if (verbose) { cat("## Place BDLs in Q1? ", place.bdls.in.Q1, "\n")  }
      q.train <- make.quantile.matrix(X.train, n.quantiles, place.bdls.in.Q1, verbose)
      q.valid <- suppressMessages(make.quantile.matrix(X.valid, n.quantiles, place.bdls.in.Q1, verbose))
    colnames(q.train) <- colnames(X)  # Transfer columnnames

    ### Comparison of train and Validation Datasets
    train.comparison <- if (0 < proportion.train & proportion.train < 1) {
      summarize.compare(train.index, y, Z, verbose = verbose)
    }   else { NA }

    #### ------ Obtaining Training Estimates (EDITED)
    # Specify bounds
    bounds <- specify.bounds(b1.pos, C)

    # Initial Values(Edited one line.)
    init <- specify.init(Z.train, y.train, b1.pos, C, family, offset.train, verbose)

#     cat("## Starting nonlinear optimization using solnp for", B, "bootstraps", "using n.train = ", nrow(q.train), "\n") #using augmented Lagrange method  ...
    l <- wqs_b.est(y.train, q.train, Z.train, B,
                   pars = init, objfn, lincon, ineqcon, ineqLB = bounds$ineqLB, ineqUB = bounds$ineqUB,
                   family, offset.train)

    # Save WQS and weights from bootstrap
    result <- l$out
   # print( summary(result[ ,1:6]) ) #should show 2 weights
    wts.matrix <- result[, -(1:6)]
    # print(head(wts.matrix))
    boot.index <- l$boot.index
   # print( head(wts.matrix) )

    # Edited: Add warning to user if the algorithm did not converge
    if (sum(result$convergence) < B & signal.fn != "signal.converge.only") {
      warning(paste(B - sum(result$convergence), "bootstrap regression estimates in the training sets have not converged. Proceed results with caution. \n"))
    }

    # Check Constraints
    if (b1.pos & min(result$beta_1) < -1e-8 | !b1.pos & max(result$beta_1) > 1e-8) { stop("Error in solnp. beta1 constaint is violated.")  }
    w.sum = apply(wts.matrix, 1, sum)
    d <- data.frame (w.min = min(wts.matrix),
                      w.max =  max(wts.matrix),
                      min(w.sum), max(w.sum)
    )
    if (verbose) {   cat("## Chk: Weight Constraints \n");  print(d) }

    #### ----- The weights are processed by signal functions.
    # cat("## Forming weighted average ... \n")
    # Define standard error function used.
    se <- function(X) { sqrt(var(X))}
    # which is equivalent to the following formula
    #  se.weight  <-  sqrt(       1/(B-1) * sum( ( A[ ,j] - w[j]/B ) ^ 2 ) )

    # No signal: uses all bootstrap-estimated weights in calculating WQS
    if (signal.fn == "signal.none") {
      w <- apply(wts.matrix, 2, mean)
      se.w <- apply(wts.matrix, 2, se)
    }

    # converged only: Only converged bootstraps are kept.
    if (signal.fn == "signal.converge.only") {
      A <- wts.matrix[result$convergence, ]
      w <- apply(A, 2, mean)
      se.w <- apply(A, 2, se)
    }

    # test statistic weight or signal.abs weighs the bootstraps by significance,   abs(test_stat_b) / sum(test_stat_b) .
    if (signal.fn == "signal.abs" | signal.fn == "signal.test.stat") {
      # if(anyNA(d$ts_weight)) { perform simple average}
      ts_weight <- abs(result$test_stat) / sum(abs(result$test_stat))
      A <- ts_weight * wts.matrix
      w <-   apply(A, 2, sum)
      se.w <- apply(A, 2, se)
    }

    # significant test stat: Only includes bootstrap samples where beta1 was significant from glm2.
    # Added Oct 19, 2018. Done in Jenna 2015 Enviornmental Chemical paper. Still need to be tested though.
    if (signal.fn == "signal.signifiance") {
      result$nb <- ifelse(result$pvalue_beta1 < 0.05, TRUE, FALSE)
      S <- wts.matrix[result$nb, ]
      w <- apply(S, 2, mean)
      se.w <- apply(S, 2, se)
    }

    #### -------- Validation Estimates
#    cat("## Estimating overall mixture effect using", nrow(q.valid), "subjects. \n")
     fit.val <- wqs.fit(q.valid, Z.valid, y.valid,  w, family, offset.valid)
     WQS <- fit.val$WQS
     fit <- fit.val$fit
    # WQS <- as.vector(q.valid %*% w)
    # if( is.null(Z.valid)){
    #   temp <- data.frame(y = y.valid, WQS)
    #   } else {
    #   temp <- data.frame(y = y.valid, Z.valid, WQS)
    # }
    # fit <- glm2::glm2(y ~ ., data = temp, family = family, offset = log(offset.valid))


    ### --------- Output to Return
    D <- formals(estimate.wqs)   # Make defaults of function as a list.
    A <- as.list(match.call())     # Make the call saved as a list.
    # I want a list of defaults, but the names are overriden by the actual functional call.
    U <- rlist::list.merge(D, A)
    # If the default for the character list is chosen, all will come up. By the match.args() function, the first one is used.
     # a <- as.list(U$signal.fn)
     # if( length(a) > 1) { U$signal.fn <- a[[2]] }

    # EDITED. Switched WQS and weights, added train
    out <- list(call = U, C = C, n = n,
                train.index = train.index,  q.train = q.train, q.valid = q.valid,
                train.comparison = train.comparison,
                initial = init,
                train.estimates = result,
                processed.weights = cbind(mean.weights = w, se.weights = se.w),
                #    convergence = result$convergence, #which bootstraps converged.
                WQS = WQS,
                fit = fit,
                boot.index = boot.index
                # beta1.glm = result$beta1.glm
                #    solnp.result = result
    )
    class(out) <- "wqs"
    return(out)
}



# \============================================================================================================================
### (III) Accessory Functions for WQS ====
# \============================================================================================================================

# #' Below you would find all the helper functions that is used in @title wqs_est.
# #' ```{r WQS.accessors}
# #' Makes sure data is in right format. (Is matrix or data frame )
check_function <- function(X, y, Z, proportion.train, n.quantiles, place.bdls.in.Q1, B,
                          b1.pos, signal.fn, family, offset, verbose) {
  ## Data
  # Remove all missing values from outcome and covariates.
  # Then, reassign Z. Desired class is NULL or a numeric matrix (with columns being continuous or 0/1 for dummies.)
  if (is.null(Z)) {
    df <- data.frame(y = y, X)
    if (verbose) {
      cat("## Outcome Summary \n");  print(summary(df$y));
    }
    if (anyNA(df$y)) {
      warning("All missing outcomes are ignored.")
      df <- df[complete.cases(df$y),   ]
    }
  } else {         # Z is not null.
    df <- data.frame(y = y, X, z2 = Z)
    cov_index <-  grep("z2", colnames(df))
    # if(verbose){
    #  cat("## Outcome & Covariate Summary \n")
    #  print( summary(df[ , c(1, cov_index)  ] ) )
    # }
    if (anyNA(df[, c(1, cov_index)  ])) {
      warning("All missing outcomes and/or covariates are ignored.")
      index <- complete.cases(df[, c(1, cov_index)  ])
      df <- df[ index,   ]
    }
    # Reassign Z.
    if (is.factor(Z) | is.data.frame(Z)) {
      Z <- model.matrix(y ~ ., data = df[, c(1, cov_index) ]) [, -1]
    } else {
      Z <- df[, cov_index, drop = FALSE]
      if (ncol(Z) == 0) {
          print(utils::head(df))
          print(cov_index)
          stop("No covariates found. Something wrong with cov_index")
      } # End possible error.
    } # End reassignment
    colnames(Z) <- gsub("z2.", "", colnames(Z)) # Revert to orginal name.
  } # End null if.

  # Replace X, y, and Z with data.frame. (Z is replaced above.)
  index <- 1 + 1:ncol(X)
  X <- df[, index]
  y <- df$y

  # Desired format of y is a numeric vector. For anything else:
  if (!is.numeric(y)) {
    if (is.null(y)) { stop("y is NULL. y must have some data") }
    if (is.factor(y)) {   warning("Converting y into a number"); y <- as.numeric(y) - 1 }
    else { stop("y must be numeric or a factor.") }
  }

  # Desired class for X is a numeric matrix. For anything else:
  {
    if (is.null(X)) { stop("X is null. X must have some data.") }
    if (is.character(X)) {  stop("X must be a numeric vector or matrix.") }
    if (class(X) == "numeric") {
      warning("There is only one chemical. No need to run WQS.")
      X  <- as.matrix(X)
    }
    if (is.data.frame(X) & sum(apply(X, 2, class) == "numeric")  == ncol(X)) {
      X  <- as.matrix(X)
    } # else { stop("X must be a numeric vector or matrix.")  }
  }

  if (verbose) {
  cat("## Classes: \n")
  classes <- c(class(y), class(X), class(Z)) ; names(classes) <-  c("y", "X", "Z")
  print(classes)
  }

  if (length(y)  != nrow(X)) { stop("check dimensions of data", call. = FALSE) }
  #    if(!is.null(Z) & nrow(X) != nrow(Z) ) { stop("check dimensions of data", call. = FALSE) }
  #   n0 <- apply(X, 2, function(X) { sum(is.na(X)) })

  # proportion.train
  val = ifelse(is.numeric(proportion.train) & 0 < proportion.train & proportion.train <= 1, TRUE, FALSE)
  if (!val) { stop("Proportion of values to the training set must be numeric and between 0 and 1 (0<prop <=1).") }

  # n.quantiles
  quant = ifelse(!is.null(n.quantiles),
                  ifelse(is.numeric (n.quantiles)  & length(n.quantiles) == 1 & n.quantiles > 1, TRUE, FALSE),
                  FALSE)
  if (!quant) {  stop("q must be numeric of length 1 and at least 1")}
  if (class(place.bdls.in.Q1) != "logical")
    stop("place bdls in Q1 must be logical value of TRUE or FALSE")
  if (!is.numeric(B) | length(B) != 1) # B < 2 ???
    stop("B must be numeric of length 1")
  if (class(b1.pos) != "logical")
    stop("b1.pos must be logical value of TRUE or FALSE")

  if (class(family) != "character") { stop("family must be a character vector")   }
  if (family == "poisson") {
    a <- ifelse(is.null(offset),
            warning("There is no offset specified. A count Poisson regression is performed"),
            "")
  }

  if (class(verbose) != "logical")
    stop("verbose must be logical value of TRUE or FALSE")

  return(list(X = X, y = y, Z = Z))
}

### randomize.train: NEW FUNCTION I ADDED from estimate.wqs.
# #'@description: This function randomly split and create training and validation sets. Note: This function should always set the seed before use.
# #'@param Y: Outcome; @param X: Chemicals; @param: Z: Covariates. Default: NONE. Passed from orginal function
# #'@param proportion.train: the proportion of data to the test dataset. Default: 1--This means that the train and validation dataset are the same.
# #'@keywords randomize
# #'@export  A list that consists of the split vector of y and split matrices of X and Z by proportion.train p and
# #'         the numerical indices of the train dataset (included as item #7 in list)
# #'@examples
# #' set<-randomize.train(WQSdata$y,X,proportion.train=0.4) #,seed=506079)[[7]]
# #' #set[[7]] matches exactly with code above.

randomize.train <- function(y, X, Z, offset, proportion.train) {
  # print( head(Z))
  # print( class(Z))

  # Randomly split and create training and validation sets
  train <- sample(nrow(X), round(proportion.train * nrow(X), 0), replace = FALSE)

  # Create training sets
  y.train = y[train]
  X.train = X[train, ]
  Z.train = Z[train, , drop = FALSE]
  offset.train = offset[train]

  # Create validation datasets
  if (0 < proportion.train & proportion.train < 1) {
    y.valid = y[-train]
    X.valid = X[-train, ]
    Z.valid = Z[-train, , drop = FALSE]
    offset.valid = offset[-train]
  } else { # if(proportion.train==1){  #if no proportion is given the sets are same.
    y.valid = y.train
    X.valid = X.train
    Z.valid = Z.train
    offset.valid = offset.train
  }

  return(list(y.train = y.train,
              X.train = X.train,
              Z.train = Z.train,
              offset.train = offset.train,
              y.valid = y.valid,
              X.valid = X.valid,
              Z.valid = Z.valid,
              offset.valid = offset.valid,
              train = train)
  )
}


### Comparsion of Test and Validation Datasets
# Helper function that compares test and validation datasets that I get from WQS. Executed in main function but printed in summary.wqs
# #'@param train.index the training index output from a wqs.est object
# #'@param y,X,Z:   the X,y, and Z used as arguments for wqs.est.
# #'@param verbose: logical. If TRUE, prints out the test results

summarize.compare <- function(train.index, y,  Z, verbose = TRUE) {
  # combine all into one dataset
  data.tmp <- if (is.null(Z)) {  data.frame(y)   } else {    data.frame(y, Z) }

  # Determine which column of data.tmp is numeric or categorical.If there are no numeric covariates,
  # set covariate.num to be NULL. If there are no categorical covariates, set to NULL.
  classes <- sapply(data.tmp,  class)
  covariate.num <- data.tmp[, classes == "numeric", drop = FALSE]
  covariate.fac <- data.tmp[, classes != "numeric", drop = FALSE]

  # Create a trained variable say "trained"
  data.tmp$trained <- as.factor(
    ifelse(1:nrow(data.tmp) %in% train.index, "trained", "validate")
    )

  # Make a descriptive table
  # compare <- make.descriptive.table(covariate.num, covariate.fac, treatment = data.tmp$trained)
  compare <- "TBD"

  return(compare)
}



# #' @section nonlinear estimation: wqs_b.est
# wqs_b.est estimates the parameters in the training data stage.Most of the arguments in wqs_b.est are passed to Rsolnp :: solnp.
# Edited. Added default parameters from what is used in main function.
# Edited. CHanged to number for pvalue and test stat--so it works with generic family. (ie t-test static is used in glm)
# Edited. Added line to warn user if algorithm does not converge
# Edited. Used Solnp's output to find the test statistic. Removed the glm2 fit from the function.
# Edited: Changed name from objfn.cont to objfn.
# Removed beta1 from loop and added at end. Removed pvalue from being calculated and/or saved. 04/04/18

# #'@description: The objective function for nonlinear regression uses oLS estimation.
# #'@param param : current parameter estimates
# #'@param q, Z, y: the data: q for the quantiles of X (usually q.train); Z for covariates (passed from main function); y for outcome (passed from main function)  )
objfn <- function(param, q, Z, y, family, offset) {
  C <- dim(q)[2]
  b0 <- param[1]
  b1 <- param[2]
  w <- param[3:(2 + C)]
  if (is.null(Z)) {
    eta <- b0 + b1 * q %*% w
  } else {
    p <- if (class(Z) == "numeric") { 1 } else { dim(Z)[2]  }  # Edited.
    theta <- param[(3 + C):(2 + C + p)]
    eta <- b0 + b1 * q %*% w + Z %*% theta
  }

  ## Edited. The least sq is only for the guassian link.
  if (family == "gaussian") {
    leastsq <- sum((y - eta)^2)
  }
  else if (family == "binomial") {
    # Probability of success: If eta becomes large, have issue with storing data. As eta ->infty, p ->0
    pi <- 1 / (1 + exp(-eta)) # = 1/2 + 1/2 *tanh( eta/2 ) #better: use plogis()
          # exp is better in test thn tanh, although tanh runs 3x faster and can use larger numbers than exp.
          # thought inverse logit was: exp(eta)/(1+exp(eta)) = 1/4 * sech( eta/2) ^2
    loglik <- sum(y * log(pi) + (1 - y) * log(1 - pi))     # log liklihood of a bernoulli rv
    leastsq <-  -loglik   # Rsolnp minimizes, but minimizing - log liklihood is identical to maximizing it.
  }
  else if (family == "poisson") {  # unusual in chemical mixture problems.  -- Used in nonlinear estimation.
    lambdai <- offset * exp(eta)
    loglik <- sum(y * log(lambdai) - lambdai)   # log likelihood of poisson.
    leastsq <- -loglik
  }

  return(leastsq)
}

# The linear constraint for WQS is that the weights must sum to 1.
lincon <- function(param, q, Z, y, family, offset) {
    C <- dim(q)[2]
  weights <- param[3:(2 + C)]
  return(sum(weights))
}

# The inequality constraint for WQS. b1>b1.pos and weights are between 0 and 1.
ineqcon <- function(param, q, Z, y, family, offset) {
  C <- dim(q)[2]
  b1 <- param[2]
  weights <- param[3:(2 + C)]
  return(c(b1, weights))
}

# Functions for nonlinear Constraint
# 8-10-17: Modified. Change the boundary of Beta1 from 0 to 1E-6.
specify.bounds <- function(b1.pos, C) {

  # Inequality constraints for Beta1.
  if (b1.pos) {
    b1.LB <- 0  # 1E-6   #EDITED.
    b1.UB <- Inf
  }
  else {
    b1.LB <- -Inf
    b1.UB <- 0 #-1E-6  # #EDITED.
  }

  # Constraints for each of the weights in X
  ineqLB <- c(b1.LB, rep(0, C))
  ineqUB <- c(b1.UB, rep(1, C))

  out <- list(ineqLB, ineqUB)
  names(out) <- c("ineqLB", "ineqUB")
  return(out)
}

# Uses glm2 to estimate Training Set Parameters
  # Default for pars is  = init
  # default for ineqLB = bounds$ineqLB, ineqUB = bounds$ineqUB,
wqs_b.est <- function(y.train, q.train, Z.train, B, pars, objfn, lincon, ineqcon, ineqLB,
                      ineqUB, family, offset) {
  # Number of chemicals C
  C <- dim(q.train)[2]

  # Add the number of covariates--whether Z is null or not
  Z.null <- ifelse(is.null(Z.train), 1, 0)
  p <- if (Z.null == 0) { dim(Z.train)[2] } else { 0 }

  # The loop
  result <- matrix(0, nrow = B, ncol = length(pars));  colnames(result) <- names(pars)
  convergence <- vector(mode = "logical", B)
  boot.index <- matrix(0, nrow = length(y.train), ncol = B)
  beta1_glm <- rep(0, B)
  SE_beta1 <- rep(0, B)
  test_stat <- rep(0, B)
  pvalue <- rep(0, B)
  fit <- vector(mode = "list", B)
  for (b in 1:B) {
    # Bootstrap
    boot.index[, b] <- samp <- sample(1:length(y.train), replace = TRUE)  # Edited. Save boot.index.
    y.b <- as.vector(y.train[samp])
    q.b <- q.train[samp, ]
    if (Z.null == 0) {
      Z.b <- as.matrix(Z.train[samp, ])
      rownames(Z.b) <- NULL
    } else {
      Z.b <- NULL
    }

    # Nonlinear regression estimates the constrained weight
    result.b <- suppressWarnings(
      Rsolnp::solnp(pars, fun = objfn,
                      eqfun = lincon, eqB = 1, ineqfun = ineqcon, ineqLB,
                      ineqUB, LB = NULL, UB = NULL, q.b, Z.b, y.b, family, offset,
                      control = list(tol = 1e-10, delta = 1e-10, trace = 0)
                    )
    )
    # note: trace shows values of function and parameters
    #  Edited. Added suppressWarnings because Warnings from solnp does not make any sense to  the average R user. Two warnings come up
    # (1) In p0 * vscale[(neq + 2):(nc + np + 1)] : longer object length is not a multiple of shorter object length. However, I added a warning that rephases solnp's warning into more user-friendly format. A minor change.
    # (2) There may be a warning from solnp: "NaN detected in function call...check your function". This means that "the log liklihood evaluated at the current parameters is too large (NaN); value is reset to be 1e24." Currently, we have discovered no issue with this reset.

    result[b, ] <- result.b$pars
    convergence[b] <- ifelse (result.b$convergence == 0, TRUE, FALSE)    # Edited. Made convergence logical instead of numeric.
    # The regression estimation converged (if 0) ; did not otherwise.

    # The Hessian provided by solnp is "the augmented problem at the optimal solution." The optimal soln are the parameter estimates that minimize the negative logliklihood.  The Hessian of miniminizng a negative log likilhood is the observed Fisher's information matrix. By inverting the Hessian, the covariance estimate for the parameters can be found (except when it is singular). The  first rows/columns are from parameters while remainders are Lagrange parameters estimating the constraints.
      # Hessian is postive semi-definite, so all its eigen-values >=0.

    # glm to obtain test statistic for beta1
    w <- result.b$pars[3:(2 + C)]
    fit <- wqs.fit(q.b, Z.b, y.b, w, family, offset)$fit
    # WQS <- as.vector( q.b %*% w)
    # tmp <- if(is.null(Z.b)) { data.frame(y.b, WQS) } else {  data.frame(y.b, Z.b, WQS)  }
    # fit <- glm2::glm2(y.b ~ ., data = tmp, family = family, offset = log(offset))
    # #print( summary(fit) )
    beta1_glm[b] <- summary(fit)$coefficients["WQS", 1]
    SE_beta1[b] <- summary(fit)$coefficients["WQS", 2]
    test_stat[b] <-  summary(fit)$coefficients["WQS", 3]
    pvalue[b] <-  summary(fit)$coefficients["WQS", 4]
    fit[[b]] <- fit
    # glm.summary <- summary(fit)$coefficients
  }

  # Weight Estimates
  wts.matrix <- as.matrix(result[, grep("w", colnames(result))])  # Weights
  colnames(wts.matrix) <-  if (!is.null(colnames(q.train))) {    colnames(q.train) } # else { paste0("w", 1:C) } #, which they are already by default. }
  # Edited: Change column names of wts.matrix to chemical names (found in colnames(q.train)), to be more definitivebut only if they exist.

  # Return
  out <- data.frame(beta_1 = result[, "b1"], beta1_glm = beta1_glm, SE_beta1,
                     pvalue_beta1 = pvalue, test_stat, convergence,
                     wts.matrix)
  return(list(out = out, boot.index = boot.index, fit = fit))
}


# Signal Function
##  #'@description A signal function that uses the test statistics of b1 estimated from glm2 but ignores the bootstraps where there is no mixture effect,
##  as determined by b1 estimate from solnp <0.0001. The processed weights then are only weighted by test statistics where there is a mixture effect.
##  Expect the signal to be higher than signal_teststat or signal_abs functions.
##  #'@param result: is an object of training estimates from wqs_solnp.
##  #'@param B: The number of bootstraps, a parameter in estimate.wqs
##  #'@return w: the processed weights.
signal_tsglmno0 <- function(result, B) {
  cat("The test statistics from", sum(result$beta_1 < 0.0001),  "out of the", B, "bootstraps are ignored in averaging the weights,
      as these have a b1 estimate of 0 (technically < 0.0001) from solnp in the training dataset")
  wts.matrix <- result[, -(1:6)]   # The chemicals
  Sb <- ifelse(result$beta_1 < 0.0001, 0, result$test_stat)
  signal <- Sb / sum(Sb)
  A <- signal * wts.matrix
  w <-   apply(A, 2, sum)
  return(w)
}


### Validation Test Estimates
# #' @wqs.fit
# #' @description Accessory: Fits WQS in regression model using \@title{"glm2"}. Used in both training and validation sets.
# #' @param q is the quantile (or chemicals)
# #' @param Z covariates (passed as argument from wqs.est)
# #' @param y outcome (passed as argument from wqs.est)
# #' @param w weights
# #' @param family distribution of y. See glm for description. Passed from wqs.est.
# #' @return WQS : Matrix used in glm2
# #' @return fit : glm2 summary
#
# #log offset is not used in glm2
# #' @examples
# #' data(simdata87)
# #' q <- make.quantile.matrix( simdata87$X.true[ ,1:2] , n.quantiles = 4 )
# #' wqs.fit( q, NULL, y = simdata87$y.scenario, w = rep(0.5, 2), family = "gaussian", offset = rep(1, 1000))
# #' wqs.fit( q, NULL, y = simdata87$y.scenario, w = rep(0.5, 2), family = "binomial", offset = rep(1, 1000))

wqs.fit <- function(q, Z, y, w, family, offset) {
  WQS <- as.vector(q %*% w)
  Z.null <- ifelse(is.null(Z), 1, 0)
  if (Z.null == 0) {
    temp <- data.frame(y, Z, WQS)
  }
  else {
    temp <- data.frame(y, WQS)
  }
  fit <- glm2::glm2(y ~ ., data = temp, family = family, offset = log(offset))
  # print(fit$family)
  if (!fit$converged) {
    stop("The Validation WQS Model did not converge.", call. = FALSE)
    }

  out <- list(WQS, fit)
  names(out) <- c("WQS", "fit")
  return(out)
}



###   \============================================================================================================================
###   At last: Save Function =====
###   \============================================================================================================================

# rm(WQSdata, z)
# save.image("~/VCU Biostatistics/as work/miWQS/WQS_Functions.RData")

# Log: Sep 17, 2018
# Added "offset" to check_function
# Removed duplicated code in wqs_b.est
# Double checked all code.
