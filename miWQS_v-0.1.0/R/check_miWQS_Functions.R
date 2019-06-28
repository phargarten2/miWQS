#' Checking miWQS functions
#'
#' @description
#' Functions used to check if arguments of miWQS functions are in the right form.
#' @noRd
#'

####################################################################################################

# Style: In compliance with tidyverse style guide, change all `stop()` functions to change the argument `call.` from TRUE to FALSE
stop <- function(..., call. = FALSE, domain = NULL) {
  stop(..., call. = call., domain = domain)
}

#' check_X: Desired class is a numeric matrix. X can also be dataframe. Errors if X is null or character. Called in Bayesian imputation, bootstrap imputation, and WQS
#' @noRd
check_X <- function(X) {
  if (is.null(X)) { stop("X is null. X must have some data.", call. = FALSE) }
  if (is.data.frame(X)) {  X <- as.matrix(X) }   # Can convert a numeric dataframe X into a matrix.
#  if( is.list(X) ) { stop("X cannot be a list. It must be a matrix or data-frame.")}
#  if( is.character(X) )  {  stop( "X is character." ) }
  if (!all (apply(X, 2, is.numeric))) { stop("X must be numeric. Some columns in X are not numeric.")}    # (mode)
  return(X)
}

# --------------------------------------------------------------------------------------------------
#' check_constants: K, B, T, n.burn all used this function
#' @param K The number of imputed datasets, a natural number.
#' @noRd
# Called in imputation model checks and also for B, number of Bootstraps.

check_constants <- function(K) {
  if (!is.numeric(K) | length(K) != 1) { stop("K must be numeric of length 1") }
  if (K <= 0) {  stop("K must be a positive natural number)") }
  if (!is.naturalnumber(K)) {
    warning("K is not a natural number. The next largest is taken.")
    K <- ceiling(K)
  }
  return(K)
}

# --------------------------------------------------------------------------------------------------
## check_covariates ()
# #'@description Checks if covariates Z have the right structure (i.e. a complete numeric matrix). Z may be an incomplete numeric matrix, factor or vector or dataframe. Z is converted to a complete numeric matrix.
#
# #'@inheritParams impute.boot
# #'@param imputation.model Logical. If TRUE, covariates are checked for imputation model. There is no y in imputation model, so the y argument is ignored. If FALSE, covariates are checked for WQS model
# #'@param y  The outcome, y, passed from WQS model.
#
# #'@ details This function is called upon Bayesian imputation functions and estimate.wqs() function.impute.Lubin() has different requirement for Z, so not called.
# Assumes that Z is on-NULL here. If Z is null, adjust in other check_functions.
# In the imputation functions,  X and y are passing arguments.
# Description of Z in impute.boot(): Any covariates used in imputing the chemical concentrations. If none, enter NULL. Ideally, a numeric matrix; but Z can be a vector or data frame. Assumed to be complete; observations with missing covariate variables are ignored in the imputation, with a warning printed.

check_covariates <- function(Z, X, imputation.model = TRUE, y = NULL) {
    # Z should be non-NULL when passed to check_covariates() because model.frame() doesn't accept NULL values. Adjust in other check_functions, as WQS and imputation models handle it differently.
    # stopifnot(is.null(Z))

    # if Z is a vector or dataframe, redefine Z as numeric matrix.
    # Note: model.matrix() expects Z to be complete and contain no missing data.
    if (!is.matrix(Z)) {
      if (is.vector(Z) | is.factor(Z)) {
        Z <- model.matrix(~., model.frame(~Z))[, -1, drop = FALSE]
      } else if (is.data.frame(Z)) {
        Z <- model.matrix(~., model.frame(~., data = Z))[, -1, drop = FALSE]
      } else { # if ( is.array(Z) | is.list(Z) ){
        (stop ("Please save Z as a vector, dataframe, or numeric matrix."))
      }
    }

    # X and Z should have same number of rows
    if (nrow(X) != nrow(Z)) {
      cat("> X has", nrow(X), "indviduals, but Z has", nrow(Z), "individuals")
      stop ("Can't Run. The total number of individuals with components X is different than total number of individuals with covariates Z.")
    }

    # Remove missing data from X (and y if neccessary), and save new Z, X, and y.
    if (anyNA(Z)) {
      warning ("Missing covariates (and outcome) are ignored.")
      # If covariate matrix Z is used without outcome (an imputation model), then y does not need to be included in dataframe.
      if (imputation.model) {
        df <- data.frame(Z, X)
        p <- if (is.vector(Z) | is.factor(Z)) { 1 } else { ncol(Z) }       # save columns of Z
        index <- complete.cases(df[, 1:p])  # save vector of where Z is complete
        Z <-  as.matrix(df[index, 1:p, drop = FALSE])   # save complete Z
        X <-  as.matrix(df[index, -(1:p), drop = FALSE])  # save X, excluding missing data from Z
        # Else if covarite matrix Z is used with an outcome (like WQS model), y needs to be included.
      } else {
        df <- data.frame(Z, y, X)
        p <- ncol(Z) + 1            # save columns of Z and y
        index <- complete.cases(df[, 1:p])  # save vector of where Z & y if complete
        Z <- as.matrix(df[index, 1:(p - 1), drop = FALSE])      # save complete Z, excluding any missing data from y
        y <- as.numeric(df[, p])                # save complete y, excluding any missing data from Z
        X <- as.matrix(df[index, -(1:p), drop = FALSE])   # save X, excluding missing data from Z and y
      }
    }

  # Return data
  return(list(Z = Z, X = X, y = y))
}






####################################################################################################
####################################################################################################
# #check_imputation: Check for proper execution of Bayesian imputation functions and bootstrap imputation function (impute.boot).
# #'@importFrom makeJournalTables is.naturalnumber
# Currently no check for impute.algorithm or verbose is included.
check_imputation <- function(X, DL, Z, T, n.burn, K, verbose = NULL) {
  ## X.  Desired class is a numeric matrix. X can also be a vector or data-frame. Errors if X is null or character.
  if (is.vector(X)) {
    warning("Only one chemical is imputed.")
    X  <- as.matrix(X)
  }
  X <- check_X(X)
  if (!anyNA(X)) { stop("Matrix X has nothing to impute. No need to impute.")}

  ## Check DL. Ideally a numeric vector with length = # of components.

  # Convert matrix and data-frame DL's into vectors
  if (is.matrix(DL) | is.data.frame(DL)) {
    # If DL is just saved as a 1-column or 1-row data-frame or matrix, convert to a vector. Otherwise, stop the function and force DL to be a vector.
    if (ncol(DL) == 1 | nrow(DL) == 1) {
      DL <-  as.numeric(DL)
    } else {
       stop("The detection limit must be a vector.")
      }
  } # end matrix/data.frame
  stopifnot(is.numeric(DL))



  # Remove any missing detection limits.
  if (anyNA(DL)) {
    no.DL <- which(is.na(DL))
    warning("The detection limit for ", names(no.DL), " is missing. Both the chemical and detection limit is removed in imputation.")
    # Remove missing detection limits
    DL <- DL [ -no.DL ]
    if (!(names(no.DL) %in% colnames(X))) {
      cat("  ##Missing DL does not match colnames(X) \n")
      }
    X <- X[, -no.DL]
  }

  # Check if each chemical has a detection limit.  The length of detection limit should equal number of chemicals
  if (length(DL) != ncol(X)) {
    cat("The following components \n");
    X[1:3, ]
    cat("has these detection limits \n");
    DL
    stop("Each component must have its own detection limit.")
    }

  ## Check Z: Create a model matrix.  Desired class is COMPLETE numeric matrix (with columns being continuous or 0/1 dummy variables for categorical)
    if (is.null(Z)) {
        # Make a matrix of 1's if no covariates.
        Z <- matrix(1, nrow = nrow(X), ncol = 1)
    } else {
      # Remove any Missing Covariates with warning. Uses helper check_covariates().
      l <- check_covariates(Z, X, imputation.model = TRUE)
      Z <- l$Z
      X <- l$X   # X matrix without
    }

  ## Checking n.burn and T (are natural number constants)
  if (!is.null(n.burn) & !is.null(T)) {
    stopifnot(is.wholenumber(n.burn))
    check_constants(T)
    if (!(n.burn < T)) {stop("Burn-in is too large; burn-in must be smaller than MCMC iterations T")}
  }

  ## K check a constant.
  K <- check_constants(K)

  ## Return items that changed:
  return(list(X = X, DL = DL, Z = Z, K = K))
}

####################################################################################################
####################################################################################################

## checks to make sure the arguments of impute.lubin() are specified correctly.
check_function.Lub <- function(chemcol, dlcol, Z,  K, verbose) {
  ## chemcol check: Should be a numeric vector.
  if (!anyNA(chemcol)) { stop("chemcol has nothing to impute. No need to bootstrap.")}
  X <- check_X (as.matrix(chemcol))
  # Note: check_X() checks if chemical X = chemcol has the proper form, but requires matrix or data-frame argument. However, chemcol is a vector so it is switch back. check_X() also used in Bayesian imputation & WQS models.

  ## dlcol check
  stopifnot(!is.null(dlcol) | is.vector(dlcol))
  stopifnot(is.numeric(dlcol))

  # If dlcol is a numeric vector, the dlcol is the smallest one. If the values are different, a warning is printed.
  if (length(dlcol) > 1) {
    # All values of dlcol are the same so just pick 1.
    if (min(dlcol, na.rm = TRUE) == max(dlcol, na.rm = TRUE)) {
      dlcol <- unique(dlcol)
    } else {
      # Values are different, so pick the smallest as detection limit.
      warning(" The detection limit is not unique, ranging from ",
              min(dlcol, na.rm = TRUE), " to ", max(dlcol, na.rm = TRUE),
              "; The smallest value is assumed to be detection limit",
              call. = FALSE, immediate. = FALSE)
      detcol <- !is.na(chemcol)
      # A summary
      cat("\n Summary when chemical is missing (BDL) \n")
      print(summary(dlcol[ detcol == 0]))
      cat("## when chemical is observed \n ")
      print(summary(dlcol[ detcol == 1]))
      # Assign Detection Limit:
      dlcol <- min(dlcol, na.rm = TRUE)           # if not, it is just the minimum of dlcol.
    }
  }

  # Cannot use missing dlcol
  if (is.na(dlcol)) {
    stop("The detection limit has missing values so chemical is not imputed.",
          immediate. = FALSE)
  }

  ## Z checks
  if (is.null(Z)) {     # if Z is null, make it an intercept-term.
    Z <- matrix(rep(1, length(chemcol)), ncol = 1)
  }
  if (anyNA(Z)) {
    warning ("Missing covariates are ignored.")
  }

  # if Z is a vector or dataframe, redefine Z as numeric matrix.
  # Note: model.matrix() expects Z to be complete and contain no missing data.
  if (!is.matrix(Z)) {
    if (is.vector(Z) | is.factor(Z)) {
      Z <- model.matrix(~., model.frame(~Z))[, -1, drop = FALSE]
    } else if (is.data.frame(Z)) {
      Z <- model.matrix(~., model.frame(~., data = Z))[, -1, drop = FALSE]
    } else { # if ( is.array(Z) | is.list(Z) ){
      stop ("Please save Z as a vector, dataframe, or numeric matrix.")
    }
  }

  # K check
  K <- check_constants(K)

  ## return any modifications of any parameters
  return(list(dlcol = dlcol, Z = Z, K = K))
}

####################################################################################################
####################################################################################################

# #' Makes sure data is in right format to run WQS.
check_wqs_function <- function(X, y, Z, proportion.train, n.quantiles, place.bdls.in.Q1, B,
                              b1.pos, signal.fn, family, offset, verbose) {
  ## Check X: Desired class is a numeric matrix. For anything else:
  if (is.vector(X)) {
    stop("Only one chemical is to be placed with an index. Don't run WQS, but a regular glm2.")
    }
  X <- check_X(X)

  ## Check y:  Desired format is a numeric vector. For anything else:
  if (!is.numeric(y)) {
    if (is.null(y)) {
      stop("y is NULL. y must have some data")
    } else if (is.factor(y)) {
      warning("Converting y into a number");
      y <- as.numeric(y) - 1
    } else {
      stop("y must be numeric or a factor.")
    }
  }
  # Missing outcomes are removed in check_covariates().
  if (length(y)  != nrow(X)) { stop("check dimensions of data", call. = FALSE) }

  ## Check Z: Create a model matrix.  Desired class is NULL or COMPLETE numeric matrix (with columns being continuous or 0/1 dummy variables for categorical)
  # Remove all missing values from y and Z with a warning. Uses miWQS helper check_covariates().
  ## Check Z: Create a model matrix.  Desired class is COMPLETE numeric matrix (with columns being continuous or 0/1 dummy variables for categorical)
  if (is.null(Z)) {
    # Make a matrix of 1's if no covariates.
    Z <- matrix(1, nrow = nrow(X), ncol = 1)
  } else {
    # Remove any Missing Covariates with warning. Uses helper check_covariates().
    l <- check_covariates(Z, X, imputation.model = FALSE, y)
    Z <- l$Z
    X <- l$X
    y <- l$y
  }

  ## Check proportion.train
  val = ifelse(is.numeric(proportion.train) & 0 < proportion.train & proportion.train <= 1, TRUE, FALSE)
  if (!val) { stop("Proportion of values to the training set must be numeric and between 0 and 1 (0<prop <=1).") }

  ## Check n.quantiles
  quant = ifelse(!is.null(n.quantiles),
                  ifelse(is.numeric (n.quantiles)  & length(n.quantiles) == 1 & n.quantiles > 1, TRUE, FALSE),
                  FALSE)
  if (!quant) {  stop("q must be numeric of length 1 and at least 1")}

  ## Check place.bdls.in.Q1
  if (class(place.bdls.in.Q1) != "logical")
    stop("place bdls in Q1 must be logical value of TRUE or FALSE")

  ## Check B
  B <- check_constants(B)

  ## Check b1.pos
  if (class(b1.pos) != "logical")
    stop("b1.pos must be logical value of TRUE or FALSE")

  ## Check family and offset
  if (class(family) != "character") { stop("family must be a character vector")   }
  if (family == "poisson") {
    a <- ifelse(is.null(offset),
                 warning("There is no offset specified. A count Poisson regression is performed"),
                 "")
  }
  # If offset is null, convert offset to a vector of 1's -- need to be split.
  if (is.null(offset)) { offset <- rep(1, nrow(X)) }

  ## Check Verbose
  if (class(verbose) != "logical") stop("verbose must be logical value of TRUE or FALSE")

  ## Return items that changed:
  return(list(X = X, y = y, Z = Z, B = B, offset = offset))
}


#########################################################
# find.examples <- function(){}

# impute.Lubin() examples
# Covariate Check
# if( !is.numeric(Z) ){ stop("Z must be a numeric") }
#  if( is.matrix(Z) | is.factor(Z) | is.data.frame(Z) ) {
#    p <- ncol(Z)
# } else{ if(is.factor(Z) | is.data.frame(Z) ) {
#    Z <- model.matrix( detcol ~ ., data = data.frame( detcol, Z)) [ , -1]
#   p <- ncol(Z)
# }  #
#  } else {
# }
## If detection limit is one number, make it a vector: pmh added
# dlcol <- if( length(dlcol) == 1) { rep(dlcol, length(chemcol)) }

#     if(anyNA(dlcol) ){
#        warning( "The detection limit has ", sum(is.na(dlcol)), " missing values, which will not be used.",
#                immediate. = FALSE)
#        dlcol <- na.omit(dlcol)
#     }

## detcol
#  stopifnot(  !is.null(detcol) | is.vector(detcol)   |  is.factor(detcol) )
# if(anyNA(detcol) ){
#    warning( "detcol has ", sum(is.na(detcol)), " missing values, which will not be used.",
#             immediate. = FALSE)
#   }
#   stopifnot( length(chemcol) == length(detcol) )


## Example: check_imputation()
# #'@example
# data(simdata87)
# l <- check_imputation( X =  simdata87$X.bdl[ , c(1,14)], DL = simdata87$DL[c(1, 14)],
#                         Z =  cbind( y = simdata87$y, simdata87$Z ), impute.algorithm = "gibbs",
#                         T = 100, n.burn = 5, K = 2)
#
#
 # In the main function type
 # check <- check_imputation(X, DL, Z, T, n.burn, K, verbose)
 # X <- check$X
 # DL <- check$DL
 # Z  <- check$Z
 # K <- check$K


# #To Debug
# X <- simdata87$X.bdl[ 1:100, c(1,14)]
# DL = simdata87$DL[c(1, 14)]
# Psi0 <- matrix(1, nrow = 8, ncol = c)
# Z <- cbind( y = simdata87$y, simdata87$Z )[1:100, ]
# impute.algorithm <- "gibbs"
# T <- 10
# n.burn <- 2
# K <- 2
# verbose <- TRUE
