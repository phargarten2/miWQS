
# miWQS 0.2.0
* Vignette posted.
* `impute.multivariate.regression()` to impute all chemicals jointly using a multivariate Bayesian regression.
* `estimate.wqs()` now has a formula capability. Can type in a full formula and specify column names instead of dividing up the data into three columns.

# miWQS 0.1.0
 * Changed the title of the package from "analysis" to "regression" to be more specific. Put the description in a README file. 
 * The `plot.wqs()` no longer automatically saves the plots to reduce clutter and save the user's workspace. However, you can still use `ggsave( )` later. If you depended on this behavior, you'll need to condition
  on `packageVersion("miWQS") > "0.0.0"`.
 * Created a README file 
 * Shortened description in DESCRIPTION file.
 * Made a vignette and submitted to Journal of Statistical Software for review.

## New Features
* These functions to be implemented when I launch the vignette 
   + New  `analyze.individually` function that does individual chemical analysis. 
   + New  `do.many.wqs()` function that does many WQS analyses, useful after doing a multiple imputation
   + New  `combine.AIC()` function to combine AIC results from many WQS analyses, similar in spirit to `pool.mi()`.
   + New `impute.boot()` function that performs bootstrapping imputation for many chemicals, not just one. 
   	- Adjusted `impute.Lubin()` to borrow from `impute.boot`.
	
## Minor Bug Fixes
* Clarified the documentation and added keywords.
* Changed most `cat()` used in the functions to messages `message()` so that the user can suppress messages using `suppressMessage()`.
* Consistent comments in all functions using notation "#>" 
 * The `estimate.wqs()` function
   +  Removed duplicate output in  by adding the `suppressMessages()` function. 
   +  the `boot` argument: clarified documentation in bootstrapping WQS. You now can do WQS regression without bootstrapping, but it is not recommended. 
   +  `offset` argument in estimate.wqs() -- offset argument is on the normal scale; the logarithm is not taken. However,  I found that the offset argument in `glm2()` and `glm()` has a default value of 0. The offset value in `estimate.wqs()` by default is a vector of 1's. When using `glm2`, offset argument now takes the logarithm as expected in all instances, (especially in `wqs.fit()`). User does not change the default of offset value.
   +  uses the `wqs.fit()` function edited instead of code in `estimate.wqs()`. 
   +  the place.bdls.in.Q1 argument now does something. You can set it to FALSE and regular quantiles are made. 
   + changed lower case "c" to upper case "C" to avoid conflict with "c()". 
 * The `estimate.wqs()` and `make.quantile.matrix()`: argument `place.bdls.in.Q1` is added to `make.quantile.matrix()` function. It is added a `NULL` default value. The default, NULL, automatically places any missing values in X placed into the first quantile. We suggest the user does not specify this argument unless the user wants to be specific in how missing values in components are handled. In version 0.0.9, this argument previously had no effect on how quantiles were made. This argument now has an effect. 
 * The `print.wqs()` function now concatenates, instead of prints, the convergence of the bootstrap samples.
* The `impute.bayesian._ ()` function:
*  `impute.univariate.bayesian.mi()` function:
	 + T argument:  changed default length of chain, T, to 1,000 for consistency with other functions. 
	 + fixed bug in initial values for the standard deviation in MCMC chain. Calculates standard deviation using the logarithm of `cov()` function. FROM:  The `complete.observations` argument was used for observed X. NOW:  standard deviations are calculated based on the substituted imputed chemical matrix, X, as covariances may not exist if X has many missing values. 
 	+ fixed bug so that the imputed values now draw from the last state, instead of the second-to-last state. 
	 + indicator.miss now is returned as a single number (a sum) rather than a vector. Make it consistent with new function `impute.multivariate.bayesian.mi()`.
	 + Reduced # of objects to be used in finding `initial2`.
	 + changed object name `x.miss.initial` to `log.x.miss.initial`. 
	 + Examples still remain the same.
 * `make.quantile.matrix()`:  Fixed an error if there are ties in the quantiles. An error occurred in `cut.default(...):  'breaks' are not unique`. We use the `.bincode()` function instead so that ties are handled if they arise. 
 * The `simdata87` data:
    + element `Z` renamed covariate names for clarity.
    + element `X.true, X.bdl, DL, delta, n0` : converted all chemical names to plain text for clarity. Removed p-p` ... and Greek letters for clarity in using R data frame. 


# miWQS 0.0.9
 *First Release of Package to the public.
 *For updates to CRAN team, see cran-comments. 
* Replaced examples using example dataset in package instead of using package wqs. Looks cleaner
* Remove printed output from estimate.wqs.
* Made documentation from estimate.wqs clearer. 
* Cleaned up print.wqs documentation


# miWQS 0.0.8
* Reworked plot.wqs() function by using ggplot2 instead of base plotting in R. 

# miWQS 0.0.7
* Fixed bug in doing Poisson Rate WQS regressions. Added argument offset to the check_function() and randomize.train()
* For updates to CRAN team, see cran-comments. 

# miWQS 0.0.0
* Added a `NEWS.md` file to track changes to the package.
* First Release of the Package to CRAN team 
* Successfully passed windows check. 
 
