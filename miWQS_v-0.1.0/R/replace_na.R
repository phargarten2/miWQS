# #'Replacing Missing Values
# Copy from Make Journal Tables
# #'@family statistics
# #'@keywords statistics
# #'
# #'@description: Replaces missing values in data with replace.

# #'@param data	 a data frame, matrix  or vector.
# #'@param replace	 If data is a data frame or matrix, a named list giving the value to replace NA with for each column.  If data is a vector, a single value used for replacement.

# #'@examples
# #' X <- matrix( c(78, NA, NA, 50, 36, NA, 143, NA, 172, 355), nrow = 5, ncol = 2)
# #' X
# #' #Example 1: 1 value replaced
# #' replace_na( X, list(3, 2) )   #The NA's in each column are replaced by 3 and 2.
# #'
# #' #Example 2: Many values replaced
# #' l <- list( rnorm(2, 50, 12), rnorm(2, 34,2) )
# #' l #replacemant values
# #' replace_na( X,  l)
# #'
# #' #Example 3: A vector
# #' replace_na( X[,1], 3)
# #'
# #' #Example 4: A factor
# #' f <- factor(c("a","b", NA))
# #' f
# #' replace_na(f , "a")
# #' replace_na(as.character(f), "a")

# #'@export

replace_na <- function(data, replace) {
  if (is.vector(data) | is.factor(data)) {
    data[is.na(data)] <- replace

  } else {         # Way to avoid a for loop???
    for (j in 1:ncol(data)) {
      data[ is.na(data[, j]), j] <- replace[[j]]
    }
  } # end class if statement

  return(data)
}
# save.image("~/VCU Biostatistics/Computer Programs/R_functions/replace_na.RData")
