#  In makeJournalTables: Don't export.
#   #   #'Formatting Frequencies and Percentages
#   #'
#   #'@family format
#   #'@keywords format
#   #'
#   #'@description Formats frequencies and percentages as Freq(Percent\%) like  6.00 (18.02\%).
#   #'
#   #'@details The columns of x can come from \code{\link[base]{table}} and
#   #'           \code{\link[base]{prop.table}}. The additional arguments format the
#   #'           frequencies and percentages.
#   #'@param n The frequency vector
#   #'@param ColPct The percentage vector
#   #'@inheritParams base::format
#   #'@param ... Additional formatting parameters passed to \code{\link[base]{format}}.
#   #'@return    A matrix consisting of formatted frequency/percentage
#   #'
#   #'@examples
#   #'\dontrun{
#   #'   #Example 1: A Toy Data Frame
#   #'   formatZ ( c( 45, 17, 14), ColPct = c (78.9, 63.0, 46.7 ), digits = 3, nsmall = 1)
#   #'
#   #'   #Example 2: Using table and prop.table functions.
#   #'   data(CO2)
#   #'   t <- table(CO2$Type, CO2$Treatment)
#   #'   t
#   #'   colPct <- 100 * prop.table(t, 2)
#   #'   #Need to convert t into a vector to apply function; can use as.data.frame(), like
#   #'   as.data.frame(t)
#   #'   cbind( as.data.frame(t)[ ,1:2], formatZ ( as.data.frame(t)[ ,3], as.data.frame(colPct)[ ,3]) )
#   #'   #which can be easily copied and pasted into any professional table, like to a colleague or a
#   #'   #peer-reviewed journal.
#   #' }
#   #'
#   #' In makeJournalTables: Don't export.

formatZ <- function(n, ColPct, digits = 1, nsmall = 0, ...) {
  # Check the function
  stopifnot(length(n) == length(ColPct))
  # x <- chk_formatz_fn(cbind(n, ColPct))

  # Assign
  # n <- x[ ,1]
  # ColPct <- x[ ,2]

  # Format...
  pct <- paste0("(", format(ColPct, trim = TRUE, digits = digits, nsmall = nsmall, ...), "%)", ",")
  A <- matrix(paste (n, pct, sep = " "), ncol = 1)

  return(A)
}

# Check the function
chk_formatz_fn <- function(x) {
  # x can be a list, matrix, or data frame. Convert these to a dataframe.
  if (is.list(x) & length(x) == 2) {
    x <- data.frame(n = x[[1]], ColPct = x[[2]])
    # print(str(x))
  } else { if (is.matrix(x) | is.data.frame(x)) {
    x <- as.data.frame(x)
  } else {
    # All other classes cannot be combined.
    stop("Frequencies and ColPct cannot be combined since one is missing or is not numeric ",
            call. = FALSE)
  }}

  # print(str(x))

  # x may only have 2 columns
  warning("Frequencies (n) is assumed to be in 1st column; column percentages (ColPct) in second.",
          call. = FALSE)
  stopifnot(ncol(x) == 2)

  # The columns of x must be numeric.
  a <- ifelse(apply(x, 2, class) == "numeric",
               "",
               stop("Frequencies and ColPct cannot be combined since at least one is not numeric",
                    call. = FALSE)
  )

  return(x)
}

# Archive
##   #'@param x  A data frame or a matrix with 2 columns or a list with 2 entries in the following order:
#   #'             a frequency vector, n, and a column percentage vector, ColPct. A warning is produced to ensure
#   #'             this is followed. Names are not necessary, but helpful.
