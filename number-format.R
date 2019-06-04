##
# Adjusted from https://stackoverflow.com/a/47710294
##
number_format <- function(x, digits = NULL)
{
  intl <- c(1e3, 1e6, 1e9, 1e12)
  suffixes <- c(' K', ' M', ' B', ' T')
  
  i <- findInterval(x, intl)
  
  i_neg <- findInterval(-x, intl)
  
  result <- character(length(x))
  
  # Note: for ggplot2 the last label element of x is NA, so we need to handle it
  ind_format <- !is.na(x) & i > 0
  neg_format <- !is.na(x) & i_neg > 0
  
  # Format only the elements that need to be formatted
  # with suffixes and possible rounding
  result[ind_format] <- paste0(
    formatC(x[ind_format] / intl[i[ind_format]], format = "f", digits = digits),
    suffixes[i[ind_format]]
  )
  # Format negative numbers
  result[neg_format] <- paste0(
    formatC(x[neg_format] / intl[i_neg[neg_format]], format = "f", digits = digits),
    suffixes[i_neg[neg_format]]
  )
  
  # To the rest only apply rounding
  result[!ind_format & !neg_format] <- as.character(
    formatC(x[!ind_format & !neg_format], format = "f", digits = digits)
  )
  
  return(invisible(result))
}
