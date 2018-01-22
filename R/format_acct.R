#' Function to simply format currency values
#' Formatting will follow Excel Accounting style
#' @param x numeric vector
#' @param digits numeric to denote number of significant digits
#' @param shown.in character to express value in Thousands, Millions, or Billions
#'
#' @return character vector of length(x) with desired formatting
#' @export
#'

format_acct <- function(x, digits = 2, shown.in = NULL) {

  if (!is.numeric(x)) stop('x must be numeric!')

  # If shown.in is not null then divide by appropriate value
  if (!is.null(shown.in)) {

    shown.in <- toupper(shown.in)

    if (shown.in == 'K') {
      x <- x/1e3
    } else if (shown.in == 'M') {
      x <- x/1e6
    } else if (shown.in == 'B') {
      x <- x/1e9
    } else {NA}

  }

  # Round x to specified number of decimal places
  format_x <- format(round(abs(x), digits), nsmall = digits, big.mark = ',')
  # Remove any whitespace
  format_x <- gsub('\\s+', '', format_x)

  # Find any negative values - these need to be specially formatted
  negative <- !is.na(x) & x < 0

  # Add dollar sign and ensure that round numbers (i.e. $100) still show desired decimal places
  # Negative values will have parens
  val <- paste0('$',
                ifelse(negative, '(', ''),
                format_x, shown.in,
                ifelse(negative, ')', ''))

  # Set NA values to NA
  is_na <- is.na(x)
  val[is_na] <- NA

  return(val)

}
