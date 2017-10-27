#'Find a vector of strings/numbers in a single string.
#'
#' \code{find_in_string} returns the sum of all the values present in its arguments.
#'
#'searches for all elements of vector b in string a, and returns true/false depending on whether or not all the elements
#'in b can also be found in a, regardless of order.
#'
#' @param b numeric or character vector, which includes all the elements you are looking for.
#' @param a the string to be searched
#' @keywords multiple, keys, string, search
#' @return boolean. True or False
#' @export
#' @examples
#' b<-c("i", "x", "v")
#' a<-"ix"
#' c <-"vix"
#' find_in_string(b,a)
#' find_in_string(b,c)
## Searches for something
find_in_string <- function(b,a) {
  o <- length(b)
  y <- NULL
  for (i in 1:o)
  {
    n <- grepl(b[i],a)
    y <-  c(y,n)
  }

  return(all(y))
}
