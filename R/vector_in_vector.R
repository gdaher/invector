#' A Function for Finding Partial Matches in Vectors
#'
#' This function searches for each element of vector1 (needle) in the corresponding element of vector2 (haystack), and returns a logical vector. Unlike the %in% function, the search key can be found anywhere in element being searched (not just the beginning).
#' @param needle the vector whose elements are searched for
#' @param haystack the vector whose elements are searched
#' @param vector Logical. Default set to TRUE. Returns a logical vector for matches in each element of haystack. If vector = FALSE, a single boolean is returned indicating if all elements have a match.
#' @keywords vector, match, find
#' @export
#' @examples
#' needle <- c("i","ii","iii")
#' haystack <- c("i","xniiv", "ximini")
#' vector_in_vector(needle,haystack, vector = TRUE)
## searches for each element of vector1 in corresponding element of vector2, and returns a logical vector. 
## Unlike the %in% function, the search key can be found anywhere in element being searched (not just the beginning)
vector_in_vector <- function(needle,haystack, vector = TRUE) {
  true_or_false <- mapply(grepl, needle, haystack)#returns a vector of true/false
  ifelse(vector == FALSE,return(all(true_or_false)), return(true_or_false))
  
}