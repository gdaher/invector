#'Find a vector of strings/numbers in a single string.
#'
#' \code{list_in_vector} returns the sum of all the values present in its arguments.
#'
#'
#'Test whether each element of list[[index]] can be found in the corresponding vector[index] for the whole list. If yes, returns TRUE else returns FALSE
#'
#' @param list list of either numeric or character vectors that you would like to find.
#' @param vector a numeric or character vector to be searched
#' @param all default is FALSE. If FALSE, returns a logical vector. If TRUE, returns a boolean indicating whether or not all elements contained matches.
#' @keywords multiple, keys, vector, list
#' @return boolean. True or False
#' @export
#' @examples
#' list <- list(c('hi','bi','sc'),c('h','o','g'),c('fu','lc','rum'))
#' vector <-c('hibiscus','goh','fulrum')
#' list_in_vector(list,vector)
#' list_in_vector(list,vector,all=FALSE)
##
list_in_vector <- function(list,vector,all=FALSE) {

  true_or_false <- mapply(find_in_string, list, vector)#returns a vector of true/false
  if(all){
    return(all(true_or_false))
  }
  else{
    return(true_or_false)
  }

}
