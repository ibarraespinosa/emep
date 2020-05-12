#' Function to return emission factors
#'
#' \code{\link{find_ef}} returns the emission factors data.frame with the parameters
#' searched.
#' @param name character; Word to be searched among the names of the cpt gradients.
#' @return names that satisfy the search.
#' @note This functions runs grep.
#' @export
#' @examples \dontrun{
#' #do not run
#' }
find_ef <- function(name){
  a <- grep(name, sysdata)
  if(length(a) == 0) {
    message("There are no results. Please, repeat your search")
  } else {
    return(sysdata[a, ])
  }
}
