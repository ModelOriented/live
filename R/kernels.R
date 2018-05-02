#' LIME kernel that treats all observations as equally similar to 
#' observation of interest.
#' 
#' @param explained_instance explained instance
#' @param simulated_instance new observation
#'
#' @return numeric
#'   
#' @export
#' 

identity_kernel <- function(explained_instance, simulated_instance) {
  1
}
  

#' LIME kernel from the original article with sigma = 1.
#' 
#' @param explained_instance explained instance
#' @param simulated_instance new observation
#'
#' @return numeric
#'   
#' @export
#' 
  
gaussian_kernel <- function(explained_instance, simulated_instance) {
  exp(-sum((explained_instance - simulated_instance)^2))
}


#' LIME kernel equal to the inverse of euclidean distance.
#' 
#' @param explained_instance explained instance
#' @param simulated_instance new observation
#'
#' @return numeric
#'   
#' @export
#' 

euclidean_kernel <- function(explained_instance, simulated_instance) {
  1/(sqrt(sum((explained_instance - simulated_instance)^2)) + 
       all(simulated_instance == explained_instance))
}
