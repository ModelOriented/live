#' LIME kernel that treats all observations as equally similar to 
#' observation of interest
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
  

#' LIME kernel from the original article
#' 
#' @param explained_instance explained instance
#' @param simulated_instance new observation
#'
#' @return numeric
#'   
#' @export
#' 
  
gaussian_kernel <- function(explained_instance, simulated_instance) {
  
}
  

#' LIME kernel from the article about Shapley values
#' 
#' @param explained_instance explained instance
#' @param simulated_instance new observation
#'
#' @return numeric
#'   
#' @export
#' 
  
shapley_kernel <- function(explained_instance, simulated_instance) {
  
}


#' LIME kernel equal to euclidean distance
#' 
#' @param explained_instance explained instance
#' @param simulated_instance new observation
#'
#' @return numeric
#'   
#' @export
#' 

euclidean_kernel <- function(explained_instance) {
  
}