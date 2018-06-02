#' LIME: sampling for local exploration by changing one value per observation.
#'
#' @param data Data frame from which observations will be generated.
#' @param explained_instance A row in an original data frame (as a data.frame).
#' @param size Number of observations to be generated.
#' @param fixed_variables Names of column which will not be changed while sampling.
#'
#' @return data.frame
#'
#'

generate_neighbourhood2 <- function(data, explained_instance, size, fixed_variables) {
  data <- data.table::as.data.table(data)
  neighbourhood <- data.table::rbindlist(lapply(1:size, function(x) explained_instance))
  for(k in 1:nrow(neighbourhood)) {
    picked_var <- sample(1:ncol(data), 1)
    data.table::set(neighbourhood, i = as.integer(k), j = as.integer(picked_var),
                    data[sample(1:nrow(data), 1), picked_var, with = FALSE])
  }
  as.data.frame(set_constant_variables(neighbourhood, explained_instance, fixed_variables))
}


#' LIME: sampling for local exploration by permuting all columns.
#'
#' @inheritParams generate_neighbourhood2
#'
#' @return data frame
#'

permutation_neighbourhood <- function(data, explained_instance, size, fixed_variables) {
  neighbourhood <- data.table::rbindlist(lapply(1:size, function(x)
    explained_instance))
  for(k in 1:ncol(neighbourhood)) {
    data.table::set(neighbourhood, j = as.integer(k),
                    value = data[sample(1:nrow(data), size, replace = TRUE),
                                 k])
  }
  as.data.frame(set_constant_variables(neighbourhood, explained_instance, fixed_variables))
}

#' LIME: sampling for local exploration based on normal distribution
#' 
#' @inheritParams generate_neighbourhood2

normal_neighbourhood <- function(data, explained_instance, size, fixed_variable, ...)

