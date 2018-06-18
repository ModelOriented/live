#' LIME: sampling for local exploration by changing one value per observation.
#'
#' @param data Data frame from which observations will be generated.
#' @param explained_instance A row in an original data frame (as a data.frame).
#' @param size Number of observations to be generated.
#' @param fixed_variables Names of column which will not be changed while sampling.
#' @param seed Seed to set before sampling. If NULL, results will not be reproducible.
#'
#' @return data.frame
#'
#'

generate_neighbourhood2 <- function(data, explained_instance, size, fixed_variables, seed = NULL) {
  data <- data.table::as.data.table(data)
  neighbourhood <- data.table::rbindlist(lapply(1:size, function(x) explained_instance))
  if(!is.null(seed)) {
    set.seed(seed)
  }
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

permutation_neighbourhood <- function(data, explained_instance, size, fixed_variables, seed = NULL) {
  neighbourhood <- data.table::rbindlist(lapply(1:size, function(x)
    explained_instance))
  if(!is.null(seed)) {
    set.seed(seed)
  }
  for(k in 1:ncol(neighbourhood)) {
    data.table::set(neighbourhood, j = as.integer(k),
                    value = data[sample(1:nrow(data), size, replace = TRUE),
                                 k])
  }
  as.data.frame(set_constant_variables(neighbourhood, explained_instance, fixed_variables))
}

#' LIME: sampling for local exploration using normal distribution.
#'
#' @inheritParams generate_neighbourhood2 
#' @param ... Mean and covariance matrix for the normal distribution.
#' 
#' @importFrom data.table as.data.table rbindlist 
#' @importFrom utils head
#' 
#' @return data.frame
#' 

normal_neighbourhood <- function(data, explained_instance, size, fixed_variables, seed = NULL, ...) {
  
  numerical_features <- dplyr::select_if(data, is.numeric)
  categorical_features <- dplyr::select_if(data, function(x) !is.numeric(x))

  if(!is.null(seed)) {
    set.seed(seed)
  }
  numerical_part <- MASS::mvrnorm(size, ...)
  
  colnames(numerical_part) <- colnames(numerical_features)

  if(ncol(categorical_features) > 0) {
    if(size <= nrow(categorical_features)) {
      categorical_features <- as.data.table(head(categorical_features, size))
    } else {
      categorical_features <- as.data.table(rbindlist(lapply(1:size, 
                                function(x) categorical_features[1, , drop = FALSE])))
    }
    
    categorical_col_nums <- which(sapply(data, function(x) !is.numeric(x)))

    for(k in 1:ncol(categorical_features)) {
      data.table::set(categorical_features, j = as.integer(k),
                      value = data[sample(1:nrow(data), size, replace = TRUE), 
                                   categorical_col_nums[k]])
    }
  }  
    
  neighbourhood <- as.data.frame(cbind(numerical_part, categorical_features))
  as.data.frame(set_constant_variables(neighbourhood,
                                       explained_instance, fixed_variables))
}
