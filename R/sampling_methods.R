generate_neighbourhood <- function(data, explained_instance, size, fixed_variables, seed = NULL) {
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

#' @importFrom utils head

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
      categorical_features <- data.table::as.data.table(head(categorical_features, size))
    } else {
      categorical_features <- data.table::as.data.table(data.table::rbindlist(lapply(1:size, 
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
