#' Replace one element of a vector with a random element from given data frame.
#' 
#' @param chosen_vars Columns to sample from [as positions/numbers].
#' @param data Data frame to sample from.
#' @param new_data Data frame in which the replacement is done.
#' 
#' @return data.frame
#' 

replace_items <- function(chosen_vars, data, new_data) {
  lapply(seq_along(chosen_vars), function(x) {
    row <- new_data[x, ]
    row[1, chosen_vars[x]] <- sample(unlist(data[, chosen_vars[x]]), 1)
    row
  })
}

#' Change value of one variable all rows.
#'
#' @param data Data frame from which observations will be generated.
#' @param new_data Data frame created by generate_neighbourhood function.
#' @param steps Number of variables to change.
#' 
#' @return data.frame
#' 

walk_through_vars <- function(data, new_data, steps) {
  if(steps == ncol(new_data)) {
    new_data <- replace_items(1:ncol(data), data, new_data)
  } else {
    chosen_vars <- sort(sample(ncol(new_data), steps))
    new_data <- replace_items(chosen_vars, data, new_data)
  }
  dplyr::bind_rows(new_data)
}


#' LIME: sampling for local exploration
#'
#' @param data Data frame from which observations will be generated.
#' @param explained_instance A row in an original data frame (as a data.frame).
#' @param size Number of observations to be generated.
#'
#' @return data.frame
#'
#' @export
#'

generate_neighbourhood <- function(data, explained_instance, size) {
  dimension <- ncol(data)
  neighbourhood <- dplyr::bind_rows(lapply(1:size, function(x) explained_instance))
  if(size <= dimension) {
     new_data <- walk_through_vars(data, neighbourhood, size)
  } else {
    k = size %/% dimension
    r = size %% dimension
    separate <- c(rep(1:k, each = dimension), rep(k + 1, r))
    divided <- split(neighbourhood, separate)
    divided[1:k] <- lapply(divided[1:k], function(x)
      walk_through_vars(data, x, dimension))
    if(r > 0) {
      divided[[k + 1]] <- walk_through_vars(data, divided[[k + 1]], r)
    }
    neighbourhood <- dplyr::bind_rows(divided)
  }
  neighbourhood
}
