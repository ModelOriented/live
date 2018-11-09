check_for_na <- function(data, explained_instance) {
  if(any(is.na(explained_instance))) warning("Missing values present in explained instance.")
  if(any(is.na(data))) warning("Missing values present in dataset. NAs will be omitted while sampling.")
}


check_conditions <- function(data, explained_instance, size) {
  if(nrow(data) == 0) stop("Empty data frame")
  if(ncol(data) == 0) stop("Data frame has no columns")
  if(size <= 0 | !is.finite(size)) stop("Size has to be a positive integer")
  if(any(colnames(data) != colnames(explained_instance)))
    stop("Explained instance must have the same variables as data")
}


set_constant_variables <- function(data, explained_instance, col_names) {
  cols <- (1:ncol(data))[which(colnames(data) %in% col_names)]
  if(length(cols) == 0) {
    return(data)
  } else {
    for(k in cols) {
      data.table::set(data, j = as.integer(k),
                      value = explained_instance[1, as.integer(k)])
    }
    data
  }
}


create_task <- function(model, dataset, target_var, weights = NULL) {
  if(grepl("regr", model)) {
    mlr::makeRegrTask(id = "lime_task",
                      data = as.data.frame(dataset),
                      target = target_var,
                      weights = weights)
  } else {
    mlr::makeClassifTask(id = "lime_task",
                         data = as.data.frame(dataset),
                         target = target_var)
  }
}
