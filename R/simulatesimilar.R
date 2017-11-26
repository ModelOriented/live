#' Replace one element of a vector with a random element from given data frame.
#' 
#' @param chosen_vars Columns to sample from [as positions/numbers].
#' @param data Data frame to sample from.
#' @param target Data frame in which the replacement is done.
#' 
#' @return data.frame
#' 

replace_items <- function(chosen_vars, data, target) {
  lapply(seq_along(chosen_vars), function(x) {
    row <- target[x, ]
    row[1, chosen_vars[x]] <- sample(unlist(data[, chosen_vars[x]]), 1)
    row
  }) %>%
    dplyr::bind_rows()
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
    replace_items(1:ncol(data), data, new_data)
  } else {
    chosen_vars <- sort(sample(ncol(new_data), steps))
    replace_items(chosen_vars, data, new_data)
  }
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
    neighbourhood <- walk_through_vars(data, neighbourhood, size)
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

#' Create regression or classification task.
#'
#' @param model Name of a used model in mlr format.
#' @param dataset Data frame on which model will be trained.
#' @param target_var Name of column in dataset containing explained variable.
#' 
#' @return 
#'

create_task <- function(model, dataset, target_var) {
  if(grepl("regr", model)) {
    mlr::makeRegrTask(id = "lime_task", 
                      data = as.data.frame(dataset),
                      target = target_var)
  } else {
    mlr::makeClassifTask(id = "lime_task", 
                         data = as.data.frame(dataset), 
                         target = target_var)
  }
}


#' Generate dataset for white box model based on black box model.
#'
#' @param data Data frame from which new dataset will be simulated.
#' @param explained_instance One row data frame with the same variables 
#'        as in data argument. Local exploration will be performed around this observation.
#' @param explained_var Name of a column with the variable to be predicted.
#' @param black_box String with mlr signature of a learner or a model with predict interface.
#' @param size Number of observations is a simulated dataset.
#' @param standardise If TRUE, numerical variables will be scaled to have mean 0, var 1.
#' @param predict_function Either a "predict" function that returns a vector of the
#'        same type as response or custom function that takes a model as a first argument,
#'        new data used to calculate predictions as a second argument called "newdata"
#'        and returns a vector of the same type as respone. 
#'        Will be used only if a model object was provided in the black_box argument.
#' @param ... Additional parameters to be passed to makeRegrTask function.
#'
#' @return list
#' 
#' @export
#'

simulate_similar <- function(data, explained_instance, explained_var, black_box,  
                            size, standardise = FALSE,
                            predict_function = predict, ...) {
  similar <- generate_neighbourhood(data, explained_instance, size)
  if(standardise) {
    similar <- similar %>%
      dplyr::mutate_if(is.numeric, function(x) as.vector(scale(x)))
  }
  
  if(is.character(black_box)) {  
    mlr_task <- create_task(black_box, as.data.frame(data), explained_var)
    pred <- mlr::makeLearner(black_box) %>% 
      mlr::train(mlr_task) %>%
      predict(newdata = as.data.frame(similar))
    similar[[explained_var]] <- pred[["data"]][["response"]]
  } else {
    similar[[explained_var]] <- predict_function(black_box, 
						  newdata = similar, ...)
  }
  
  list(data = similar, target = explained_var, black_box_model = black_box)
}


#' Fit white box model to the simulated data.
#' 
#' @param live_object List return by simulate_similar function. 
#' @param white_box String, learner name recognized by mlr package.
#' @param selection If TRUE, variable selection based on AIC will be performed.
#' @param maximum_depth maximum depth of a tree (when decision tree is used).
#'
#' @return mlr object returned by train function.
#' 
#' @export
#' 


train_white_box <- function(live_object, white_box, selection = FALSE, maximum_depth = 0) {
  if(n_distinct(live_object$data[[live_object$target]]) == 1) 
    stop("All predicted values were equal.")
  
  if(selection) {
    explained_var_col <- which(colnames(live_object$data) == live_object$target)
    selected_vars <- selectModel(as.data.frame(live_object$data[, -explained_var_col]), 
                                      unlist(live_object$data[, explained_var_col]), crit = aic)
  } else {
    selected_vars <- colnames(live_object$data)
  }
  
  mlr_task <- create_task(white_box, 
                          live_object$data[, unique(c(selected_vars, live_object$target))],
                          live_object$target)
  
  if(grepl("regr", white_box)) {
    lrn <- mlr::makeLearner(white_box)
  } else {
    lrn <- mlr::makeLearner(white_box, max_depth = maximum_depth)
  }
  
  mlr::train(lrn, mlr_task)
}   
