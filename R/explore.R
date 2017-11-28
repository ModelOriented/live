#' Replace one element of each row with a random element from given data frame.
#' 
#' @param chosen_vars Columns to sample from [as positions/numbers].
#' @param data Data frame to sample from.
#' @param target Data frame in which the replacement is done.
#' 
#' @return data.frame
#' 

replace_items_by_rows <- function(chosen_vars, data, target) {
  lapply(seq_along(chosen_vars), function(x) {
    row <- target[x, ]
    row[1, chosen_vars[x]] <- sample(unlist(data[, chosen_vars[x]]), 1)
    row
  }) %>%
    dplyr::bind_rows()
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
    neighbourhood <- replace_items_by_rows(sort(sample(ncol(neighbourhood), size)), 
                                           data, neighbourhood)
  } else {
    k = size %/% dimension
    r = size %% dimension
    separate <- c(rep(1:k, each = dimension), rep(k + 1, r))
    divided <- split(neighbourhood, separate)
    divided[1:k] <- lapply(divided[1:k], function(x)
                           replace_items_by_rows(sort(sample(ncol(x), dimension)), 
                                                 data, x))
    if(r > 0) {
      divided[[k + 1]] <- replace_items_by_rows(sort(sample(ncol(divided[[k + 1]]), r)),                                         data, divided[[k + 1]])
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


#' Add predictions to generated dataset.
#' 
#' @param black_box String with mlr signature of a learner or a model with predict interface.
#' @param explained_var Name of a column with the variable to be predicted.
#' @param similar Dataset created for local exploration.
#' @param predict_function Either a "predict" function that returns a vector of the
#'        same type as response or custom function that takes a model as a first argument,
#'        new data used to calculate predictions as a second argument called "newdata"
#'        and returns a vector of the same type as respone. 
#'        Will be used only if a model object was provided in the black_box argument.
#' @param ... Additional parameters to be passed to predict function.
#' 
#' @return Vector of model predictions.
#' 

give_predictions <- function(black_box, explained_var, similar, predict_function, ...) {
  if(is.character(black_box)) {  
    mlr_task <- create_task(black_box, as.data.frame(similar), explained_var)
    pred <- mlr::makeLearner(black_box) %>% 
      mlr::train(mlr_task) %>%
      predict(newdata = as.data.frame(similar))
    pred[["data"]][["response"]]
  } else {
    predict_function(black_box, 
                     newdata = similar, ...)
  }
}


#' Generate dataset for local exploration.
#'
#' @param data Data frame from which new dataset will be simulated.
#' @param explained_instance One row data frame with the same variables 
#'        as in data argument. Local exploration will be performed around this observation.
#' @param explained_var Name of a column with the variable to be predicted.
#' @param size Number of observations is a simulated dataset.
#' @param standardise If TRUE, numerical variables will be scaled to have mean 0, var 1.
#' @param predict_function Either a "predict" function that returns a vector of the
#'        same type as response or custom function that takes a model as a first argument,
#'        new data used to calculate predictions as a second argument called "newdata"
#'        and returns a vector of the same type as respone. 
#'        Will be used only if a model object was provided in the black_box argument.
#' @param ... Additional parameters to be passed to predict function.
#'
#' @return list
#' 
#' @export
#' 
#' @examples 
#' \dontrun{
#' # Train model inside the function.
#' dataset_for_local_exploration <- sample_locally(data = winequality_red,
#'                                                explained_instance = winequality_red[5, ], 
#'                                                explained_var = "quality", 
#'                                                size = 50,
#'                                                standardise = TRUE)
#' # Pass trained model to the function. 
#' library(e1071)
#' svm_model <- svm(quality ~., data = winequality_red)
#' dataset_for_local_exploration2 <- sample_locally(data = winequality_red,
#'                                                explained_instance = winequality_red[5, ], 
#'                                                explained_var = "quality", 
#'                                                size = 50,
#'                                                standardise = TRUE)
#' }
#'                                                                                         

sample_locally <- function(data, explained_instance, explained_var, size, standardise = FALSE) {
  explained_var_col <- which(colnames(data) == explained_var)
  similar <- generate_neighbourhood(data[, -explained_var_col], 
                                    explained_instance[, -explained_var_col], size)
  if(standardise) {
    similar <- similar %>%
      dplyr::mutate_if(is.numeric, function(x) as.vector(scale(x)))
  }
  
  list(data = similar, target = explained_var)
}


#' Add black box predictions to generated dataset
#'
#' @param to_explain List return by sample_locally function.
#' @param black_box_model String with mlr signature of a learner or a model with predict interface.
#' @param predict_function Either a "predict" function that returns a vector of the
#'        same type as response or custom function that takes a model as a first argument,
#'        new data used to calculate predictions as a second argument called "newdata"
#'        and returns a vector of the same type as respone. 
#'        Will be used only if a model object was provided in the black_box argument.
#' @param ... Additional parameters to be passed to predict function.
#' 
#' @return list
#' 
#' @export
#' 

add_predictions <- function(to_explain, black_box_model, predict_fun = predict, ...) {
  to_explain$data[[explained_var]] <- give_predictions(black_box = black_box_model,
                                                       explained_var = to_explain$explained_var,
                                                       similar = to_explain$data, 
                                                       predict_function = predict_fun, 
                                                       ...)
  list(data = to_explain$data, target = to_explain$explained_var)
}
