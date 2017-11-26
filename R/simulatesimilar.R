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
