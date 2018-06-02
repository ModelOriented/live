#' Set date values to one value
#'
#' @param data Data frame to change.
#' @param explained_instance 1-row data frame with instance of interest.
#'

set_constant_dates <- function(data, explained_instance) {
  date_cols <- (1:ncol(data))[unlist(lapply(data,
                                            function(x) lubridate::is.Date(x) | lubridate::is.POSIXt(x)),
                                     use.names = FALSE)]
  if(length(date_cols) == 0) {
    return(data)
  } else {
    for(k in date_cols) {
      data.table::set(data, j = as.integer(k),
                      value = explained_instance[1, as.integer(k)])
    }
    data
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

generate_neighbourhood <- function(data, explained_instance, size) {
  data <- data.table::as.data.table(data)
  neighbourhood <- data.table::rbindlist(lapply(1:size, function(x) explained_instance))
  for(k in 1:nrow(neighbourhood)) {
    picked_var <- sample(1:ncol(data), 1)
    data.table::set(neighbourhood, i = as.integer(k), j = as.integer(picked_var),
                    data[sample(1:nrow(data), 1), picked_var, with = FALSE])
  }
  as.data.frame(set_constant_dates(neighbourhood, explained_instance))
}

#' Add predictions to generated dataset.
#'
#' @param data Original data frame used to generate new dataset.
#' @param black_box String with mlr signature of a learner or a model with predict interface.
#' @param explained_var Name of a column with the variable to be predicted.
#' @param similar Dataset created for local exploration.
#' @param predict_function Either a "predict" function that returns a vector of the
#'        same type as response or custom function that takes a model as a first argument,
#'        new data used to calculate predictions as a second argument called "newdata"
#'        and returns a vector of the same type as response.
#'        Will be used only if a model object was provided in the black_box argument.
#' @param hyperpars Optional list of (hyper)parameters to be passed to mlr::makeLearner.
#' @param ... Additional parameters to be passed to predict function.
#'
#' @return A list containing black box model object and predictions.
#'

give_predictions <- function(data, black_box, explained_var, similar, predict_function,
                             hyperpars = list(), ...) {
  if(is.character(black_box)) {
    mlr_task <- create_task(black_box, as.data.frame(data), explained_var)
    lrn <- mlr::makeLearner(black_box, par.vals = hyperpars)
    trained <- mlr::train(lrn, mlr_task)
    pred <- predict(trained, newdata = as.data.frame(similar))
    list(model = mlr::getLearnerModel(trained),
         predictions = pred[["data"]][["response"]])
  } else {
    list(model = black_box,
         predictions = predict_function(black_box, similar, ...))
  }
}


#' Generate dataset for local exploration.
#
#' DEPRECATED. Please refer to sample_locally2 function for updated and improved interface.
#' This function will be removed in the future and was left only
#' to remain consistent with the examples given in https://arxiv.org/abs/1804.01955.
#' For more, see NEWS and vignette.
#'
#' @param data Data frame from which new dataset will be simulated.
#' @param explained_instance One row data frame with the same variables
#'        as in data argument. Local exploration will be performed around this observation.
#' @param explained_var Name of a column with the variable to be predicted.
#' @param size Number of observations is a simulated dataset.
#' @param standardise If TRUE, numerical variables will be scaled to have mean 0, var 1.
#'
#' @return list
#'
#' @export
#'
#' @examples
#' \dontrun{
#' dataset_for_local_exploration <- sample_locally(data = wine,
#'                                                explained_instance = wine[5, ],
#'                                                explained_var = "quality",
#'                                                size = 50,
#'                                                standardise = TRUE)
#' }
#'

sample_locally <- function(data, explained_instance, explained_var, size,
                           standardise = FALSE) {
  check_conditions(data, explained_instance, size)
  explained_var_col <- which(colnames(data) == explained_var)
  similar <- generate_neighbourhood(data[, -explained_var_col],
                                    explained_instance[, -explained_var_col], size)
  if(standardise) {
    vscale <- function(x) as.vector(scale(x))
    similar <- dplyr::mutate_if(similar, is.numeric, vscale)
  }

  list(data = similar, target = explained_var)
}


#' Add black box predictions to generated dataset
#'
#' DEPRECATED. Please refer to add_predictions2 function for updated and improved interface.
#' This function will be removed in the future and was left only
#' to remain consistent with the examples given in https://arxiv.org/abs/1804.01955.
#' For more, see NEWS and vignette.
#'
#' @param data Original data frame used to generate new dataset.
#' @param to_explain List return by sample_locally function.
#' @param black_box_model String with mlr signature of a learner or a model with predict interface.
#' @param predict_fun Either a "predict" function that returns a vector of the
#'        same type as response or custom function that takes a model as a first argument,
#'        and data used to calculate predictions as a second argument
#'        and returns a vector of the same type as respone.
#'        Will be used only if a model object was provided in the black_box argument.
#' @param hyperparams Optional list of (hyper)parameters to be passed to mlr::makeLearner.
#' @param ... Additional parameters to be passed to predict function.
#'
#' @return list containing simulated dataset with added predictions,
#'              name of a response variable and black box model object.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' local_exploration1 <- add_predictions(wine, dataset_for_local_exploration,
#'                                       black_box_model = "regr.svm")
#' # Pass trained model to the function.
#' svm_model <- svm(quality ~., data = wine)
#' local_exploration2 <- add_predictions(wkne, dataset_for_local_exploration,
#'                                       black_box_model = svm_model)
#' }
#'

add_predictions <- function(data, to_explain, black_box_model, predict_fun = predict,
                            hyperparams = list(), ...) {
  trained_black_box <- give_predictions(data = data,
                                        black_box = black_box_model,
                                        explained_var = to_explain$target,
                                        similar = to_explain$data,
                                        predict_function = predict_fun,
                                        hyperpars = hyperparams,
                                        ...)
  to_explain$data[[to_explain$target]] <- trained_black_box$predictions

  list(data = to_explain$data, target = to_explain$target,
       model = trained_black_box$model)
}


#' Fit white box model to the simulated data.
#'
#' DEPRECATED. Please refer to fit_explanation2 function for updated and improved interface.
#' This function will be removed in the future and was left only
#' to remain consistent with the examples given in https://arxiv.org/abs/1804.01955.
#' For more, see NEWS and vignette.
#'
#' @param live_object List return by add_predictions function.
#' @param white_box String, learner name recognized by mlr package.
#' @param selection If TRUE, variable selection based on glmnet implementation of LASSO
#'        will be performed.
#' @param response_family family argument to glmnet (and then glm) function.
#'                        Default value is "gaussian"
#' @param predict_type Argument passed to mlr::makeLearner() argument "predict.type".
#'                     Defaults to "response".
#' @param hyperpars Optional list of values of hyperparameteres of a model.
#'
#' @return mlr object returned by train function.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' fitted_explanation <- fit_explanation(local_exploration1, "regr.lm", selection = TRUE)
#' }
#'

fit_explanation <- function(live_object, white_box, selection = FALSE,
                            response_family = "gaussian",
                            predict_type = "response", hyperpars = list()) {
  if(!(any(colnames(live_object$data) == live_object$target)))
    stop("First call add_predictions function to add black box predictions.")
  if(dplyr::n_distinct(live_object$data[[live_object$target]]) == 1)
    stop("All predicted values were equal.")

  if(selection) {
    selected_vars <- select_variables(live_object$data, 
                                      live_object$target, 
                                      response_family)
  } else {
    selected_vars <- colnames(live_object$data)
  }
  
  mlr_task <- create_task(white_box,
                          live_object$data[, unique(c(selected_vars, live_object$target))],
                          live_object$target)
  if(grepl("glm", white_box) & !(response_family == "poisson" | response_family == "binomial")) {
    hyperpars <- c(hyperpars, family = response_family)
  }
  lrn <- mlr::makeLearner(white_box, predict.type = predict_type, par.vals = hyperpars)

  mlr::train(lrn, mlr_task)
}


#' Waterfall plot or forestplot for lm/glm explanations.
#'
#' DEPRECATED
#'
#' @param plot_type Chr, "forestplot" or "waterfallplot" depending
#'                  on which type of plot is to be created.
#' @param fitted_model glm or lm object.
#' @param explained_instance Observation around which model was fitted.
#' @param classif logical, if TRUE, probabilities will be plotted 
#'
#' @return plot (ggplot2 or lattice)
#'

plot_regression <- function(plot_type, fitted_model, explained_instance, classif) {
  if(plot_type == "forestplot") {
    forestmodel::forest_model(fitted_model)
  } else {
    if(classif) {
      plot(breakDown::broken(fitted_model, explained_instance, baseline = "intercept"),
           trans = function(x) exp(x)/(1 + exp(x)))
    } else {
      plot(breakDown::broken(fitted_model, explained_instance, baseline = "intercept"))
    }
  }
}


#' Plotting white box models.
#'
#' DEPRECATED. Please refer to generic plot function for updated and improved interface.
#' This function will be removed in the future and was left only
#' to remain consistent with the examples given in https://arxiv.org/abs/1804.01955.
#' For more, see NEWS and vignette.
#'
#' @param model object returned by mlr::train function.
#' @param regr_plot_type Chr, "forestplot" or "waterfallplot" depending
#'                       on which type of plot is to be created.
#'                       if lm/glm model is used as interpretable approximation.
#' @param explained_instance Observation around which model was fitted.
#'                           Needed only if waterfall plot is drawn.
#'
#' @return plot (ggplot2 or base)
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Forest plot for regression
#' plot_explanation(fitted_explanation1, "forestplot", wine[5, ])
#' # Waterfall plot
#' plot_explanation(fitted_explanation1, "waterfallplot", wine[5, ])
#' # Plot decision tree
#' plot_explanation(fitted_explanation2)
#' }
#'

plot_explanation <- function(model, regr_plot_type = NULL, explained_instance = NULL) {
  trained_model <- mlr::getLearnerModel(model)
  classif <- model$learner$type == "classif"
  
  if(any(grepl("lm", class(trained_model)))) {
    plot_regression(regr_plot_type, trained_model, explained_instance, classif)
  } else {
    plot(trained_model)
  }
}

#' Plotting white box models.
#'
#' DEPRECATED. Please refer to the generic plot function.
#'
#' @param explained_model List returned by fit_explanation function.
#' @param regr_plot_type Chr, "forest" or "waterfall" depending
#'                       on which type of plot is to be created.
#'                       if lm/glm model is used as interpretable approximation.
#'
#' @return plot (ggplot2 or base)
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Forest plot for regression
#' plot_explanation(fitted_explanation1, "forest", wine[5, ])
#' # Waterfall plot
#' plot_explanation(fitted_explanation1, "waterfall", wine[5, ])
#' # Plot decision tree
#' plot_explanation(fitted_explanation2)
#' }
#'

plot_explanation2 <- function(explained_model, regr_plot_type = NULL) {
  trained_model <- mlr::getLearnerModel(explained_model$model)
  present_variables <- colnames(explained_model$data)
  explained_instance <- explained_model$explained_instance[, present_variables]
  classif <- explained_model$model$learner$type == "classif"
  
  if(any(grepl("lm", class(trained_model)))) {
    plot_regression2(regr_plot_type, trained_model, explained_instance, classif)
  } else {
    plot(trained_model)
  }
}
