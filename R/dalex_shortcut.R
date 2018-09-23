#' Fit local model around the observation: shortcut for DALEX explainer objects
#'
#' @param explainer a model to be explained, preprocessed by the DALEX::explain function
#' @param observation a new observation for which predictions need to be explained
#' @param target_variable_name name of the response variablea as a character
#' @param n_new_obs Number of observation in the simulated dataset
#' @param local_model Character specyfing mlr learner to be used as a local model
#' @param select_variables If TRUE, variable selection will be performed while
#' fitting the local linear model
#' @param predict_type Argument passed to mlr::makeLearner() argument "predict.type"
#' while fitting the local model. Defaults to "response"
#' @param kernel_type Function which will be used to calculate distances from
#' simulated observation to explained instance
#' @param ... Arguments to be passed to sample_locally2 function
#'
#' @export
#'

live <- function(explainer, observation, target_variable_name,
                 n_new_obs,
                 local_model = "regr.lm",
                 select_variables = F,
                 predict_type = "response",
                 kernel_type = gaussian_kernel, ...) {
  
  neighbourhood <- sample_locally2(explainer$data,
                                   observation,
                                   target_variable_name,
                                   n_new_obs,
                                   ...)
  with_predictions <- add_predictions2(neighbourhood,
                                       explainer$model,
                                       predict_fun = explainer$predict_function)
  live::fit_explanation2(with_predictions,
                         white_box = local_model,
                         selection = select_variables,
                         predict_type = predict_type,
                         kernel = kernel_type)
}