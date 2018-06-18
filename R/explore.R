#' Generate dataset for local exploration.
#'
#' @param data Data frame from which new dataset will be simulated.
#' @param explained_instance One row data frame with the same variables
#'        as in data argument. Local exploration will be performed around this observation.
#' @param explained_var Name of a column with the variable to be predicted.
#' @param size Number of observations is a simulated dataset.
#' @param method If "live", new observations will be created by changing one value
#'        per observation. If "lime", new observation will be created by permuting  all
#'        columns of data. If "normal", numerical features will be sampled from multivariate
#'        normal distribution specified by ... arguments mu and Sigma.
#' @param fixed_variables names or numeric indexes of columns which will not be changed
#'        while sampling.
#' @param seed Seed to set before sampling. If NULL, results will not be reproducible.
#' @param ... Mean and covariance matrix for normal sampling method.
#'
#' @return list consisting of
#' \item{data}{Simulated dataset.}
#' \item{target}{Name of the response variable.}
#' \item{explained_instance}{Instance that is being explained.}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' dataset_for_local_exploration <- sample_locally2(data = wine,
#'                                                explained_instance = wine[5, ],
#'                                                explained_var = "quality",
#'                                                size = 50,
#'                                                standardise = TRUE)
#' }
#'

sample_locally2 <- function(data, explained_instance, explained_var, size,
                           method = "live", fixed_variables = NULL, seed = NULL, ...) {
  check_conditions(data, explained_instance, size)
  explained_var_col <- which(colnames(data) == explained_var)
  if(method == "live") {
    similar <- generate_neighbourhood2(data[, -explained_var_col],
                                      explained_instance[, -explained_var_col], 
                                      size,
                                      fixed_variables,
                                      seed)
  } else if(method == "lime") {
    similar <- permutation_neighbourhood(data[, -explained_var_col],
                                         explained_instance[, -explained_var_col],
                                         size,
                                         fixed_variables,
                                         seed)
  } else {
    similar <- normal_neighbourhood(data[, -explained_var_col],
                                    explained_instance[, -explained_var_col],
                                    size,
                                    fixed_variables,
                                    seed,
                                    ...)
  }

  explorer <- list(data = similar,
       target = explained_var,
       explained_instance = explained_instance,
       sampling_method = method,
       fixed_variables = fixed_variables)
  class(explorer) <- c("live_explorer", "list")
  explorer
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
#' @importFrom stats predict
#'
#' @return A list that consists of black box model object and predictions.
#'

give_predictions2 <- function(data, black_box, explained_var, similar, predict_function,
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


#' Add black box predictions to generated dataset
#'
#' @param to_explain List return by sample_locally2 function.
#' @param black_box_model String with mlr signature of a learner or a model with predict interface.
#' @param data Original data frame used to generate new dataset.
#'        Need not be provided when a trained model is passed in
#'        black_box_model argument.
#' @param predict_fun Either a "predict" function that returns a vector of the
#'        same type as response or custom function that takes a model as a first argument,
#'        and data used to calculate predictions as a second argument
#'        and returns a vector of the same type as respone.
#'        Will be used only if a model object was provided in the black_box argument.
#' @param hyperparams Optional list of (hyper)parameters to be passed to mlr::makeLearner.
#' @param ... Additional parameters to be passed to predict function.
#'
#' @return list consisting of
#' \item{data}{Dataset generated by sample_locally2 function with response variable.}
#' \item{target}{Name of the response variable.}
#' \item{model}{Black box model which is being explained.}
#' \item{explained_instance}{Instance that is being explained.}
#'
#' @importFrom stats predict
#'
#' @export
#'
#' @examples
#' \dontrun{
#' local_exploration1 <- add_predictions2(wine, dataset_for_local_exploration,
#'                                       black_box_model = "regr.svm")
#' # Pass trained model to the function.
#' svm_model <- svm(quality ~., data = wine)
#' local_exploration2 <- add_predictions2(wine, dataset_for_local_exploration,
#'                                       black_box_model = svm_model)
#' }
#'

add_predictions2 <- function(to_explain, black_box_model, data = NULL, predict_fun = predict,
                            hyperparams = list(), ...) {
  if(is.null(data) & is.character(black_box_model))
    stop("Dataset for training black box model must be provided")
  trained_black_box <- give_predictions2(data = data,
                                        black_box = black_box_model,
                                        explained_var = to_explain$target,
                                        similar = to_explain$data,
                                        predict_function = predict_fun,
                                        hyperpars = hyperparams,
                                        ...)
  to_explain$data[[to_explain$target]] <- trained_black_box$predictions

  explorer <- list(data = to_explain$data,
       target = to_explain$target,
       model = trained_black_box$model,
       explained_instance = to_explain$explained_instance,
       sampling_method = to_explain$sampling_method,
       fixed_variables = to_explain$fixed_variables)
  class(explorer) <- c("live_explorer", "list")
  explorer
}
