#' Calculate weights for explanation model
#'
#' @param simulated_dataset Dataset simulated by sample_locally function.
#' @param explained_instance Instance to be explained.
#' @param kernel Chosen kernel function.
#'
#' @return Numeric vector of weights for each row in simulated dataset.
#'

calculate_weights <- function(simulated_dataset, explained_instance, kernel) {
  for_weights_x <- dplyr::bind_rows(simulated_dataset, explained_instance)
  for_weights_x <- dplyr::mutate_if(for_weights_x, is.character, as.factor)
  proxy_response <- rep(1, nrow(for_weights_x))
  for_weights <- dplyr::bind_cols(y = proxy_response, for_weights_x)
  proxy_model <- stats::lm(y ~., data = for_weights)
  model_matrix <- stats::model.matrix(proxy_model)
  explained_instance_coords <- model_matrix[nrow(model_matrix), ]
  other_observations_coords <- model_matrix[1:(nrow(model_matrix) - 1), ]
  sapply(as.data.frame(t(other_observations_coords)),
         function(x) kernel(explained_instance_coords, x))
}

#' Select variables for explanation model.
#'
#' @param source_data Simulated dataset.
#' @param target Name of the response variable.
#' @param response_family Name of distribution family to be used in lasso/glm fit.
#'
#' @importFrom stats as.formula model.matrix
#'
#' @return Character vector of names of selected variables
#'

select_variables <- function(source_data, target, response_family) {
  form <- as.formula(paste(target, "~."))
  explained_var_col <- which(colnames(source_data) == target)
  lasso_fit <- glmnet::cv.glmnet(model.matrix(form, data = source_data),
                                 as.matrix(source_data[, explained_var_col]),
                                 family = response_family,
                                 nfolds = 5, alpha = 1)
  coefs_lasso <- glmnet::coef.cv.glmnet(lasso_fit)
  nonzero_coefs <- row.names(coefs_lasso)[which(as.numeric(coefs_lasso) != 0)]
  nonzero_coefs <- nonzero_coefs[nonzero_coefs != "(Intercept)"]
  factors <- colnames(source_data)[sapply(source_data,
                                          function(x)
                                            is.character(x) | is.factor(x))]
  selected_vars <- colnames(source_data)[colnames(source_data) %in% nonzero_coefs]

  if(length(factors) != 0) {
    selected_vars <- selected_vars[!is.na(selected_vars)]
    factors_lasso <- setdiff(nonzero_coefs, selected_vars)
    selected_factors_lgl <- sapply(factors, function(x) any(grepl(x, factors_lasso)))
    selected_factors <- names(selected_factors_lgl)[selected_factors_lgl]
    selected_vars <- c(selected_vars,
                       selected_factors)
  }
  selected_vars
}

#' Fit white box model to the simulated data.
#'
#' @param live_object List return by add_predictions function.
#' @param white_box String, learner name recognized by mlr package.
#' @param kernel function which will be used to calculate distance between simulated
#'        observations and explained instance.
#' @param standardize If TRUE, numerical variables will be scaled to have mean 0, variance 1
#'        before fitting explanation model.
#' @param selection If TRUE, variable selection based on glmnet implementation of LASSO
#'        will be performed.
#' @param response_family family argument to glmnet (and then glm) function.
#'        Default value is "gaussian"
#' @param predict_type Argument passed to mlr::makeLearner() argument "predict.type".
#'        Defaults to "response".
#' @param hyperpars Optional list of values of hyperparameteres of a model.
#'
#' @return List consting of
#' \item{data}{Dataset used to fit explanation model (may have less column than the original)}
#' \item{model}{Fitted explanation model}
#' \item{explained_instance}{Instance that is being explained}
#'
#' @export
#'
#' @import mlr
#'
#' @examples
#' \dontrun{
#' fitted_explanation <- fit_explanation2(local_exploration1, "regr.lm", selection = TRUE)
#' }
#'

fit_explanation2 <- function(live_object, white_box = "regr.lm",
                            kernel = gaussian_kernel, standardize = FALSE,
                            selection = FALSE, response_family = "gaussian",
                            predict_type = "response", hyperpars = list()) {
  if(!(any(colnames(live_object$data) == live_object$target)))
    stop("First call add_predictions function to add black box predictions.")
  if(dplyr::n_distinct(live_object$data[[live_object$target]]) == 1)
    stop("All predicted values were equal.")
  source_data <- dplyr::select_if(live_object$data,
                                  function(x) dplyr::n_distinct(x) > 1)
  source_data <- dplyr::mutate_if(source_data, is.factor, droplevels)
  response_ncol <- which(colnames(source_data) == live_object$target)

  explained_instance <- live_object$explained_instance[, colnames(live_object$explained_instance) %in% colnames(source_data)]

  if(standardize) {
    source_data <- dplyr::mutate_at(source_data,
                     dplyr::vars(setdiff(1:ncol(source_data), response_ncol)),
                     function(x) {
                       if(is.numeric(x)) {
                          as.numeric(scale(x, scale = FALSE))
                       } else {
                         x
                       }
                     })
  }

  if(selection) {
    selected_vars <- select_variables(source_data, live_object$target,
                                      response_family)
  } else {
    selected_vars <- colnames(source_data)
  }

  list_learners <- suppressWarnings(mlr::listLearners(properties = "weights")$short.name)
  if(any(grepl(gsub("classif.", "", white_box), list_learners)) |
     any(grepl(gsub("regr.", "", white_box), list_learners))) {
    response_ncol_instance <- which(colnames(explained_instance) == live_object$target)
    live_weights <- calculate_weights(source_data[, -response_ncol],
                                      explained_instance[, -response_ncol_instance],
                                      kernel)
    if(dplyr::n_distinct(live_weights) == 1)
      live_weights <- NULL
  } else {
    warning("Chosen method does not support weights.")
    live_weights <- NULL
  }

  mlr_task <- create_task(white_box,
                          source_data[, unique(c(selected_vars, live_object$target))],
                          live_object$target,
                          live_weights)

  if(grepl("glm", white_box) & !(response_family == "poisson" | response_family == "binomial")) {
    hyperpars <- c(hyperpars, family = response_family)
  }
  lrn <- mlr::makeLearner(white_box, predict.type = predict_type, par.vals = hyperpars)

  explainer <- list(data = source_data,
       model = mlr::train(lrn, mlr_task),
       explained_instance = live_object$explained_instance,
       weights = live_weights,
       selected_variables = selection)
  class(explainer) <- c("live_explainer", "list")
  explainer
}
