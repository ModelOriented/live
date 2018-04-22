

#' Fit white box model to the simulated data.
#'
#' @param live_object List return by add_predictions function.
#' @param white_box String, learner name recognized by mlr package.
#' @param kernel function which will be used to calculate distance between simulated
#'        observations and explained instance.
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
#' @examples
#' \dontrun{
#' fitted_explanation <- fit_explanation(local_exploration1, "regr.lm", selection = TRUE)
#' }
#'

fit_explanation <- function(live_object, white_box, kernel = identity_kernel,                            selection = FALSE, response_family = "gaussian",
                            predict_type = "response", hyperpars = list()) {
  if(dplyr::n_distinct(live_object$data[[live_object$target]]) == 1)
    stop("All predicted values were equal.")
  if(!(any(colnames(live_object$data) == live_object$target)))
    stop("First call add_predictions function to add black box predictions.")
  source_data <- select_if(live_object$data, function(x) n_distinct(x) > 1)
  
  if(selection) {
    form <- as.formula(paste(live_object$target, "~."))
    explained_var_col <- which(colnames(source_data) == live_object$target)
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

  } else {
    selected_vars <- colnames(source_data)
  }

  mlr_task <- create_task(white_box,
                          source_data[, unique(c(selected_vars, live_object$target))],
                          live_object$target)
  if(grepl("glm", white_box) & !(response_family == "poisson" | response_family == "binomial")) {
    hyperpars <- c(hyperpars, family = response_family)  
  }
  lrn <- mlr::makeLearner(white_box, predict.type = predict_type, par.vals = hyperpars)

  list(data = source_data,
       model = mlr::train(lrn, mlr_task),
       explained_instance = live_object$explained_instance)
}


#' Waterfall plot or forestplot for lm/glm explanations.
#'
#' @param plot_type Chr, "forestplot" or "waterfallplot" depending
#'                  on which type of plot is to be created.
#' @param fitted_model glm or lm object.
#' @param explained_instance Observation around which model was fitted.
#' @param scale Only for classification problems, "logit" or "probability".
#'
#' @return plot (ggplot2 or lattice)
#'

plot_regression <- function(plot_type, fitted_model, explained_instance, scale = NULL) {
  if(plot_type == "forestplot") {
    forestmodel::forest_model(fitted_model)
  } else {
    if(scale == "probability") {
      plot(breakDown::broken(fitted_model, explained_instance, baseline = "intercept"),
           trans = function(x) exp(x)/(1 + exp(x))) +
      ggplot2::scale_y_continuous(limits = c(0, 1), 
                                  name = "probability", 
                                  expand = c(0, 0))
    } else {
      plot(breakDown::broken(fitted_model, explained_instance, baseline = "intercept"))
    }
  }
}


#' Plotting white box models.
#'
#' @param explained_model List returned by fit_explanation function.
#' @param regr_plot_type Chr, "forestplot" or "waterfallplot" depending
#'                       on which type of plot is to be created.
#'                       if lm/glm model is used as interpretable approximation.
#' @param scale When probabilities are predicted, they can be plotted on "logit" scale 
#'              or "probability" scale.
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

plot_explanation <- function(explained_model, regr_plot_type = NULL, scale = "logit") {
  trained_model <- mlr::getLearnerModel(explained_model$model)
  present_variables <- colnames(explained_model$data)
  explained_instance <- explained_model$explained_instance[, present_variables]
  
  if(any(grepl("lm", class(trained_model)))) {
    plot_regression(regr_plot_type, trained_model, explained_instance, scale)
  } else {
    plot(trained_model)
  }
}
