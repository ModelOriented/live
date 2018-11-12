#' @importFrom graphics plot

plot_regression <- function(plot_type, fitted_model, explained_instance, classif, ...) {
  if(plot_type == "forest") {
    forestmodel::forest_model(fitted_model)
  } else {
    if(classif) {
      plot(breakDown::broken(fitted_model, explained_instance, baseline = "Intercept"),
           trans = function(x) exp(x)/(1 + exp(x)), ...)
    } else {
      plot(breakDown::broken(fitted_model, explained_instance, baseline = "Intercept"), ...)
    }
  }
}


#' Plotting white box models.
#'
#' @param x List returned by fit_explanation function.
#' @param type Chr, "forest" or "waterfall" depending
#'        on which type of plot is to be created.
#'        if lm/glm model is used as interpretable approximation.
#' @param ... Additional parameters that will be passed to plot.broken or plot method.
#'
#' @return plot (ggplot2 or base)
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Forest plot for regression
#' plot(fitted_explanation1, type = "forest")
#' # Waterfall plot
#' plot(fitted_explanation1, type = "waterfall")
#' # Plot decision tree
#' plot(fitted_explanation2)
#' }
#'

plot.live_explainer <- function(x, type = "waterfall", ...) {
  trained_model <- mlr::getLearnerModel(x$model)
  present_variables <- colnames(x$data)
  explained_instance <- x$explained_instance[, present_variables]
  classif <- x$model$learner$type == "classif"

  if(any(grepl("lm", class(trained_model)))) {
    plot_regression(type, trained_model, explained_instance, classif, ...)
  } else {
    plot(trained_model, ...)
  }
}
