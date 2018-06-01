#' Waterfall plot or forestplot for lm/glm explanations.
#'
#' @param plot_type Chr, "forest" or "waterfall" depending
#'                  on which type of plot is to be created.
#' @param fitted_model glm or lm object.
#' @param explained_instance Observation around which model was fitted.
#' @param classif logical, if TRUE, probabilities will be plotted 
#'
#' @importFrom graphics plot
#'
#' @return plot (ggplot2 or lattice)
#'

plot_regression2 <- function(plot_type, fitted_model, explained_instance, classif) {
  if(plot_type == "forest") {
    forestmodel::forest_model(fitted_model)
  } else {
    if(classif) {
      plot(breakDown::broken(fitted_model, explained_instance, baseline = "Intercept"),
           trans = function(x) exp(x)/(1 + exp(x)))
    } else {
      plot(breakDown::broken(fitted_model, explained_instance, baseline = "Intercept"))
    }
  }
}


#' Plotting white box models.
#'
#' @param x List returned by fit_explanation function.
#' @param ... Additional parameters.
#' @param type Chr, "forest" or "waterfall" depending
#'        on which type of plot is to be created.
#'        if lm/glm model is used as interpretable approximation.
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

plot.live_explainer <- function(x, ..., type = NULL) {
  trained_model <- mlr::getLearnerModel(x$model)
  present_variables <- colnames(x$data)
  explained_instance <- x$explained_instance[, present_variables]
  classif <- x$model$learner$type == "classif"

  if(any(grepl("lm", class(trained_model)))) {
    plot_regression2(type, trained_model, explained_instance, classif)
  } else {
    plot(trained_model)
  }
}
