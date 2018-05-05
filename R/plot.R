#' Waterfall plot or forestplot for lm/glm explanations.
#'
#' @param plot_type Chr, "forest" or "waterfall" depending
#'                  on which type of plot is to be created.
#' @param fitted_model glm or lm object.
#' @param explained_instance Observation around which model was fitted.
#' @param scale Only for classification problems, "logit" or "probability".
#'
#' @importFrom graphics plot
#'
#' @return plot (ggplot2 or lattice)
#'

plot_regression2 <- function(plot_type, fitted_model, explained_instance, scale = NULL) {
  if(plot_type == "forest") {
    forestmodel::forest_model(fitted_model)
  } else {
    if(scale == "probability") {
      plot(breakDown::broken(fitted_model, explained_instance, baseline = "Intercept"),
           trans = function(x) exp(x)/(1 + exp(x))) +
        ggplot2::scale_y_continuous(limits = c(0, 1), 
                                    name = "probability", 
                                    expand = c(0, 0))
    } else {
      plot(breakDown::broken(fitted_model, explained_instance, baseline = "Intercept"))
    }
  }
}


#' Plotting white box models.
#'
#' @param explained_model List returned by fit_explanation function.
#' @param regr_plot_type Chr, "forest" or "waterfall" depending
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
#' plot_explanation(fitted_explanation1, "forest", wine[5, ])
#' # Waterfall plot
#' plot_explanation(fitted_explanation1, "waterfall", wine[5, ])
#' # Plot decision tree
#' plot_explanation(fitted_explanation2)
#' }
#'

plot_explanation2 <- function(explained_model, regr_plot_type = NULL, scale = "logit") {
  trained_model <- mlr::getLearnerModel(explained_model$model)
  present_variables <- colnames(explained_model$data)
  explained_instance <- explained_model$explained_instance[, present_variables]
  
  if(any(grepl("lm", class(trained_model)))) {
    plot_regression2(regr_plot_type, trained_model, explained_instance, scale)
  } else {
    plot(trained_model)
  }
}
