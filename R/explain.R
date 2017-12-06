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
#' @examples
#' \dontrun{
#' fitted_explanation <- fit_explanation(local_exploration1, "regr.lm", selection = TRUE)
#' }
#'


fit_explanation <- function(live_object, white_box, selection = FALSE, maximum_depth = 0) {
  if(dplyr::n_distinct(live_object$data[[live_object$target]]) == 1)
    stop("All predicted values were equal.")

  if(selection) {
    explained_var_col <- which(colnames(live_object$data) == live_object$target)
    selected_vars <- bigstep::selectModel(as.data.frame(live_object$data[, -explained_var_col]),
                                          unlist(live_object$data[, explained_var_col]), crit = bigstep::aic)
  } else {
    selected_vars <- colnames(live_object$data)
  }

  mlr_task <- create_task(white_box,
                          live_object$data[, unique(c(selected_vars, live_object$target))],
                          live_object$target)

  if(grepl("ctree", white_box)) {
    lrn <- mlr::makeLearner(white_box, max_depth = maximum_depth)
  } else {
    lrn <- mlr::makeLearner(white_box)
  }

  mlr::train(lrn, mlr_task)
}


#' Draw a forest plot with proper annotations.
#'
#' @param coefficients Matrix of regression coefficients (from summary).
#' @param explained_instance Instance which is being explained (as a row).
#'
#' @return NULL
#'

prepare_forestplot <- function(coefficients, explained_instance) {
  if(is.null(explained_instance)) stop("Explained instance needs to be provided")
  model_summary <- coefficients %>%
    as.data.frame() %>%
    tibble::rownames_to_column(var = "vars_names") %>%
    dplyr::mutate(lower = Estimate - `Std. Error`,
                  upper = Estimate + `Std. Error`,
                  variable = vars_names) %>%
    dplyr::arrange(desc(abs(`t value`))) %>%
    dplyr::filter(variable != "(Intercept)") %>%
    dplyr::select(Estimate, lower, upper, variable)

  plot_values <- structure(list(mean = c(NA, model_summary$Estimate),
                                lower = c(NA, model_summary$lower),
                                upper = c(NA, model_summary$upper)),
                           .Names = c("mean", "lower", "upper"),
                           row.names = c(NA, -(length(model_summary$variable) + 1)),
                           class = "data.frame")
  plot_text <- cbind(c("Variable", model_summary$variable),
                     c("Observed", round(unlist(explained_instance)[model_summary$variable], 2)),
                     c("Estimate", round(model_summary$Estimate, 2)),
                     c("Lower", round(model_summary$lower, 2)),
                     c("Upper", round(model_summary$upper, 2)))

  forestplot::forestplot(plot_text, plot_values, boxsize = 0.4)
}


#' Watefall plot or forestplot for lm/glm explanations.
#'
#' @param plot_type Chr, "forestplot" or "waterfallplot" depending
#'                  on which type of plot is to be created.
#' @param fitted_model glm or lm object.
#' @param explained_instance Observation around which model was fitted.
#'
#'
#' @return plot (ggplot2 or base)
#'
plot_regression <- function(plot_type, fitted_model, explained_instance) {
  if(plot_type == "forestplot") {
    prepare_forestplot(summary(fitted_model)$coefficients, explained_instance)
  } else {
    plot(breakDown::broken(fitted_model, explained_instance))
  }
}


#' Plotting white box models.
#'
#' @param model object returned by mlr::train function.
#' @param regr_plot_type Chr, "forestplot" or "waterfallplot" depending
#'                       on which type of plot is to be created.
#'                       if lm/glm model is used as interpretable approximation.
#' @param explained_instance Observation around which model was fitted.
#'                           Needed only if forest plot/waterfall plot is drawn.
#'
#' @return plot (ggplot2 or base)
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Forest plot for regression
#' plot_explanation(fitted_explanation1, "forestplot", winequality_red[5, ])
#' # Waterfall plot
#' plot_explanation(fitted_explanation1, "waterfallplot", winequality_red[5, ])
#' # Plot decision tree
#' plot_explanation(fitted_explanation2)
#' }
#'

plot_explanation <- function(model, regr_plot_type = NULL, explained_instance = NULL) {
  trained_model <- mlr::getLearnerModel(model)
  if(any(grepl("lm", class(trained_model)))) {
    plot_regression(regr_plot_type, trained_model, explained_instance)
  } else {
    plot(trained_model)
  }
}
