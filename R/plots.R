#' Plotting white box models.
#' 
#' @param white_box object returned by mlr::train function.
#' @param observation Observation around which model was fitted.
#' 
#' @return plot
#' 
#' @export
#' 
#' @examples
#' plotwhite_box()
#' 

plot_white_box <- function(white_box, observation = NULL) {
  trained_model <- mlr::getLearnerModel(white_box)
  if(any(grepl("lm", class(trained_model)))) {
    model_summary <- summary(trained_model)$coefficients %>%
      as.data.frame() %>%
      rownames_to_column() %>%
      rename(vars_names = rowname) %>%
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
                       c("Observed", round(unlist(observation)[model_summary$variable], 2)),
                       c("Estimate", round(model_summary$Estimate, 2)),
                       c("Lower", round(model_summary$lower, 2)),
                       c("Upper", round(model_summary$upper, 2)))
    forestplot::forestplot(plot_text, plot_values, boxsize = 0.4)
  } else {
    plot(trained_model)
  }
}
