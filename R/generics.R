#' Generic print function for live explainer
#' 
#' @param x Object created using fit_explanation function
#' @param ... other arguments
#' 
#' @export
#' 

print.live_explainer <- function(x, ...) {
  if(x$selected_variables) {
    selection_present <- "Variable selection was performed"
  } else {
    selection_present <- "Variable selection wasn't performed"
  }
  if(is.null(x$weights)) {
    weights_present <- "Weights not present in the explanation model"
  } else {
    weights_present <- "Weights present in the explanation model"
  }
  model_tmp <- mlr::getLearnerModel(x$model)
  if(class(model_tmp) == "lm") {
    rsq <- summary(model_tmp)$r.squared
    gof <- paste("R-squared:", round(rsq, 4))
  } else {
    gof <- NULL
  }
  
  cat("Dataset: \n",
      "Observations: ", nrow(x$data), "\n",
      "Variables: ", ncol(x$data), "\n",
      "Response variable: ", mlr::getTaskTargetNames(x$model$task.desc), "\n")
  cat("Explanation model: \n",
      "Name: ", mlr::getLearnerId(x$model$learner), "\n",
      selection_present, "\n",
      weights_present, "\n",
      gof)
  invisible(x)
}

#' Generic print function for class live_explorer
#' 
#' @param x Object created by sample_locally function or add_predictions function
#' @param ... Other arguments
#' 
#' @export
#' 

print.live_explorer <- function(x, ...) {
  if(x$target %in% colnames(x$data)) {
    target_message <- "Black box model predictions were added"
    if(class(x$model)[1] == "WrappedModel") {
      model_message <- paste("Model: ", mlr::getLearnerId(x$model$learner))
    } else {
      model_message <- paste("Model: ", class(x$model)[1])
    }
  } else {
    target_message <- "Black box model predictions were not added"
    model_message <- NULL
  }
  cat("Dataset: \n",
      "Observations: ", nrow(x$data), "\n",
      "Variables: ", ncol(x$data), "\n",
      "Response variable: ", x$target, "\n")
  cat("Properties: \n",
      "Sampling method:", x$sampling_method, "\n",
      "Fixed variables", x$fixed_variables, "\n",
      target_message, "\n",
      model_message)
  invisible(x)
}
