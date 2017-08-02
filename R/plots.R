#' Plotting white box models.
#' 
#' @param whiteBox object returned by mlr::train function.
#' @param observation Observation around which model was fitted.
#' 
#' @return plot
#' 
#' @export
#' 

plotWhiteBox <- function(whiteBox, observation = NULL) {
  trainedModel <- getLearnerModel(whiteBox)
  if(any(grepl("lm", class(trainedModel)))) {
    srcM <- summary(trainedModel)$coefficients
    varNames <- row.names(srcM)
    src <- as.data.frame(srcM) %>%
      mutate(lower = Estimate - `Std. Error`,
	     upper = Estimate + `Std. Error`,
	     variable = varNames) %>%
    arrange(desc(abs(`t value`))) %>%
    filter(variable != "(Intercept)") %>%
    select(Estimate, lower, upper, variable)
    varNames <- src$variable
    plotVals <- structure(list(mean = c(NA, src$Estimate),
			       lower = c(NA, src$lower),
			       upper = c(NA, src$upper)),
			  .Names = c("mean", "lower", "upper"),
			  row.names = c(NA, -11L),
			  class = "data.frame")
    tableText <- cbind(c("Variable", varNames),
		       c("Observed", round(unlist(observation)[varNames], 2)),
		       c("Estimate", round(src$Estimate, 2)),
		       c("Lower", round(src$lower, 2)),
		       c("Upper", round(src$upper, 2)))
    forestplot(tableText, plotVals, boxsize = 0.4)
  } else {
      plot(trainedModel)
  }
}
