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
    src <- summary(trainedModel)$coefficients
    varNames <- row.names(src)
    src <- as.data.frame(src) %>%
      arrange(desc(abs(`t value`)))
    plotVals <- structure(list(
      mean  = c(NA, src[, 1]), 
      lower = c(NA, src[, 1] - src[, 2]),
      upper = c(NA, src[, 1] + src[, 2])),
      .Names = c("mean", "lower", "upper"), 
      row.names = c(NA, -11L),
      class = "data.frame")
    
    tableText<-cbind(
      c("Variable", varNames),
      c("Observed", unlist(observation, use.names = FALSE)), # Przystosować później do faktorów.
      c("Estimate", round(as.numeric(src[, 1]), 2)),
      c("Lower", round(as.numeric(src[, 1] - src[, 2]), 2)),
      c("Upper", round(as.numeric(src[, 1] + src[, 2]), 2)))
    
    forestplot(tableText, plotVals, boxsize = 0.4)
  } else {
    plot(trainedModel)
  }
}
