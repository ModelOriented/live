#' Plotting white box models.
#' 
#' @param trainedModel glm object or ctree
#' 
#' @return plot
#' 
#' @export
#' 

plotWhiteBox <- function(trainedModel) {
  if(grepl("lm", class(trainedModel))) {
    src <- summary(trainedModel)$coefficients
    plotVals <- structure(list(
      mean  = c(NA, src[, 1]), 
      lower = c(NA, src[, 1] - src[, 2]),
      upper = c(NA, src[, 1] + src[, 2])),
      .Names = c("mean", "lower", "upper"), 
      row.names = c(NA, -11L),
      class = "data.frame")
    
    tableText<-cbind(
      c("Variable", as.character(row.names(src))),
      c("Estimate", round(as.numeric(src[, 1]), 2)),
      c("Lower", round(as.numeric(src[, 1] - src[, 2]), 2)),
      c("Upper", round(as.numeric(src[, 1] + src[, 2]), 2)))
    
    forestplot(tableText,
               plotVals)
  } else {
    plot(trainedModel)
  }
  # if(grepl("classif", liveObject@blackBoxName)) {
  #   # ROC curve
  # } else {
  #   # ?
  # }
}
