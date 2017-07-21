#' Plotting white box models.
#' 
#' @param liveObject Object returned by whiteBoxify function.
#' 
#' @return plot
#' 
#' @export
#' 

plot.live <- function(liveObject) {
  toFormula <- paste(colnames(liveObject@data)[ncol(liveObject@data)], "~", ".")
  if(liveObject@whiteBoxName == "reg") {
    regModel <- lm(as.formula(toFormula), data = liveObject@data)
    src <- summary(regModel)$coefficients
    plotVals <- 
      structure(list(
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
    plot(ctree(as.formula(toFormula), data = liveObject@data))
  }
  # if(grepl("classif", liveObject@blackBoxName)) {
  #   # ROC curve
  # } else {
  #   # ?
  # }
}
