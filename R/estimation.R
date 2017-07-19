#' Estimate linear regression white box model
#' 
#' @param originalObservation observation (vector) around which local
#'        exploration is being done
#' @param simulatedData d.f returned by generateNeighbourhood function
#' @param explainedVar vector of predictions from black box model
#' @param kernel kernel function used to calculate weights
#' @param sigma parameter used in weights calculation
#'
#' @return lm object
#'   
#' @export 
#'  

estimateWhiteBoxRegLin <- function(originalObservation, simulatedData, explainedVar, 
                             kernel = "gaussian", sigma = 1) {
  forWeightsX <- bind_rows(simulatedData, originalObservation)
  forWeightsY <- as.numeric(c(explainedVar, 1))
  forWeights <- bind_cols(y = forWeightsY, forWeightsX)
  colnames(forWeights)[1] <- "y"
  reg <- lm(y ~ ., data = forWeights)
  modelMatrix <- model.matrix(reg)
  originalObsCoords <- modelMatrix[nrow(modelMatrix), ]
  otherObsCoords <- modelMatrix[1:(nrow(modelMatrix) - 1), ]
  diffs <- apply(otherObsCoords, 1, function(x) sum(x - originalObsCoords)^2)
  weightsWB <- exp(-diffs/sigma)
  forWhiteBox <- bind_cols(y = explainedVar, simulatedData)
  lm(y ~., data = forWhiteBox, weights = weightsWB)
}