setClass("live", contains = "data.frame", slots = list(whiteBoxName = "character"))

#' Create white box model based on a black box.
#'
#' @param originalDataset D.f with variables, from which new dataset will be simulated.
#' @param observation One-row d.f with the same variables as in originalDataset argument,
#'        local exploration will be performed around this observation.
#' @param explainedVar Name of a column with the variable to be predicted.
#' @param blackBox String with mlr signature of a learner or a model
#'        with predict interface.
#' @param whiteBox String, "reg" for linear regression or "dtree" for decision tree.
#' @param noOfNeighbours Number of similar observations to simulate.
#' @param ... Additional parameters to be passed to makeRegrTask function.
#'
#' @return 
#' 
#' @export
#'

whiteboxify <- function(originalDataset, observation, explainedVar, blackBox, whiteBox,  
                           noOfNeighbours, ...) {
  # if(is.character(blackBox)) {
  #   
  # }
  blackTask <- makeRegrTask(id = "blackTask", data = originalDataset,
                      target = explainedVar, ...)
  lrn <- makeLearner(blackBox)
  blackTrain <- train(lrn, blackTask)
  similar <- generateNeighbourhood(observation, noOfNeighbours, originalDataset)
  similar <- similar %>%
    mutate(yWB = predict(blackTrain, newdata = similar)[["data"]][["response"]])
  colnames(similar)[ncol(similar)] <- explainedVar
  new("live", similar, whiteBoxName = whiteBox)
}
