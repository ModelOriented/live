setClass("live", contains = "data.frame", 
  slots = list(data = "data.frame", whiteBoxName = "character",
    blackBoxName = "character", regrFamily = "character"))

#' Generate dataset for white box model based on black box model.
#'
#' @param data D.f with variables, from which new dataset will be simulated.
#' @param newData One-row d.f with the same variables as in originalDataset argument,
#'        local exploration will be performed around this observation.
#' @param explainedVar Name of a column with the variable to be predicted.
#' @param blackBox String with mlr signature of a learner or a model
#'        with predict interface.
#' @param whiteBox String, "reg" for linear regression or "dtree" for decision tree.
#' @param noOfNeighbours Number of similar observations to simulate.
#' @param standardise If TRUE, numerical variables will be scaled to have mean 0, var 1.
#' @param regressionFamily Family argument for glm function.
#' @param predictionFunction Either a "predict" function that returns a vector of the
#'        same type as response or custom function that takes a model as a first argument,
#'        new data to base predictions on as a second argument and returns a vector
#'        of the same type as respone. Will be used only if a model object was provided
#'        in the blackBox argument.
#' @param ... Additional parameters to be passed to makeRegrTask function.
#'
#' @return 
#' 
#' @export
#'

simulateSimilar <- function(data, newData, explainedVar, blackBox, whiteBox,  
                        noOfNeighbours, standardise = FALSE,
                        regressionFamily = "gaussian",
                        predictionFunction = predict, ...) {
  similar <- generateNeighbourhood(data, newData, noOfNeighbours)
  if(is.character(blackBox)) {  
    if(grepl("regr", blackBox)) {
      blackTask <- mlr::makeRegrTask(id = "blackTask", data = data,
                                target = explainedVar, ...)  
    } else {
      blackTask <- mlr::makeClassifTask(id = "blackTask", data = data,
                                   target = explainedVar, ...)
    }
    lrn <- mlr::makeLearner(blackBox)
    blackTrain <- mlr::train(lrn, blackTask)
    similar[[explainedVar]] <-  mlr::predict(blackTrain, newdata = similar)[["data"]][["response"]]
  } else {
    similar[[explainedVar]] <- predictionFunction(blackBox, 
      newdata = similar[, -which(colnames(similar == explainedVar))])
  }
  
  if(standardise) {
    similar <- similar %>%
      dplyr::mutate_if(is.numeric, function(x) as.vector(scale(x)))
  }
  new("live", data = similar, whiteBoxName = whiteBox, 
      blackBoxName = blackBox, regrFamily = regressionFamily)
}
