#' Generate dataset for white box model based on black box model.
#'
#' @param data D.f with variables, from which new dataset will be simulated.
#' @param newData One-row d.f with the same variables as in originalDataset argument,
#'        local exploration will be performed around this observation.
#' @param explainedVar Name of a column with the variable to be predicted.
#' @param blackBox String with mlr signature of a learner or a model
#'        with predict interface.
#' @param size Number of observations is a simulated dataset.
#' @param standardise If TRUE, numerical variables will be scaled to have mean 0, var 1.
#' @param predictionFunction Either a "predict" function that returns a vector of the
#'        same type as response or custom function that takes a model as a first argument,
#'        new data to base predictions on as a second argument and returns a vector
#'        of the same type as respone. Will be used only if a model object was provided
#'        in the blackBox argument.
#' @param ... Additional parameters to be passed to makeRegrTask function.
#'
#' @return list
#' 
#' @export
#'

simulateSimilar <- function(data, newData, explainedVar, blackBox,  
                        size, standardise = FALSE,
                        predictionFunction = predict, ...) {
  colNum <- which(colnames(data) == explainedVar)
  similar <- generateNeighbourhood(data[, -colNum], newData[, -colNum], size)
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
    pred <-  predict(blackTrain, 
		     newdata = similar)
    similar[[explainedVar]] <- pred[["data"]][["response"]]
  } else {
    similar[[explainedVar]] <- predictionFunction(blackBox, 
      newdata = similar)
  }
  if(standardise) {
    similar <- similar %>%
      dplyr::mutate_if(is.numeric, function(x) as.vector(scale(x)))
  }
  list(data = similar, target = explainedVar, blackBoxName = blackBox)
}


#' Fit white box model to the simulated data.
#' 
#' @param liveObject List return by simulateSimilar function. 
#' @param whiteBox String, learner name recognized by mlr package.
#' @param ... Additional arguments passedto makeLearner function.
#'
#' @return mlr object returned by train function.
#' 
#' @export
#' 

trainWhiteBox <- function(liveObject, whiteBox, ...) {
  if(n_distinct(liveObject$data[[liveObject$target]]) == 1) stop("All predicted values were equal.")
  if(grepl("regr", whiteBox)) {
    whiteTask <- mlr::makeRegrTask(id = "whiteTask", data = liveObject$data, target = liveObject$target)
  } else {
    whiteTask <- mlr::makeClassifTask(id = "whiteTask", data = liveObject$data, target = liveObject$target)
  }
  lrn <- mlr::makeLearner(whiteBox, ...)
  mlr::train(lrn, whiteTask)
}   
