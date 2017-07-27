#' Generate dataset for white box model based on black box model.
#'
#' @param data D.f with variables, from which new dataset will be simulated.
#' @param newData One-row d.f with the same variables as in originalDataset argument,
#'        local exploration will be performed around this observation.
#' @param explainedVar Name of a column with the variable to be predicted.
#' @param blackBox String with mlr signature of a learner or a model
#'        with predict interface.
#' @param noOfNeighbours Number of similar observations to simulate.
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
                        noOfNeighbours, standardise = FALSE,
                        predictionFunction = predict, ...) {
  colNum <- which(colnames(data) == explainedVar)
  similar <- generateNeighbourhood(data[, -colNum], newData[, -colNum], noOfNeighbours)
  if(is.character(blackBox)) {  
    if(grepl("regr", blackBox)) {
      blackTask <- mlr::makeRegrTask(id = "blackTask", data = data,
                                target = explainedVar, ...)  
    } else {
      blackTask <- mlr::makeClassifTask(id = "blackTask", data = data,
                                   target = explainedVar, ...)
    }
    if(grepl("regr", blackBox)) {
      lrn <- mlr::makeLearner(blackBox)  
    } else {
      lrn <- mlr::makeLearner(blackBox, predict.type = "prob")
    }
    blackTrain <- mlr::train(lrn, blackTask)
    pred <-  predict(blackTrain, newdata = similar)
    if(grepl("regr", blackBox)) {
      similar[[explainedVar]] <- pred[["data"]][["response"]]
    } else {
      probs <- pred$data
      similar[explainedVar] <- log(probs[, 2]/probs[, 3])
    }
  } else {
    similar[[explainedVar]] <- predictionFunction(blackBox, 
      newdata = similar[, -which(colnames(similar == explainedVar))])
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
#' @param whiteBox String, "reg" for linear regression or "dtree" for decision tree.
#' @param maxDepth Maximum depth of tree, argument passed to ctree().
#'
#' @return lm or party object 
#' 
#' @export
#' 

trainWhiteBox <- function(liveObject, whiteBox, maxDepth = Inf) {
  liveObject$data <- liveObject$data %>%
    select_if(function(x) {dplyr::n_distinct(x) > 1})
  if(n_distinct(liveObject$data[[liveObject$target]]) == 1) stop("All predicted values were equal.")
  liveObject$data <- liveObject$data[is.finite(liveObject$data[[liveObject$target]]), ]
  toFormula <- paste(liveObject$target, "~", ".")
  if(whiteBox == "reg") {
    lm(as.formula(toFormula), data = liveObject$data)
  } else {
    ctree(as.formula(toFormula), data = liveObject$data, maxdepth = maxDepth)
  }
}   
