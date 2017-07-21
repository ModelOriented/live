#' Replace one element of a vector with a random element from given d.f
#' 
#' @param chosenVariables numbers of columns from d.f to sample from
#' @param originalDataFrame d.f to sample from
#' @param newDataFrame d.f in which the replacement is done
#' 
#' @return data.frame
#' 

replaceItems <- function(chosenVariables, originalDataFrame, newDataFrame) {
  lapply(1:nrow(newDataFrame), function(x) {
    row <- newDataFrame[x, ]
    row[1, chosenVariables[x]] <- sample(unlist(originalDataFrame[, chosenVariables[x]]), 1)
    row
  })
}

#' Change value of one variable all rows.
#'
#' @param originalDataFrame d.f from which observations will be generated
#'        excluding the explained variable
#' @param newDataFrame d.f created by generateNeighbourhood function
#' @param steps Number of variables to change
#' 
#' @return data.frame
#' 

walkThroughVariables <- function(originalDataFrame, newDataFrame, steps) {
  if(steps == ncol(newDataFrame)) {
    newDataFrame <- replaceItems(1:ncol(originalDataFrame), originalDataFrame, newDataFrame)
  } else { # if(steps < ncol(newDataFrame)) 
    chosenVariables <- sort(sample(ncol(newDataFrame), steps))
    newDataFrame <- replaceItems(chosenVariables, originalDataFrame, newDataFrame)
  }
  bind_rows(newDataFrame)
}


#' LIME: sampling for local exploration
#'
#' @param data d.f from which observations will be generated
#'        excluding the explained variable
#' @param newData a number of row in an original data frame
#' @param noOfNeighbours number of observations to be generated
#'
#' @return data.frame
#'
#' @export
#'

generateNeighbourhood <- function(data, newData, noOfNeighbours) {
  p <- ncol(data)
  newDataFrame <- bind_rows(lapply(1:noOfNeighbours, function(x) newData))
  if(noOfNeighbours == p) {
    newDataFrame <- walkThroughVariables(data, newDataFrame, p)
  } else if(noOfNeighbours < p) {
    newDataFrame <-walkThroughVariables(data, newDataFrame, noOfNeighbours)
  }
  else {
    k = noOfNeighbours %/% p
    r = noOfNeighbours %% p
    separate <- c(rep(1:k, each = p), rep(k+1, r))
    divided <- split(newDataFrame, separate)
    divided[1:k] <- lapply(divided[1:k], function(x)
      walkThroughVariables(data, x, p))
    if(r > 0) {
      divided[[k + 1]] <- walkThroughVariables(data, divided[[k + 1]], r)
    }
    newDataFrame <- bind_rows(divided)
  }
  newDataFrame
}
