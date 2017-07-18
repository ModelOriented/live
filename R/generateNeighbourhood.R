#' Change value of one variable all rows.
#'
#' @param originalDataFrame d.f from which observations will be generated
#'        excluding the explained variable
#' @param newDataFrame d.f created by generateNeighbourhood function
#' @param steps Number of variables to change

walkThroughVariables <- function(originalDataFrame, newDataFrame, steps) {
  if(steps == ncol(newDataFrame)) {
    diag(newDataFrame) <- sapply(originalDataFrame,
      function(x) x[sample(nrow(originalDataFrame), 1)])
  } else if(steps < ncol(newDataFrame)) {
    chosenVariables <- sample(ncol(newDataFrame), steps)
    colNames <- colnames(newDataFrame)
    newDataFrame <- lapply(1:nrow(newDataFrame), function(x) {
      # row <- unlist(newDataFrame[x, ], use.names = F)
      row <- newDataFrame[x, ]
      row[1, chosenVariables[x]] <- sample(unlist(originalDataFrame[, chosenVariables[x]]), 1)
      row
    })
    # newDataFrame <- as_tibble(t(as.data.frame(newDataFrame)))
    # colnames(newDataFrame) <- colNames
    newDataFrame <- bind_rows(newDataFrame)
  }
  newDataFrame
}


#' LIME: sampling for local exploration
#'
#' @param observation a number of row in an original data frame
#' @param noOfNeighbours number of observations to be generated
#' @param originalDataFrame d.f from which observations will be generated
#'        excluding the explained variable
#'
#' @export
#'

generateNeighbourhood <- function(observation, noOfNeighbours, originalDataFrame) {
  p <- ncol(originalDataFrame)
  newDataFrame <- originalDataFrame[rep(observation, noOfNeighbours), ]
  if(noOfNeighbours == p) {
    newDataFrame <- walkThroughVariables(originalDataFrame, newDataFrame, p)
  } else if(noOfNeighbours < p) {
    newDataFrame <-walkThroughVariables(originalDataFrame, newDataFrame, noOfNeighbours)
  }
  else {
    k = noOfNeighbours %/% p
    r = noOfNeighbours %% p
    separate <- c(rep(1:k, each = p), rep(k+1, r))
    divided <- split(newDataFrame, separate)
    divided[1:k] <- lapply(divided[1:k], function(x)
      walkThroughVariables(originalDataFrame, x, p))
    if(r > 0) {
      divided[[k + 1]] <- walkThroughVariables(originalDataFrame, divided[[k + 1]], r)
    }
    newDataFrame <- bind_rows(divided)
  }
  newDataFrame
}
