#' Local permutation variable importance
#' 
#' This function calculates local variable importance (variable drop-out)
#' by finding top_n observations closest to the explained instance,
#' performing permutation variable importance and using weighted mean square
#' error as loss function with weights equal to 1 - Gower distances of the
#' closest observations to the explainedi instance.
#' 
#' @param explained_instance Data frame with one observation for which
#' prediction will be explained
#' @param data Data from with the same columns as explained_instance
#' @param explained_var Character with the names of response variable
#' @param model Model to be explained
#' @param top_n Number of observation that will be used to calculate 
#' local variable importance
#' 
#' @export
#' 
#' @return list of class "local_permutation_importance" that consists of
#' \item{residuals}{Data frame with names of variables in the dataset ("label") and
#' values of drop-out loss ("dropout_loss")}
#' \item{weighted_local_mse}{Value of weighted MSE for the whole dataset with weights
#' given by 1 - Gower distance from the explained instance}
#' \item{explained_instance}{Explained instance as a data frame}
#' 
#' @examples
#' \dontrun{
#' local_permutation_importance(wine[5, ], wine, 
#'                              randomForest(quality~., data = wine),
#'                              top_n = 1000)
#' }
#' 
#' @importFrom stats predict
#' @importFrom dplyr desc
#' 

local_permutation_importance <- function(explained_instance, data,
                                         explained_var, model, 
                                         top_n = nrow(data)) {
  dropout_loss <- NULL
  
  predictors <- data[, -which(colnames(data) == explained_var)]
  top_closest <- gower::gower_topn(explained_instance, predictors, n = top_n)
  indices <- as.numeric(top_closest$index)
  distances <- as.numeric(top_closest$distance)
  data_closest <- predictors[indices, ]
  res <- sapply(colnames(data_closest), function(predictor) {
    permuted <- data_closest
    permuted[, predictor] <- sample(unlist(data_closest[, predictor],
                                           use.names = FALSE), 
                                    size = nrow(permuted))
    predicted <- predict(model, permuted)
    weighted.mean((predicted - data[indices, explained_var])^2, 1 - distances)
  })
  res <- data.frame(label = names(res), dropout_loss = res)
  residuals <- dplyr::arrange(res, desc(dropout_loss))
  full_mse <- weighted.mean((data[, explained_var] - predict(model, data))^2,
                            1 - gower::gower_dist(explained_instance, data))
  result <- list(residuals = residuals, 
                 weighted_local_mse = full_mse,
                 explained_instance = explained_instance)
  class(result) <- c("local_permutation_importance", "list")
  result
}


#' Plot local permutation importance
#' 
#' @param x Object of class local_permutation_importance
#' @param ... Optional arguments, currently ignored
#' 
#' @return ggplot2 object
#' 
#' @import ggplot2
#' @importFrom stats reorder weighted.mean
#' 
#' @export
#' 

plot.local_permutation_importance <- function(x, ...) {
  dropout_loss <- label <- NULL
  x$residuals <- dplyr::mutate(x$residuals, dropout_loss = dropout_loss - x$weighted_local_mse)
  ggplot(x$residuals, 
         aes(reorder(label, x$residuals$dropout_loss, desc = T), 
             ymin = 0, ymax = dropout_loss)) +
    geom_errorbar() + 
    coord_flip() +
    ylab("Weighted drop-out loss") + 
    xlab("")
}


#' Print method for local_permutation_importance class
#' 
#' @param x Object of class local_permutation_importance
#' @param ... Optional arguments, currently ignored
#' 
#' @export
#' 

print.local_permutation_importance <- function(x, ...) {
  max_length = max(nchar(as.character(unlist(x$residuals$label, use.names = FALSE))))
  if(max_length < 8) {
    initial_spaces <- 0
  } else {
    initial_spaces <- max_length - 8
  }
  cat("Variable", paste(rep(" ", times = initial_spaces), sep = "", collapse = ""),  "Drop-out loss", "\n")
  for(i in 1:nrow(x$residuals)) {
    current_label <- as.character(x$residuals[i, "label"])
    n_spaces = max_length - nchar(current_label)
    cat(paste(current_label, paste0(rep(" ", times = n_spaces), sep = "",
                                    collapse = "")), 
        x$residuals[i, "dropout_loss"], "\n")
  }
  invisible(x)
}
