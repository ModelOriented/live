#' live: visualizing interpretable models to explain black box models.
#'
#' This package aims to help locally fit and visualize interpretable models similarly to LIME methodology.
#' Interface provided by mlr package is used. Tools are provided to create a simulated dataset of 
#' similar observations, fit a chosen white box models (GLM and CART in particular) and visualize
#' them. 
#'
#' @section Important function:
#' \code{\link{sample_locally}} generates a dataset that will be used for local exploration.
#' \code{\link{add_predictions}} add black box model predictions to simulated dataset.
#' \code{\link{fit_explanation}} fits a chosen white box model to simulated dataset.
#' \code{\link{plot_explanation}} visualizes fitted model.
#'
#' @section Example datasets:
#' \code{wine} Data on wine quality taken from 
#' Modeling wine preferences by data mining from physicochemical properties
#'
#' @docType package
#' @name live
NULL

#' Red wine characteristics and quality.
#'
#' Popular dataset related to wine samples from north Portugal.
#'   
#' @format Data frame with 1599 rows and 12 columns.
#' 
#' @references P. Cortez, A. Cerdeira, F. Almeida, T. Matos and J. Reis. 
#' Modeling wine preferences by data mining from physicochemical properties.
#' In Decision Support Systems, Elsevier, 47(4):547-553, 2009.
"wine"