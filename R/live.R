#' live: visualizing interpretable models to explain black box models.
#'
#' This package aims to help locally fit and visualize interpretable models as in LIME methodology.
#' Interface provided by mlr package is used. Tools are provided to create a simulated dataset of 
#' similar observations, fit a chosen white box models (GLM and CART in particular) and visualize
#' them. 
#'
#' @section Important function:
#' \code{\link{sample_locally}} generates a dataset that will be used for local eploration.
#' \code{\link{fit_explanation}} fits a chosen model to simulated dataset.
#' \code{\link{plot_explanation}} visualizes fitted model.
#'
#' @section Example datasets:
#' \code{wine_quality} Data on wine quality taken from 
#' Modeling wine preferences by data mining from physicochemical properties
#'
#' 
#' @docType package
#' @name live
NULL
