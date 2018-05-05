#' live: visualizing interpretable models to explain black box models.
#'
#' This package aims to help locally fit and visualize interpretable models similarly to LIME methodology.
#' Interface provided by mlr package is used. Tools are provided to create a simulated dataset of
#' similar observations, fit a chosen white box models (GLM and CART in particular) and visualize
#' them. The methodology is based on Tulio Ribeiro, Singh, Guestrin (2016) <doi:10.1145/2939672.2939778>.
#' More details can be found in Staniak, Biecek (2018) https://arxiv.org/abs/1804.01955.
#'
#' @section Important functions:
#' \code{\link{sample_locally2}} generates a dataset that will be used for local exploration.
#' \code{\link{add_predictions2}} adds black box model predictions to simulated dataset.
#' \code{\link{fit_explanation2}} fits a chosen white box model to simulated dataset.
#' \code{\link{plot_explanation2}} visualizes fitted model.
#' 
#' Older versions of these function (with the "2" suffix) were kept in this release for
#' consistency with examples from https://arxiv.org/abs/1804.01955.
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
