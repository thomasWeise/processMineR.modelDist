#' @include distanceQuality.R
#' @title The default \code{\link[regressoR]{RegressionResult}} Distance Metric
#' @description The default distance metric for two instances \code{a} and
#'   \code{b}  of \code{\link[regressoR]{RegressionResult}}.
#' @param a the first instance of \code{\link{RegressionResult}}
#' @param b the second instance of \code{\link{RegressionResult}}
#' @return a distance value in \code{[0,+Inf]}
#' @importClassesFrom regressoR RegressionResult
#' @export RegressionResult.dist.default
RegressionResult.dist.default <- RegressionResult.dist.quality
