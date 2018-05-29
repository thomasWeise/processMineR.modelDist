#' @include aggregatedRankDistance.R

#' @title Create a Distance Matrix of Fitted Models
#'
#' @description A distance matrix is constructed which represents the distances
#'   between the models. For each model. This method represents the default
#'   approach for that, which current is \code{\link{Models.dist.rank.local.mean}}.
#' @param models the \code{\link[processMineR.models]{Models}} records
#' @param distance the
#'   \code{\link[regressoR]{RegressionResult}}-to-\code{\link[regressoR]{RegressionResult}}
#'    distance metric
#' @param cores the number of CPU cores to use
#' @param logging the logging setup, see \code{\link[utilizeR]{makeLogger}}
#' @return an instance of \code{\link[stats]{dist}}
#' @export Models.dist.default
#' @seealso Models.dist.rank.mean
#' @importClassesFrom processMineR.models Models
Models.dist.default <- Models.dist.rank.local.mean
