#' @include distanceDefault.R

#' @title Create a Distance Matrix Corresponding to the Aggregated Result of the
#'   Ranked Model Distances
#'
#' @description A distance matrix is constructed which represents the aggregated
#'   ranked distances between the models. This should be robust against some odd
#'   outliers.
#'
#' @param models the \code{\link[processMineR.models]{Models}} records
#' @param distance the
#'   \code{\link[regressoR]{RegressionResult}}-to-\code{\link[regressoR]{RegressionResult}}
#'    distance metric
#' @param rank.all the ranking to be applied to all distances
#' @param rank.fromSingle the ranking to be applied to all the distances from
#'   one specific sample to the other samples
#' @param aggregate the function used for joining the models created by
#'   \code{distance}
#' @param cores the number of CPU cores to use
#' @return an instance of \code{\link[stats]{dist}}
#' @export Models.dist.rank.aggregate
#' @importFrom stats dist
#' @seealso Models.dist.rank.mean
#' @seealso Models.dist.rank.median
#' @importClassesFrom processMineR.models Models
#' @importFrom processMineR.models Models.names
#' @importFrom distanceR dist.apply.samples.ranked rank.dist dist.create
#' @include prepare.R
Models.dist.rank.aggregate <- function(models,
                                       distance=RegressionResult.dist.default,
                                       rank.all=rank.dist,
                                       rank.fromSingle=identity,
                                       aggregate=mean, cores=1L) {
  suppressWarnings({
    distances <- dist.apply.samples.ranked(X=.prepare(models),
                                           FUN=distance,
                                           sampler=function(model) model@models,
                                           aggregate=aggregate,
                                           rank.all=rank.all,
                                           rank.fromSingle=rank.fromSingle,
                                           FUN.VALUE=+Inf,
                                           cores=cores);
  })
  distances <- force(distances);
  ret <- dist.create(distances, Models.names(models));
  ret <- force(ret);
  return(ret);
}



#' @title Create a Distance Matrix Corresponding to the Mean Ranks over all
#'   Model Distances
#' @description First we compute the distances between all models not in the
#'   same \code{\link[processMineR.models]{Models}} set. These distances are
#'   then ranked using the normalizing ranking method
#'   \code{\link[distanceR]{rank.dist}}. The distance of one
#'   \code{\link[processMineR.models]{Models}} set to another model set is then
#'   the mean of the corresponding ranks.
#' @param models the \code{\link[processMineR.models]{Models}} records
#' @param distance the
#'   \code{\link[regressoR]{RegressionResult}}-to-\code{\link[regressoR]{RegressionResult}}
#'    distance metric
#' @param cores the number of CPU cores to use
#' @return an instance of \code{\link[stats]{dist}}
#' @export Models.dist.rank.global.mean
#' @seealso Models.dist.rank.aggregate
#' @seealso Models.dist.rank.local.mean
Models.dist.rank.global.mean <- function(models, distance=RegressionResult.dist.default, cores=1L)
  Models.dist.rank.aggregate(models=models, distance=distance,
                             rank.all=rank.dist,
                             rank.fromSingle=identity,
                             aggregate=mean, cores=cores)

#' @title Create a Distance Matrix Corresponding to the Mean Ranks of the
#'   Distances from Each Model to any other Model
#' @description For each model \code{M} we compute the distances to all other
#'   models. These distances are then ranked using the normalizing ranking
#'   method \code{\link[distanceR]{rank.dist}}. The distance of one
#'   \code{\link[processMineR.models]{Models}} set to another model set is then
#'   the mean of the corresponding ranks.
#' @param models the \code{\link[processMineR.models]{Models}} records
#' @param distance the
#'   \code{\link[regressoR]{RegressionResult}}-to-\code{\link[regressoR]{RegressionResult}}
#'    distance metric
#' @param cores the number of CPU cores to use
#' @return an instance of \code{\link[stats]{dist}}
#' @export Models.dist.rank.local.mean
#' @seealso Models.dist.rank.aggregate
#' @seealso Models.dist.rank.global.mean
Models.dist.rank.local.mean <- function(models, distance=RegressionResult.dist.default, cores=1L)
  Models.dist.rank.aggregate(models=models, distance=distance,
                             rank.all=identity,
                             rank.fromSingle=rank.dist,
                             aggregate=mean, cores=cores)
