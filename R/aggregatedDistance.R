#' @include distanceDefault.R

#' @title Create a Distance Matrix Corresponding to the Aggregated Result of the
#'   Model Distances
#' @description A distance matrix is constructed which represents the aggregated
#'   distances between the Models.
#' @param models the \code{\link[processMineR.models]{Models}} records
#' @param distance the
#'   \code{\link[regressoR]{RegressionResult}}-to-\code{\link[regressoR]{RegressionResult}}
#'    distance metric
#' @param aggregate the function used for joining the models created by
#'   \code{distance}
#' @param cores the number of CPU cores to use (ignored)
#' @return an instance of \code{\link[stats]{dist}}
#' @export Models.dist.aggregate
#' @importFrom stats dist
#' @seealso Models.dist.mean
#' @seealso Models.dist.median
#' @importClassesFrom processMineR.models Models
#' @importFrom processMineR.models Models.names
#' @importFrom distanceR dist.apply.samples dist.create
Models.dist.aggregate <- function(models, distance=RegressionResult.dist.default, aggregate=mean, cores=1L) {
  suppressWarnings({
    distances <- dist.apply.samples(X=models, FUN=distance,
                                    sampler=function(model) model@models,
                                    aggregate=aggregate,
                                    FUN.VALUE=+Inf,
                                    cores=cores);
  })
  distances <- force(distances);
  ret <- dist.create(distances, Models.names(models));
  ret <- force(ret);
  return(ret);
}


#' @title Create a Distance Matrix Corresponding to the Mean of the Model
#'   Distances
#' @description A distance matrix is constructed which represents the mean
#'   distances between the Models.
#' @param models the \code{\link[processMineR.models]{Models}} records
#' @param distance the
#'   \code{\link[regressoR]{RegressionResult}}-to-\code{\link[regressoR]{RegressionResult}}
#'    distance metric
#' @param cores the number of CPU cores to use (ignored)
#' @return an instance of \code{\link[stats]{dist}}
#' @export Models.dist.mean
#' @seealso Models.dist.aggregate
#' @seealso Models.dist.median
Models.dist.mean <- function(models, distance=RegressionResult.dist.default, cores=1L)
  Models.dist.aggregate(models=models, distance=distance, aggregate=mean, cores=cores)


#' @title Create a Distance Matrix Corresponding to the Median of the Model
#'   Distances
#' @description A distance matrix is constructed which represents the median
#'   distances between the Models.
#' @param models the \code{\link[processMineR.models]{Models}} records
#' @param distance the
#'   \code{\link[regressoR]{RegressionResult}}-to-\code{\link[regressoR]{RegressionResult}}
#'    distance metric
#' @param cores the number of CPU cores to use (ignored)
#' @return an instance of \code{\link[stats]{dist}}
#' @importFrom stats median
#' @export Models.dist.median
#' @seealso Models.dist.aggregate
#' @seealso Models.dist.mean
Models.dist.median <- function(models, distance=RegressionResult.dist.default, cores=1L)
  Models.dist.aggregate(models=models, distance=distance, aggregate=median, cores=cores)
