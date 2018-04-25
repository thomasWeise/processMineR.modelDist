#' @include distUtils.R
#' @include utils.R
#' @include distanceDefault.R

#' @title Create a Distance Matrix Corresponding to the Joined Result of the
#'   Totally Ranked Model Distances
#' @description A distance matrix is constructed which represents the joined
#'   totally ranked distances between the models. We first compute all the
#'   distances between all the models and then rank them. These ranks are then
#'   used instead of the actual distances to build the distance matrix.
#'
#' @param results the \code{\link[processMineR.models]{Models}} records
#' @param distance the
#'   \code{\link[regressoR]{RegressionResult}}-to-\code{\link[regressoR]{RegressionResult}}
#'    distance metric
#' @param join the function used for joining the results created by
#'   \code{distance}
#' @param cores the number of CPU cores to use
#' @return an instance of \code{\link[stats]{dist}}
#' @export Models.dist.totalRank.join
#' @importFrom stats dist
#' @seealso Models.dist.totalRank.mean
#' @seealso Models.dist.totalRank.median
#' @importFrom parallel mclapply
Models.dist.totalRank.join <- function(results, distance=RegressorResult.dist.default, join=mean, cores=1L) {
  n <- length(results);
  n.dist <- (n * (n - 1L)) / 2L;

  # compute how many distance values we will get per element
  sizes <- unlist(lapply(X=seq_len(n-1L), FUN=function(i) vapply(X=(i+1L):n,
                  FUN=function(j,a) a*length(results[[j]]@results),
                  FUN.VALUE=0L, a=length(results[[i]]@results))), recursive=TRUE);
  # allocate the temporary storage
  storage <- lapply(X=1L:n.dist, FUN=function(i) rep(+Inf, sizes[i]));

  # suppress warnings due to possible NaNs somewhere in the computation which will be fixed
  suppressWarnings({
    # get all the results with protected models
    deflated <- .deflate.results(results);
    n.deflated <- length(deflated);
    squared <- ((n.deflated * (n.deflated - 1L)) / 2L);

    dister <- function(index) {
      ij <- dist.ij(index, n.deflated);
      return(distance(deflated[[ij[1]]], deflated[[ij[2]]]));
    }
    if(cores <= 1L) {
      totalDistances <- vapply(X=seq_len(squared), FUN=dister, FUN.VALUE=+Inf);
    } else {
      totalDistances <- unname(unlist(mclapply(X=seq_len(squared), FUN=dister,
                                               mc.cores=cores), recursive = TRUE));
    }
    totalDistances <- rank(totalDistances) / squared;
  })

  # fill the temporary storages with distance values
  for(i in 1L:(n.deflated-1L)) {
    r1 <- deflated[[i]];
    i1 <- .deflated.index(r1);
    for(j in (i+1L):n.deflated) {
      r2 <- deflated[[j]];
      i2 <- .deflated.index(r2);
      if(i1 != i2) {
        # store the result
        index <- dist.index(i1, i2, n);
        si <- sizes[index];
        storage[[index]][si] <- totalDistances[dist.index(i, j, n.deflated)];
        sizes[index] <- (si - 1L);
      }
    }
  }

  # create the distance matrix by computing the distance means
 res <- dist.create(distances=vapply(X=storage, FUN=join, FUN.VALUE=+Inf),
                    names=.names.results(results));

 res <- force(res);
 return(res);
}




#' @title Create a Distance Matrix Corresponding to the Mean of the Model
#'   Distances
#' @description A distance matrix is constructed which represents the mean
#'   distances between the models.
#' @param results the \code{\link[processMineR.models]{Models}} records
#' @param distance the
#'   \code{\link[regressoR]{RegressionResult}}-to-\code{\link[regressoR]{RegressionResult}}
#'    distance metric
#' @param cores the number of CPU cores to use
#' @return an instance of \code{\link[stats]{dist}}
#' @export Models.dist.totalRank.mean
#' @seealso Models.dist.totalRank.join
#' @seealso Models.dist.totalRank.median
Models.dist.totalRank.mean <- function(results, distance=RegressorResult.dist.default, cores=1L)
  Models.dist.totalRank.join(results=results, distance=distance, join=mean, cores=cores)


#' @title Create a Distance Matrix Corresponding to the Median of the Model
#'   Distances
#' @description A distance matrix is constructed which represents the median
#'   distances between the models.
#' @param results the \code{\link[processMineR.models]{Models}} records
#' @param distance the
#'   \code{\link[regressoR]{RegressionResult}}-to-\code{\link[regressoR]{RegressionResult}}
#'    distance metric
#' @param cores the number of CPU cores to use
#' @return an instance of \code{\link[stats]{dist}}
#' @importFrom stats median
#' @export Models.dist.totalRank.median
#' @seealso Models.dist.totalRank.join
#' @seealso Models.dist.totalRank.mean
#' @importClassesFrom processMineR.models Models
Models.dist.totalRank.median <- function(results, distance=RegressorResult.dist.default, cores=1L)
  Models.dist.totalRank.join(results=results, distance=distance, join=median, cores=cores)
