#' @include distUtils.R
#' @include utils.R
#' @include distanceDefault.R

#' @title Create a Distance Matrix Corresponding to the Joined Result of the
#'   Ranked Model Distances
#'
#' @description A distance matrix is constructed which represents the joined
#'   ranked distances between the models. For each model, we rank all the other
#'   models according to their distance. This should be robust against some odd
#'   outliers.
#'
#' @param results the \code{\link[processMineR.models]{Models}} records
#' @param distance the
#'   \code{\link[regressoR]{RegressionResult}}-to-\code{\link[regressoR]{RegressionResult}}
#'    distance metric
#' @param join the function used for joining the results created by
#'   \code{distance}
#' @param cores the number of CPU cores to use
#' @return an instance of \code{\link[stats]{dist}}
#' @export Models.dist.rank.join
#' @importFrom stats dist
#' @seealso Models.dist.rank.mean
#' @seealso Models.dist.rank.median
#' @importClassesFrom processMineR.models Models
Models.dist.rank.join <- function(results, distance=RegressionResult.dist.default, join=mean, cores=1L) {
  n <- length(results);
  n.dist <- (n * (n - 1L)) / 2L;

  # compute how many distance values we will get per element
  sizes <- unlist(lapply(X=1L:(n-1L), FUN=function(i) vapply(X=(i+1L):n,
                  FUN=function(j,a) 2L*a*length(results[[j]]@results),
                  FUN.VALUE=0L, a=length(results[[i]]@results))), recursive=TRUE);
  # allocate the temporary storage
  storage <- lapply(X=1L:n.dist, FUN=function(i) rep(+Inf, sizes[i]));


  # suppress warnings due to possible NaNs somewhere in the computation which will be fixed
  suppressWarnings({
    # get all the results with protected models
    deflated <- .deflate.results(results);

    # compute the number of distinct distances
    n.deflated <- length(deflated);
    squared <- ((n.deflated * (n.deflated - 1L)) / 2L);

    # the distance function computes the distances between two models identified
    # by their indices
    dst <- function(i, j) distance(deflated[[i]], deflated[[j]])

    # if there are not too many distances, try to cache them
    if(squared < 2000000000L) {
      dister <- function(index) {
        ij <- dist.ij(index, n.deflated);
        return(dst(ij[1], ij[2]));
      }
      if(cores <= 1L) {
        # no parallelization
        totalDistances <- vapply(X=seq_len(squared), FUN=dister, FUN.VALUE=+Inf);
      } else {
        totalDistances <- unname(unlist(mclapply(X=seq_len(squared), FUN=dister,
                                                 mc.cores=cores), recursive = TRUE));
      }

      # ok, we have cached all the distances, so we can now re-write the
      # distance function
      dst <- function(i, j) {
        if(i==j) { 0 }
        else { totalDistances[dist.index(i, j, n.deflated)]; }
      }
    }
    # force the distance function
    dst <- force(dst);

    # fill the temporary storages with distance values
    # lazy: i am doing this in n^2 steps, while I could use half of them,
    # i guess, if i was a bit more intelligent and less tired
    for(i in seq_len(n.deflated)) {
      # we compute the distances to all other objects and then rank them
      # rank and then scale the distances
      computed <- rank(vapply(X=seq_len(n.deflated),
                              FUN=dst, FUN.VALUE = +Inf, j=i)) / n.deflated;

      i1 <- .deflated.index(deflated[[i]]);

      # we then push these ranks into the temporary storages
      for(j in seq_len(n.deflated)) {
        i2 <- .deflated.index(deflated[[j]]);
        if(i1 != i2) {
          # store the result
          index <- dist.index(i1, i2, n);
          si <- sizes[index];
          storage[[index]][si] <- computed[j];
          sizes[index] <- (si - 1L);
        }
      }
    }
  })

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
#' @export Models.dist.rank.mean
#' @seealso Models.dist.rank.join
#' @seealso Models.dist.rank.median
Models.dist.rank.mean <- function(results, distance=RegressionResult.dist.default, cores=1L)
  Models.dist.rank.join(results=results, distance=distance, join=mean, cores=cores)


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
#' @export Models.dist.rank.median
#' @seealso Models.dist.rank.join
#' @seealso Models.dist.rank.mean
Models.dist.rank.median <- function(results, distance=RegressionResult.dist.default, cores=1L)
  Models.dist.rank.join(results=results, distance=distance, join=median, cores=cores)
