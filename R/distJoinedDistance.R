#' @include distUtils.R
#' @include utils.R
#' @include distanceDefault.R

#' @title Create a Distance Matrix Corresponding to the Joined Result of the
#'   Model Distances
#' @description A distance matrix is constructed which represents the joined
#'   distances between the Models.
#' @param models the \code{\link[processMineR.models]{Models}} records
#' @param distance the
#'   \code{\link[regressoR]{RegressionResult}}-to-\code{\link[regressoR]{RegressionResult}}
#'    distance metric
#' @param join the function used for joining the models created by
#'   \code{distance}
#' @param cores the number of CPU cores to use (ignored)
#' @return an instance of \code{\link[stats]{dist}}
#' @export Models.dist.join
#' @importFrom stats dist
#' @seealso Models.dist.mean
#' @seealso Models.dist.median
#' @importClassesFrom processMineR.models Models
Models.dist.join <- function(models, distance=RegressionResult.dist.default, join=mean, cores=1L) {
  n <- length(models);
  n.dist <- (n * (n - 1L)) / 2L;

  # compute how many distance values we will get per element
  sizes <- unlist(lapply(X=seq_len(n-1L), FUN=function(i) vapply(X=(i+1L):n,
                  FUN=function(j,a) a*length(models[[j]]@models),
                  FUN.VALUE=0L, a=length(models[[i]]@models))), recursive=TRUE);
  # allocate the temporary storage
  storage <- lapply(X=seq_len(n.dist), FUN=function(i) rep(+Inf, sizes[i]));

  # suppress warnings due to possible NaNs somewhere in the computation which will be fixed
  suppressWarnings({
    # get all the models with protected models
    deflated <- .deflate.models(models);
    n.deflated <- length(deflated);

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
          storage[[index]][si] <- distance(r1, r2);
          sizes[index] <- (si - 1L);
        }
      }
    }

   # compute the distances and normalize them
   distances <- vapply(X=storage, FUN=join, FUN.VALUE=+Inf);
   md <- max(distances);
   if(md > 0) { distances <- distances / md; }

    # create the distance matrix by computing the distance means
   res <- dist.create(distances=distances,
                      names=.names.models(models));
  });

 res <- force(res);
 return(res);
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
#' @seealso Models.dist.join
#' @seealso Models.dist.median
Models.dist.mean <- function(models, distance=RegressionResult.dist.default, cores=1L)
  Models.dist.join(models=models, distance=distance, join=mean, cores=cores)


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
#' @seealso Models.dist.join
#' @seealso Models.dist.mean
Models.dist.median <- function(models, distance=RegressionResult.dist.default, cores=1L)
  Models.dist.join(models=models, distance=distance, join=median, cores=cores)
