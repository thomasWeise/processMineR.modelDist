#' @title A \code{\link{RegressionResult}} Distance Metric based on Plain
#'   Quality
#' @description The distance of two instances \code{a} and \code{b} of
#'   \code{\link{RegressionResult}} is based on how well the models represent
#'   the points of the other's source data.
#' @param a the first instance of \code{\link{RegressionResult}}
#' @param b the second instance of \code{\link{RegressionResult}}
#' @return a distance value in \code{[0,+Inf]}
#' @importClassesFrom regressoR RegressionResult
#' @export RegressionResult.dist.quality
RegressionResult.dist.quality <- function(a, b) {
  # return 0 if a and b are the same
  if(identical(a, b)) {
    return(0);
  }

  # if either a or b is undefined, we return \code{Inf}
  if(is.null(a) || is.null(b) ||
     is.null(a@metric) || is.null(b@metric) ||
     is.null(a@result) || is.null(b@result)) {
    return(+Inf);
  }

  # get the qualities
  q.bb <- b@result@quality;
  q.ab <- b@metric@quality(a@result@f);
  if(is.finite(q.bb) && is.finite(q.ab)) {
    # if model a better represents data b than model b, distance is 0
    if(q.ab <= q.bb) { return(0); }
    q.ab <- q.ab - q.bb;
  } else {
    if(!is.finite(q.bb)) { q.bb <- +Inf; }
  }
  if(!is.finite(q.ab)) { q.ab <- +Inf; }

  q.aa <- a@result@quality;
  q.ba <- a@metric@quality(b@result@f);
  if(is.finite(q.aa) && is.finite(q.ba)) {
    if(q.ba <= q.aa) { return(0); }
    q.ba <- q.ba - q.aa;
  } else {
    if(!is.finite(q.aa)) { q.aa <- +Inf; }
  }
  if(!is.finite(q.ba)) { q.ba <- +Inf; }


  if(is.finite(q.ab)) {
    if(is.finite(q.ba)) {
      # and return the minimum
      return(max(0L, min(q.ab, q.ba)));
    }
    return(q.ab);
  } else {
    if(is.finite(q.ba)) {
      return(q.ba);
    }
  }
  return(+Inf);
}
