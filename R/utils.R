# deflate a list of Models to a list of RegressionResult where each
# item has an attribute i pointing to the position of its original
# Models in the original list
#' @importFrom regressoR.base model.protect
.deflate.results <- function(results) {
  result <- unname(unlist(lapply(X=1:length(results),
                   FUN=function(i) {
                     lapply(X=results[[i]]@results,
                            FUN=function(result, i) {
                              result@result@f <- model.protect(result@result@f,
                                                               result@metric@x,
                                                               result@metric@y);
                              attr(result, "i") <- i;
                              return(result);
                            }, i=i)
                   }), recursive = TRUE));
}

# Get the original index of the deflated result
.deflated.index <- function(result) attr(result, "i")

# Get the names of the results
.names.results <- function(results) {
  unname(unlist(lapply(X=results, FUN=function(result) result@name),
                       recursive=TRUE))
}
