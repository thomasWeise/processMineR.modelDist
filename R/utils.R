# deflate a list of Models to a list of RegressionResult where each
# item has an attribute i pointing to the position of its original
# Models in the original list
#' @importFrom regressoR.base model.protect
.deflate.models <- function(models) {
  result <- unname(unlist(lapply(X=1:length(models),
                   FUN=function(i) {
                     lapply(X=models[[i]]@models,
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

# Get the names of the models
.names.models <- function(models) {
  unname(unlist(lapply(X=models, FUN=function(result) result@name),
                       recursive=TRUE))
}
