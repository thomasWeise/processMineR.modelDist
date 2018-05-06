# Protect all the models against NaN or infinite results
#' @importFrom regressoR.base model.protect
.prepare <- function(models) {
  lapply(X=models, FUN=function(mods) {
    mods@models <- lapply(X=mods@models,
       FUN=function(model) {
         model@result@f <- model.protect(model@result@f,
                                         model@metric@x,
                                         model@metric@y);
         return(model);
       })
    return(mods);
  })
}
