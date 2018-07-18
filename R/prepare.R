# Protect all the models against NaN or infinite results
#' @importFrom regressoR.base model.protect
#' @importClassesFrom dataManageR dataset
.prepare <- function(models) {
  lapply(X=models, FUN=function(mods) {
    mods@data <- lapply(X=mods@data,
       FUN=function(model) {
         model@result@f <- model.protect(model@result@f,
                                         model@metric@x,
                                         model@metric@y);
         return(model);
       })
    return(mods);
  })
}
