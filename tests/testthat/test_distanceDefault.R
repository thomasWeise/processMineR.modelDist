library("processMineR.modelDist")
context("RegressionResult.dist.default")

library(regressoR)


.file.make <- function(dir, name, x, y) {
  con <- file(file.path(dir, name), open="wt");
  text <- unname(unlist(lapply(X=1:length(x), FUN=function(i) paste(c(x[i], y[i]), sep="", collapse="\t"))));
  writeLines(text=text, con=con);
  close(con);
}

.file.make.2 <- function(dir, name, f) {
  x <- sort(unique(c(runif(n=as.integer(runif(n=1, min=1, max=4)), min=1, max=6),
                     runif(n=as.integer(runif(n=1, min=2, max=4)), min=1, max=50))));
  .file.make(dir, name, x, f(x));
}


.make.data <- function() {
  set.seed(1255L);
  dir <- tempfile();
  dir.create(dir, showWarnings=FALSE, recursive=TRUE);

  results <- file.path(dir, "results");
  dir.create(results, showWarnings=FALSE, recursive=TRUE);

  dir.a <- file.path(results, "a");
  dir.create(dir.a, showWarnings=FALSE, recursive=TRUE);
  .file.make.2(dir.a, "1.txt", function(x) 1+2*x);
  .file.make.2(dir.a, "2.txt", function(x) 2+5*x);
  .file.make.2(dir.a, "3.txt", function(x) -4+7*x*x);
  .file.make.2(dir.a, "4.txt", function(x) 2+10*x-x*x);
  .file.make.2(dir.a, "5.txt", function(x) x*x-5);
  .file.make.2(dir.a, "6.txt", function(x) x*x-2*x-1);
  .file.make.2(dir.a, "7.txt", function(x) 7+0.1*x);

  return(c(dir, results));
}


test_that("Test dist.quality ", {
  data <- .make.data();
  output <- regressoR::regressoR.batchLearn(source=data[2],
                                            q=0, learn.single = TRUE, learn.all = FALSE,
                                            returnResults = TRUE,
                                            cores=2L,
                                            logging = FALSE);

  suppressWarnings(
  for(res.1 in output) {
    for(res.2 in output) {
      dist <- RegressionResult.dist.default(res.1, res.2);
      expect_true(is.finite(dist));
      if(identical(res.1, res.2)) {
        expect_identical(dist, 0);
      } else {
        expect_gt(dist, 0);
      }
    }
  })

  unlink(data[1], recursive = TRUE);
})
