library(aitoaEvaluate);

dir <- getwd();

subdir <- "experiments/jssp/jssp_aitoa/results";

dir.results <- NULL;
repeat {
  dir.results <- file.path(dir, subdir);
  if(dir.exists(dir.results)) {
    break;
  }
  dir.results <- NULL;
  dir <- dirname(dir);
  if((dir == "/") || (nchar(dir) <= 1L)) { break; }
}

stopifnot(!is.null(dir.results), nchar(dir.results) > 0L);

dir.evaluation <- file.path(dir.results, "..", "evaluation");

config <- aitoa.config(dir.results = dir.results,
                       dir.evaluation = dir.evaluation,
                       graphics.ext = "eps")
