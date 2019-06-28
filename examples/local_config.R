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
rm("dir");
rm("subdir");

stopifnot(!is.null(dir.results), nchar(dir.results) > 0L);
dir.results <- normalizePath(dir.results, mustWork = TRUE);
stopifnot(dir.exists(dir.results));

dir.evaluation <- file.path(dir.results, "..", "evaluation");
dir.evaluation <- normalizePath(dir.evaluation, mustWork = TRUE);
stopifnot(dir.exists(dir.evaluation));

config <- aitoa.config(dir.results = dir.results,
                       dir.evaluation = dir.evaluation,
                       graphics.ext = "eps")

rm("dir.results");
rm("dir.evaluation");
