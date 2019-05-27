
# create a logger string from a set of strings
.logger <- function(...) {
  cat(as.character(Sys.time()), ": ", paste(..., sep="", collapse=""), "\n", sep="", collapse="");
  invisible(TRUE);
}

#' @title Generate the Configuration Object to be Passed to Functions of this
#'   Package
#' @description Generate the Configuration Object to be Passed to Functions of
#'   this Package
#' @param dir.results the directory where the results are expected
#' @param dir.evaluation the directory where the evaluation data should be
#'   written to
#' @param logger the logger function
#' @param min.instances the minimum number of instances
#' @param min.runs the minimum number of runs per instance and algorithm
#' @return the configuration object
#' @export aitoa.config
aitoa.config <- function(dir.results="./results",
                         dir.evaluation="./evaluation",
                         logger=.logger,
                         min.instances=4L,
                         min.runs=101L) {

  stopifnot(is.character(dir.results),
            is.character(dir.evaluation),
            is.function(logger),
            is.integer(min.instances),
            is.integer(min.runs),
            min.instances > 0L,
            is.finite(min.instances),
            min.runs > 0L,
            is.finite(min.runs));

  dir.results <- normalizePath(dir.results, mustWork=TRUE);
  stopifnot(dir.exists(dir.results));

  dir.evaluation <- normalizePath(dir.evaluation, mustWork=FALSE);
  stopifnot(dir.results != dir.evaluation);

  config <- list(dir.results = dir.results,
                 dir.evaluation = dir.evaluation,
                 logger=logger,
                 min.instances=min.instances,
                 min.runs=min.runs);

  config <- force(config);
  return(config);
}
