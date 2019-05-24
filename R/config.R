
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
#' @param num.instances the expected number of instances
#' @param num.runs the expected number of runs per instance and algorithm
#' @return the configuration object
#' @export aitoa.config
aitoa.config <- function(dir.results="./results",
                         dir.evaluation="./evaluation",
                         logger=.logger,
                         num.instances=4L,
                         num.runs=101L) {

  stopifnot(is.character(dir.results),
            is.character(dir.evaluation),
            is.function(logger),
            is.integer(num.instances),
            is.integer(num.runs),
            num.instances > 0L,
            is.finite(num.instances),
            num.runs > 0L,
            is.finite(num.runs));

  dir.results <- normalizePath(dir.results, mustWork=TRUE);
  stopifnot(dir.exists(dir.results));

  dir.evaluation <- normalizePath(dir.evaluation, mustWork=FALSE);
  stopifnot(dir.results != dir.evaluation);

  config <- list(dir.results = dir.results,
                 dir.evaluation = dir.evaluation,
                 logger=logger,
                 num.instances=num.instances,
                 num.runs=num.runs);

  config <- force(config);
  return(config);
}
