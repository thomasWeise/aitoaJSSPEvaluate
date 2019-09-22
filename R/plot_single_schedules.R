
#' @title Plot the Single Gantt Charts of the Median and Best Results
#' @description Plot the Gantt charts of the median and best solutions.
#' @param setup the name of the setup to plot
#' @param instances a list of \code{(instanceName1=maxTime1,
#'   instanceName2=maxTime2, ...)} of instance name and maximum time tuples
#'   (maximum time = \code{NULL} means auto-detect)
#' @param stat the statistic to plot, could be \code{med} or \code{min}
#' @param config the configuration
#' @return a list of paths to the generated graphics files
#' @include end_results_stats.R
#' @include utils.R
#' @include graphics.R
#' @importFrom plotteR plots.arrange
#' @importFrom graphics par
#' @include plot_schedules.R
#' @export aitoa.plot.gantt.charts.single
aitoa.plot.gantt.charts.single <- function(setup,
                                           stat="med",
                                           instances,
                                           config=aitoa.config()) {
  config$logger("plotting Gantt charts: first loading data");
  frame <- aitoa.end.results.statistics.frame(config);
  stopifnot(is.data.frame(frame), nrow(frame) > 0L, ncol(frame) > 0L);

  names <- aitoa.algorithm.parameters.frame(config);
  stopifnot(is.data.frame(names), nrow(names) > 0L, nrow(names) <= nrow(frame));

  setups <- as.character(unname(unlist(names$algo.id)));
  names <- as.character(unname(unlist(names$algo.name)));

  setup.index <- (!is.na(names)) & (names==setup);
  stopifnot(sum(setup.index) == 1L);
  setup.index <- which(setup.index);
  stopifnot(length(setup.index) == 1L);
  setup.name <- setups[[setup.index]];
  stopifnot(nchar(setup.name) > 0L);

  rm("setup.index");
  rm("setups");
  rm("names");

  features <- aitoa.instance.features.frame(config);
  stopifnot(is.data.frame(features), nrow(features) > 0L, ncol(features) > 0L);
  instances.names <- names(instances);
  features.names <- as.character(unname(unlist(features$inst.id)));
  stopifnot(all(vapply(instances.names, function(n) sum(features.names == n), 1L) == 1L));

  config$logger("data loaded, now picking algorithms");
  setups <- as.character(unname(unlist(frame$algo.id)));
  stopifnot(length(setups) > 0L);
  sel <- setups == setup.name;
  stopifnot(sum(sel) >= length(instances));
  frame <- frame[sel, ];
  stopifnot(nrow(frame) >= length(instances));

  stopifnot(is.character(stat));

  metric <- paste0("best.f.", stat);
  metric.file <- paste0(metric, ".file");

  frame.instances <- as.character(unname(unlist(frame$inst.id)));

  .dir <- .dir.plots("gantt", "single", stat, config=config);

  res <- vapply(instances.names, function(n) n, "");

  for(i in seq_along(instances.names)) {
    n <- instances.names[[i]];
    stopifnot(nchar(n) > 0L, is.character(n));
    features.sel <- features.names == n;
    stopifnot(sum(features.sel) == 1L);
    features.sel <- which(features.sel);
    stopifnot(length(features.sel) == 1L);
    m <- features$inst.machines[[features.sel]];

    result.sel <- frame.instances == n;
    stopifnot(sum(result.sel) == 1L);
    result.sel <- which(result.sel);
    stopifnot(length(result.sel) == 1L);

    file <- as.character(unname(unlist(frame[result.sel, metric.file])));
    stopifnot(length(file) == 1L);

    val <- unname(unlist(frame[result.sel, metric]));

    limit <- instances[[n]];
    if(is.null(limit)) {
      limit <- NA_integer_;
      lt <- "";
    } else {
      stopifnot(is.integer(limit), limit>0L);
      lt <- paste0("_", limit);
    }

    path <- file.path(.dir, .graphics.name(config, paste0("jssp_gantt_",
                                           setup, "_", n, "_", stat, lt)));

    res[[n]] <-  .graphic(config=config,
                           path=path,
                           width=5L,
                           height=5*.golden.ratio,
                           expr = {
                             mar <- 0.5*par()$mar;
                             mar[3L] <- 0.25 * mar[3L];
                             mar[1L] <- 0.85 * mar[1L];
                             #mar[4L] <- 0.15 * mar[4L];
                             p1 <- par(cex=0.78, mar=mar);
                             .plot.gantt(n,
                                         config,
                                         file,
                                         val,
                                         m,
                                         t.lim=limit);
                             par(p1);
                           });
  }

  res <- force(res);
  stopifnot(all(!is.na(res)),
            all(nchar(res) > 1L),
            all(file.exists(res)));
  rm("features");
  rm("frame");
  rm("setups");
  invisible(gc());
  return(res);
}
