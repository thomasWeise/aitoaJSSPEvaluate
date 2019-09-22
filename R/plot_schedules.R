
# plot a single gantt chart
#' @include config.R
#' @importFrom graphics legend
.plot.gantt <- function(instance, config, path, value, m, t.lim=NA_integer_) {
  path <- file.path(config$dir.results, path);
  stopifnot(file.exists(path), file.size(path) > 100L);
  config$logger("loading file ", path);
  data <- trimws(suppressWarnings(readLines(con=path)));
  stopifnot(length(data) > 4L);
  config$logger("finished loading file ", path);
  start <- which(data=="# BEST_Y")[[1L]];
  stopifnot(is.finite(start), is.integer(start), start>0L);
  end <- which(data=="# END_BEST_Y")[[1L]];
  stopifnot(is.finite(end), is.integer(end), end>(start+3L));
  data <- data[(start+1L):(end-1L)];
  stopifnot(length(data) > 1L);
  start <- which(data == "")[[1L]];
  stopifnot(is.finite(start), is.integer(start), start>0L, start<(length(data)-1L));
  data <- data[(start+1L):length(data)];
  stopifnot(length(data) > 0L);
  code <- paste(data, collapse="", sep="");
  rm("data");
  code.1 <- substr(code, start=1L, stop=(nchar(code)-2L));
  stopifnot(nchar(code.1) > 0L);
  code.2 <- substr(code, start=(nchar(code)-1L), stop=nchar(code));
  stopifnot(nchar(code.2) > 0L);
  code.3 <- ', xlab=NA, ylab=NA, prefix.machine="", print.jobs=FALSE';
  if(!is.na(t.lim)) {
    stopifnot(is.finite(t.lim), is.integer(t.lim), t.lim > 1L)
    code.3.l <- nchar(code.3);
    code.3 <- paste0(code.3, ", xlim=c(0L,", t.lim, "L)");
    stopifnot((nchar(code.3)-14L) > code.3.l);
  }
  code <- paste(code.1, code.3, code.2, sep="", collapse="");
  stopifnot(nchar(code) == (nchar(code.1) + nchar(code.2) + nchar(code.3)));
  rm("code.1");
  rm("code.2");
  rm("code.3");

  config$logger("finished extracting code from file ", path);
  eval(as.expression(parse(text=code)));
  rm("code");
  config$logger("code parsed and executed");

  label <- paste0(instance, " / ", value);
  col <- rgb(0.89, 0.89, 0.89, alpha=0.75);

  if(is.na(t.lim)) { t.lim = value; }
  legend(x=0.5*t.lim, y=0.3*m/10,
         xjust=0.5,
         yjust=0.5,
         adj=c(0.1, 0.2),
         cex=1.66,
         legend=label,
         box.col=col,
         box.lwd=0L,
         bg=col);
}

#' @title Plot the Gantt Charts of the Median and Best Results
#' @description Plot the Gantt charts of the median and best solutions.
#' @param config the configuration
#' @return a list of paths to the generated graphics files
#' @include end_results_stats.R
#' @include utils.R
#' @include graphics.R
#' @include get_names.R
#' @importFrom plotteR plots.arrange
#' @importFrom graphics par
#' @export aitoa.plot.gantt.charts
aitoa.plot.gantt.charts <- function(config=aitoa.config()) {
  config$logger("plotting Gantt charts: first loading data");
  frame <- aitoa.end.results.statistics.frame(config);
  stopifnot(is.data.frame(frame),
            nrow(frame) > 0L,
            nrow(frame) >= config$min.instances,
            ncol(frame) > 0L);


  setups <- unique(unname(unlist(frame$algo.id)));
  stopifnot(length(setups) > 0L);
  names <- .get.setup.names(setups, config);
  stopifnot(length(names) == length(setups));

  features <- aitoa.instance.features.frame(config);
  stopifnot(is.data.frame(features),
            nrow(features) > 0L,
            nrow(features) >= config$min.instances,
            ncol(features) > 0L);

  config$logger("data loaded, now picking algorithms");
  chosen <- !is.na(names);
  stopifnot(sum(chosen) > 0L);
  names <- names[chosen];
  stopifnot(length(names) > 0L);
  setups <- setups[chosen];
  stopifnot(length(names) == length(setups));

  config$logger("found ", length(setups), " setups");

  goal.length <- length(setups) * 2L;
  stopifnot(goal.length > 1L);
  paths <- character(goal.length);
  index <- 0L;

  for(i in seq_along(setups)) {
    setup <- setups[[i]];
    stopifnot(!is.null(setup), !is.na(setup));
    name <- names[[i]];
    stopifnot(!is.null(name), !is.na(name));

    frame.setup <- frame[frame$algo.id == setup,];
    stopifnot(nrow(frame.setup) > 0L);

    instances <- sort(unname(unlist(frame.setup$inst.id)));
    length.instances <- length(instances);
    stopifnot(length.instances > 0L, length.instances == length(unique(instances)));
    config$logger("found ", length.instances, " instances for setup ", setup);

    plot.tasks <- list(
      list(prefix="med",
           file=unname(unlist(frame.setup$best.f.med.file)),
           value=unname(unlist(frame.setup$best.f.med))),
      list(prefix="min",
           file=unname(unlist(frame.setup$best.f.min.file)),
           value=unname(unlist(frame.setup$best.f.min)))
    );

    for(plot.task in plot.tasks) {
      .dir <- .dir.plots("gantt", plot.task$prefix, config=config);

      file <- file.path(.dir, .graphics.name(config, paste("jssp_gantt", name,
                                plot.task$prefix, sep="_", collapse="_")));
      index <- index + 1L;
      paths[[index]] <- .graphic(config=config,
                           path=file,
                           width=6L,
                           height=(2.15*length(instances)),
                           expr = {
                             mar <- 0.5*par()$mar;
                             mar[3L] <- 0.25 * mar[3L];
                             mar[1L] <- 0.85 * mar[1L];
                             mar[4L] <- 0.15 * mar[4L];
                             p1 <- par(cex=0.78, mar=mar);
                             p2 <- par(mfrow=c(length(instances), 1L));
                             for(instance in instances) {
                               sel <- (frame.setup$inst.id == instance);
                               stopifnot(sum(sel) == 1L);
                               sel <- which(sel);
                               stopifnot(length(sel) == 1L);
                               m <- (features$inst.id == instance);
                               stopifnot(sum(m) == 1L);
                               m <- which(m);
                               stopifnot(length(m) == 1L);
                               .plot.gantt(instance,
                                           config,
                                           plot.task$file[[sel]],
                                           plot.task$value[[sel]],
                                           features$inst.machines[[m]]);
                             }
                             par(p2);
                             par(p1);
                           });
    }

    rm("frame.setup");
  }

  paths <- force(paths);
  stopifnot(index == length(paths),
            all(!is.na(paths)),
            all(nchar(paths) > 1L),
            all(file.exists(paths)));
  rm("features");
  rm("frame");
  rm("names");
  invisible(gc());
  return(paths);
}
