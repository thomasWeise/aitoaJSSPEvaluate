
# plot a single gantt chart
#' @include config.R
.plot.gantt <- function(instance, config, path, value, m) {
  path <- file.path(config$dir.results, path);
  stopifnot(file.exists(path));
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
  code <- paste(code.1, code.3, code.2, sep="", collapse="");
  stopifnot(nchar(code) == (nchar(code.1) + nchar(code.2) + nchar(code.3)));
  rm("code.1");
  rm("code.2");
  rm("code.3");

  config$logger("finished extracting code: ", code);
  eval(as.expression(parse(text=code)));
  rm("code");
  config$logger("code parsed and executed");

  label <- paste0(instance, " / ", value);
  col <- rgb(0.89, 0.89, 0.89, alpha=0.75);
  legend(x=0.5*value, y=0.3*m/10,
         xjust=0.5,
         yjust=0.5,
         adj=c(0.1, 0.2),
         cex=1.66,
         legend=label,
         box.col=col,
         box.lwd=0L,
         bg=col);
}

#' @include end_results_stats.R
#' @include utils.R
#' @include graphics.R
#' @importFrom plotteR plots.arrange
#' @export aitoa.plot.gantt.charts
aitoa.plot.gantt.charts <- function(config=aitoa.config()) {
  config$logger("plotting Gantt charts: first loading data");
  frame <- aitoa.end.results.statistics.frame(config);
  stopifnot(is.data.frame(frame), nrow(frame) > 0L, ncol(frame) > 0L);

  names <- aitoa.algorithm.parameters.frame(config);
  stopifnot(is.data.frame(names), nrow(names) > 0L, nrow(names) <= nrow(frame));

  features <- aitoa.instance.features.frame(config);
  stopifnot(is.data.frame(features), nrow(features) > 0L, ncol(features) > 0L);

  config$logger("data loaded, now picking algorithms");
  setups <- unique(unname(unlist(frame$algorithm)));
  stopifnot(length(setups) > 0L);
  name.1 <- unname(unlist(names$algo.setup));
  stopifnot(length(name.1) == length(setups),
            length(unique(name.1)) == length(name.1));
  name.1.in.setups <- vapply(name.1, function(nn) which(setups==nn), 1L);
  rm("name.1.in.setups");
  stopifnot(all(name.1.in.setups > 0L), all(name.1.in.setups <= length(setups)));
  setups.in.name.1 <- vapply(setups, function(nn) which(name.1==nn), 1L);
  stopifnot(all(setups.in.name.1 > 0L), all(setups.in.name.1 <= length(name.1)));
  rm("name.1");
  names <- unname(unlist(names$algo.name[setups.in.name.1]));
  rm("setups.in.name.1");
  stopifnot(length(names) == length(setups));
  chosen <- !is.na(names);
  stopifnot(sum(chosen) > 0L);
  names <- names[chosen];
  stopifnot(length(names) > 0L);
  setups <- setups[chosen];
  stopifnot(length(names) == length(setups));

  config$logger("found ", length(setups), " setups");

  for(i in seq_along(setups)) {
    setup <- setups[[i]];
    stopifnot(!is.null(setup), !is.na(setup));
    name <- names[[i]];
    stopifnot(!is.null(name), !is.na(name));

    frame.setup <- frame[frame$algorithm == setup,];
    stopifnot(nrow(frame.setup) > 0L);

    instances <- sort(unname(unlist(frame.setup$instance)));
    length.instances <- length(instances);
    stopifnot(length.instances > 0L, length.instances == length(unique(instances)));
    config$logger("found ", length.instances, " instances for setup ", setup);

    dir.med <- .dir.eval("graphics", "gantt", "med", config=config);

    file <- file.path(dir.med, .graphics.name(paste("jssp_gantt", name, "med", sep="_", collapse="_")));
      .graphic(config=config,
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
                   sel <- frame.setup$instance == instance;
                   stopifnot(sum(sel) == 1L);
                   sel <- which(sel);
                   stopifnot(length(sel) == 1L);
                   m <- features$inst.name == instance;
                   stopifnot(sum(m) == 1L);
                   m <- which(m);
                   stopifnot(length(m) == 1L);
                   .plot.gantt(instance,
                               config,
                               frame.setup$best.f.med.file[[sel]],
                               frame.setup$best.f.med[[sel]],
                               features$inst.machines[[m]]);
                 }
                 par(p2);
                 par(p1);
               });

    rm("frame.setup");
  }

  rm("features");
  rm("frame");
  rm("names");
  gc();
}