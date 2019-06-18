#' @title Obtain a Data Frame with the End Results Statistics
#' @description We create a data frame with the statistics of the end results
#'   for each algorithm-instance combination.
#' @param config the configuration
#' @return a data frame with the information extracted from the files
#' @include utils.R
#' @include config.R
#' @include setups.R
#' @include load_log_file.R
#' @include end_results.R
#' @include instance_features.R
#' @export aitoa.end.results.statistics.frame
#' @importFrom utils read.csv write.csv
#' @importFrom stats sd median
aitoa.end.results.statistics.frame <- function(config=aitoa.config()) {
  old.options <- options(warn=2);
  file <- file.path(.dir.eval(config=config), "endResultsStats.txt");

  if(!file.exists(file)) {
    config$logger("end results statistics file '", file, "' does not yet exist, so we need to create it.");

    endResults <- aitoa.end.results.frame(config);
    stopifnot(is.data.frame(endResults),
              nrow(endResults) > 0L);

    algorithms.all <- as.character(unname(unlist(endResults$algorithm)));
    stopifnot(length(algorithms.all) > 0L,
              all(is.character(algorithms.all)),
              all(nchar(algorithms.all) > 0L));
    algorithms.unique <- sort(unique(algorithms.all));
    stopifnot(length(algorithms.unique) > 0L,
              (2L*length(algorithms.unique)) <= length(algorithms.all));
    algorithms.indices <- vapply(algorithms.all,
                                 function(a) which(algorithms.unique == a)[[1L]],
                                 NA_integer_);
    stopifnot(all(is.finite(algorithms.indices)),
              all(algorithms.indices > 0L),
              all(algorithms.indices <= length(algorithms.unique)));

    instances.all <- as.character(unname(unlist(endResults$instance)));
    stopifnot(length(instances.all) > 0L,
              all(is.character(instances.all)),
              all(nchar(instances.all) > 0L));
    instances.unique <- sort(unique(instances.all));
    stopifnot(length(instances.unique) > 0L,
              (2L*length(instances.unique)) <= length(instances.all));
    instances.indices <- vapply(instances.all,
                                 function(a) which(instances.unique == a)[[1L]],
                                 NA_integer_);
    stopifnot(all(is.finite(instances.indices)),
              all(instances.indices > 0L),
              all(instances.indices <= length(instances.unique)));

    instances <- aitoa.instance.features.frame(config);
    instances.names <- as.character(unname(unlist(instances$inst.name)));
    o <- order(instances.names);
    stopifnot(identical(instances.names[o], instances.unique));
    instances.opt.bound.lower <- unname(unlist(instances$inst.opt.bound.lower))[o];
    instances.opt.bound.upper <- unname(unlist(instances$inst.opt.bound.upper))[o];
    stopifnot(length(instances.opt.bound.lower) == length(instances.opt.bound.upper),
              length(instances.opt.bound.lower) == length(instances.names),
              all(instances.opt.bound.lower) > 0L,
              all(instances.opt.bound.upper) > 0L,
              all(instances.opt.bound.upper >= instances.opt.bound.lower),
              all(is.integer(instances.opt.bound.lower)),
              all(is.integer(instances.opt.bound.upper)));
    rm("instances");

    n <- as.integer(length(algorithms.unique) * length(instances.unique));
    stopifnot(is.finite(n),
              is.integer(n),
              n > 0L);

    n.runs <- integer(n);

    frame.algorithm <- character(n);
    frame.instance <- character(n);

    total.time.min <- integer(n);
    total.time.mean <- numeric(n);
    total.time.med <- integer(n);
    total.time.max <- integer(n);
    total.time.sd <- numeric(n);

    total.fes.min <- integer(n);
    total.fes.mean <- numeric(n);
    total.fes.med <- integer(n);
    total.fes.max <- integer(n);
    total.fes.sd <- numeric(n);

    best.f.min <- integer(n);
    best.f.min.file <- character(n);
    best.f.mean <- numeric(n);
    best.f.mean.file <- character(n);
    best.f.med <- integer(n);
    best.f.med.file <- character(n);
    best.f.max <- integer(n);
    best.f.max.file <- character(n);
    best.f.sd <- numeric(n);

    best.f.rel.min <- numeric(n);
    best.f.rel.mean <- numeric(n);
    best.f.rel.med <- numeric(n);
    best.f.rel.max <- numeric(n);
    best.f.rel.sd <- numeric(n);

    last.improvement.fes.min <- integer(n);
    last.improvement.fes.mean <- numeric(n);
    last.improvement.fes.med <- integer(n);
    last.improvement.fes.max <- integer(n);
    last.improvement.fes.sd <- numeric(n);

    last.improvement.time.min <- integer(n);
    last.improvement.time.mean <- numeric(n);
    last.improvement.time.med <- integer(n);
    last.improvement.time.max <- integer(n);
    last.improvement.time.sd <- numeric(n);

    ert.opt.bound.lower.time <- numeric(n);
    ert.opt.bound.lower.fes <- numeric(n);
    ert.opt.bound.upper.time <- numeric(n);
    ert.opt.bound.upper.fes <- numeric(n);

    n.opt.bound.lower <- integer(n);
    n.opt.bound.upper <- integer(n);

    endResults.best.f.rel <- rep(NA_real_, nrow(endResults));

# the internal function for computing the ert
    .ert <- function(times, sel, n.reached, n.total, time.max) {
      stopifnot(sum(sel) == n.reached,
                all(is.finite(times[sel])),
                all(times[sel] > 0L),
                all(times[sel] <= time.max),
                is.finite(time.max),
                n.total > 0L,
                n.reached >= 0L,
                n.reached <= n.total,
                n.total == length(sel));

      if(n.reached <= 0L) {
        return(+Inf);
      }
      stopifnot(n.reached > 0L);

      time.sum <- sum(as.numeric(times[sel]));
      time.sum <- force(time.sum);
      stopifnot(time.sum > 0,
                is.finite(time.sum),
                (time.sum / n.reached) > 0,
                (time.sum / n.reached) <= time.max);

      if(n.reached < n.total) {
        time.sum.1 <- time.sum + (as.numeric(n.total - n.reached) * time.max);
        time.sum.1 <- force(time.sum.1);
        stopifnot(time.sum.1 > time.sum,
                  is.finite(time.sum.1));
        time.sum <- time.sum.1;
        time.sum <- force(time.sum);
      }

      stopifnot((time.sum / n.total) > 0,
                (time.sum / n.total) <= time.max);

      time.sum <- time.sum / n.reached;
      time.sum <- force(time.sum);
      stopifnot(time.sum > 0,
                is.finite(time.sum));
      return(time.sum);
    }

# the caches for the algorithm and instance index
    frame.algorithm.index <- integer(n);
    frame.instance.index <- integer(n);

    i <- 0L;
    for(algorithm.i in seq_along(algorithms.unique)) {
      for(instance.i in seq_along(instances.unique)) {
        i <- i + 1L;
        i <- force(i);
        stopifnot(i > 0L, i <= n);

        algorithm <- algorithms.unique[[algorithm.i]];
        instance <- instances.unique[[instance.i]];

        sel <- (algorithms.all == algorithm) &
               (instances.all == instance);
        count <- sum(sel);
        stopifnot(count > 1L);
        sel <- which(sel);
        stopifnot(length(sel) == count);

        frame.algorithm.index[[i]] <- algorithm.i;
        frame.instance.index[[i]] <- instance.i;

        n.runs[[i]] <- count;
        frame.algorithm[[i]] <- algorithm;
        frame.instance[[i]] <- instance;

# computing stats for the total consumed time
        total.time <- unname(unlist(endResults$total.time[sel]));
        stopifnot(is.integer(total.time),
                  length(total.time) == count);

        total.time.min.i <- min(total.time);
        total.time.min.i <- force(total.time.min.i);
        stopifnot(total.time.min.i >= 0,
                  is.finite(total.time.min.i));
        total.time.min.i.int <- as.integer(total.time.min.i);
        total.time.min.i.int <- force(total.time.min.i.int);
        stopifnot(total.time.min.i == total.time.min.i.int,
                  is.finite(total.time.min.i.int),
                  total.time.min.i.int >= 0L);
        total.time.min[[i]] <- total.time.min.i.int;

        total.time.mean.i <- mean(total.time);
        total.time.mean.i <- force(total.time.mean.i);
        stopifnot(total.time.mean.i >= total.time.min.i,
                  is.finite(total.time.mean.i));
        total.time.mean[[i]] <- total.time.mean.i;

        total.time.med.i <- median(total.time);
        total.time.med.i <- force(total.time.med.i);
        stopifnot(total.time.med.i >= total.time.min.i,
                  is.finite(total.time.med.i));
        total.time.med.i.int <- as.integer(total.time.med.i);
        total.time.med.i.int <- force(total.time.med.i.int);
        stopifnot(total.time.med.i == total.time.med.i.int,
                  is.finite(total.time.med.i.int),
                  total.time.med.i.int >= total.time.min.i.int);
        total.time.med[[i]] <- total.time.med.i.int;

        total.time.max.i <- max(total.time);
        total.time.max.i <- force(total.time.max.i);
        stopifnot(total.time.max.i >= total.time.min.i,
                  total.time.max.i >= total.time.med.i,
                  total.time.max.i >= total.time.mean.i,
                  is.finite(total.time.max.i));
        total.time.max.i.int <- as.integer(total.time.max.i);
        total.time.max.i.int <- force(total.time.max.i.int);
        stopifnot(total.time.max.i == total.time.max.i.int,
                  is.finite(total.time.max.i.int),
                  total.time.max.i.int >= total.time.min.i.int,
                  total.time.max.i.int >= total.time.med.i.int,
                  total.time.max.i.int >= total.time.mean.i);
        total.time.max[[i]] <- total.time.max.i.int;

        total.time.sd.i <- sd(total.time);
        total.time.sd.i <- force(total.time.sd.i);
        stopifnot(total.time.sd.i >= 0,
                  is.finite(total.time.sd.i));
        total.time.sd[[i]] <- total.time.sd.i;
        stopifnot(xor((total.time.sd.i <= 0), (total.time.max.i.int > total.time.min.i.int)),
                  xor((total.time.sd.i  > 0), (total.time.max.i == total.time.min.i)));

# now computing stats for the total consumed FEs
        total.fes <- unname(unlist(endResults$total.fes[sel]));
        stopifnot(is.integer(total.fes),
                  length(total.fes) == count);

        total.fes.min.i <- min(total.fes);
        total.fes.min.i <- force(total.fes.min.i);
        stopifnot(total.fes.min.i > 0,
                  is.finite(total.fes.min.i));
        total.fes.min.i.int <- as.integer(total.fes.min.i);
        total.fes.min.i.int <- force(total.fes.min.i.int);
        stopifnot(total.fes.min.i == total.fes.min.i.int,
                  is.finite(total.fes.min.i.int),
                  total.fes.min.i.int > 0L);
        total.fes.min[[i]] <- total.fes.min.i.int;

        total.fes.mean.i <- mean(total.fes);
        total.fes.mean.i <- force(total.fes.mean.i);
        stopifnot(total.fes.mean.i >= total.fes.min.i,
                  is.finite(total.fes.mean.i));
        total.fes.mean[[i]] <- total.fes.mean.i;

        total.fes.med.i <- median(total.fes);
        total.fes.med.i <- force(total.fes.med.i);
        stopifnot(total.fes.med.i >= total.fes.min.i,
                  is.finite(total.fes.med.i));
        total.fes.med.i.int <- as.integer(total.fes.med.i);
        total.fes.med.i.int <- force(total.fes.med.i.int);
        stopifnot(total.fes.med.i == total.fes.med.i.int,
                  is.finite(total.fes.med.i.int),
                  total.fes.med.i.int >= total.fes.min.i.int);
        total.fes.med[[i]] <- total.fes.med.i.int;

        total.fes.max.i <- max(total.fes);
        total.fes.max.i <- force(total.fes.max.i);
        stopifnot(total.fes.max.i >= total.fes.min.i,
                  total.fes.max.i >= total.fes.med.i,
                  total.fes.max.i >= total.fes.mean.i,
                  is.finite(total.fes.max.i));
        total.fes.max.i.int <- as.integer(total.fes.max.i);
        total.fes.max.i.int <- force(total.fes.max.i.int);
        stopifnot(total.fes.max.i == total.fes.max.i.int,
                  is.finite(total.fes.max.i.int),
                  total.fes.max.i.int >= total.fes.min.i.int,
                  total.fes.max.i.int >= total.fes.med.i.int,
                  total.fes.max.i.int >= total.fes.mean.i);
        total.fes.max[[i]] <- total.fes.max.i.int;

        total.fes.sd.i <- sd(total.fes);
        total.fes.sd.i <- force(total.fes.sd.i);
        stopifnot(total.fes.sd.i >= 0,
                  is.finite(total.fes.sd.i));
        total.fes.sd[[i]] <- total.fes.sd.i;
        stopifnot(xor((total.fes.sd.i <= 0), (total.fes.max.i.int > total.fes.min.i.int)),
                  xor((total.fes.sd.i  > 0), (total.fes.max.i == total.fes.min.i)));


# getting the bounds of the objective function
        inst <- (instances.names == instance);
        stopifnot(sum(inst) == 1L);
        opt.bound.lower <- instances.opt.bound.lower[inst];
        stopifnot(length(opt.bound.lower) == 1L,
                  opt.bound.lower > 0L,
                  is.integer(opt.bound.lower));
        opt.bound.upper <- instances.opt.bound.upper[inst];
        stopifnot(length(opt.bound.upper) == 1L,
                  opt.bound.upper > 0L,
                  is.integer(opt.bound.upper),
                  opt.bound.upper >= opt.bound.lower);
        rm("inst");

# computing stats for the final/best result
        best.f <- unname(unlist(endResults$best.f[sel]));
        stopifnot(is.integer(best.f),
                  length(best.f) == count,
                  all(best.f >= opt.bound.lower));

        best.f.min.i <- min(best.f);
        best.f.min.i <- force(best.f.min.i);
        stopifnot(best.f.min.i >= opt.bound.lower,
                  is.finite(best.f.min.i));
        best.f.min.i.int <- as.integer(best.f.min.i);
        best.f.min.i.int <- force(best.f.min.i.int);
        stopifnot(best.f.min.i == best.f.min.i.int,
                  is.finite(best.f.min.i.int),
                  best.f.min.i.int >= opt.bound.lower);
        best.f.min[[i]] <- best.f.min.i.int;

        best.f.mean.i <- mean(best.f);
        best.f.mean.i <- force(best.f.mean.i);
        stopifnot(best.f.mean.i >= best.f.min.i,
                  is.finite(best.f.mean.i));
        best.f.mean[[i]] <- best.f.mean.i;

        best.f.med.i <- median(best.f);
        best.f.med.i <- force(best.f.med.i);
        stopifnot(best.f.med.i >= best.f.min.i,
                  is.finite(best.f.med.i));
        best.f.med.i.int <- as.integer(best.f.med.i);
        best.f.med.i.int <- force(best.f.med.i.int);
        stopifnot(best.f.med.i == best.f.med.i.int,
                  is.finite(best.f.med.i.int),
                  best.f.med.i.int >= best.f.min.i.int);
        best.f.med[[i]] <- best.f.med.i.int;

        best.f.max.i <- max(best.f);
        best.f.max.i <- force(best.f.max.i);
        stopifnot(best.f.max.i >= best.f.min.i,
                  best.f.max.i >= best.f.med.i,
                  best.f.max.i >= best.f.mean.i,
                  is.finite(best.f.max.i));
        best.f.max.i.int <- as.integer(best.f.max.i);
        best.f.max.i.int <- force(best.f.max.i.int);
        stopifnot(best.f.max.i == best.f.max.i.int,
                  is.finite(best.f.max.i.int),
                  best.f.max.i.int >= best.f.min.i.int,
                  best.f.max.i.int >= best.f.med.i.int,
                  best.f.max.i.int >= best.f.mean.i);
        best.f.max[[i]] <- best.f.max.i.int;

        best.f.sd.i <- sd(best.f);
        best.f.sd.i <- force(best.f.sd.i);
        stopifnot(best.f.sd.i >= 0,
                  is.finite(best.f.sd.i));
        best.f.sd[[i]] <- best.f.sd.i;
        stopifnot(xor((best.f.sd.i <= 0), (best.f.max.i.int > best.f.min.i.int)),
                  xor((best.f.sd.i  > 0), (best.f.max.i == best.f.min.i)));

# picking the files
        files.sel <- endResults$file[sel];
        files.sel <- force(files.sel);
        if(is.factor(files.sel)) {
          files.sel <- as.character(files.sel);
        }
        files.sel <- force(files.sel);
        stopifnot(is.character(files.sel),
                  length(files.sel) == count);

        best.f.min.dist <- best.f - best.f.min.i;
        stopifnot(all(best.f.min.dist >= 0L));
        best.f.min.file.i <- files.sel[best.f.min.dist <= min(best.f.min.dist)];
        best.f.min.file.i <- force(best.f.min.file.i);
        stopifnot(length(best.f.min.file.i) >= 1L);
        best.f.min.file.i <- best.f.min.file.i[[1L]];
        best.f.min.file.i <- force(best.f.min.file.i);
        best.f.min.file[[i]] <- best.f.min.file.i;

        best.f.mean.dist <- abs(best.f - best.f.mean.i);
        stopifnot(all(best.f.mean.dist >= 0L));
        best.f.mean.file.i <- files.sel[best.f.mean.dist <= min(best.f.mean.dist)];
        best.f.mean.file.i <- force(best.f.mean.file.i);
        stopifnot(length(best.f.mean.file.i) >= 1L);
        best.f.mean.file.i <- best.f.mean.file.i[[1L]];
        best.f.mean.file.i <- force(best.f.mean.file.i);
        best.f.mean.file[[i]] <- best.f.mean.file.i;

        best.f.med.dist <- abs(best.f - best.f.med.i);
        stopifnot(all(best.f.med.dist >= 0L));
        best.f.med.file.i <- files.sel[best.f.med.dist <= min(best.f.med.dist)];
        best.f.med.file.i <- force(best.f.med.file.i);
        stopifnot(length(best.f.med.file.i) >= 1L);
        best.f.med.file.i <- best.f.med.file.i[[1L]];
        best.f.med.file.i <- force(best.f.med.file.i);
        best.f.med.file[[i]] <- best.f.med.file.i;

        best.f.max.dist <- best.f.max.i - best.f;
        stopifnot(all(best.f.max.dist >= 0L));
        best.f.max.file.i <- files.sel[best.f.max.dist <= min(best.f.max.dist)];
        best.f.max.file.i <- force(best.f.max.file.i);
        stopifnot(length(best.f.max.file.i) >= 1L);
        best.f.max.file.i <- best.f.max.file.i[[1L]];
        best.f.max.file.i <- force(best.f.max.file.i);
        best.f.max.file[[i]] <- best.f.max.file.i;



        # computing stats for the final/best result
        best.f.rel <- (best.f - opt.bound.lower) / opt.bound.lower;
        stopifnot(all(best.f.rel >= 0),
                  all(is.finite(best.f.rel)));
        endResults.best.f.rel[sel] <- best.f.rel;

        best.f.rel.min.i <- min(best.f.rel);
        best.f.rel.min.i <- force(best.f.rel.min.i);
        stopifnot(best.f.rel.min.i >= 0,
                  is.finite(best.f.rel.min.i));
        best.f.rel.min[[i]] <- best.f.rel.min.i;

        best.f.rel.mean.i <- mean(best.f.rel);
        best.f.rel.mean.i <- force(best.f.rel.mean.i);
        stopifnot(best.f.rel.mean.i >= best.f.rel.min.i,
                  is.finite(best.f.rel.mean.i));
        best.f.rel.mean[[i]] <- best.f.rel.mean.i;

        best.f.rel.med.i <- median(best.f.rel);
        best.f.rel.med.i <- force(best.f.rel.med.i);
        stopifnot(best.f.rel.med.i >= best.f.rel.min.i,
                  is.finite(best.f.rel.med.i));
        best.f.rel.med[[i]] <- best.f.rel.med.i;

        best.f.rel.max.i <- max(best.f.rel);
        best.f.rel.max.i <- force(best.f.rel.max.i);
        stopifnot(best.f.rel.max.i >= best.f.rel.min.i,
                  best.f.rel.max.i >= best.f.rel.med.i,
                  best.f.rel.max.i >= best.f.rel.mean.i,
                  is.finite(best.f.rel.max.i));
        best.f.rel.max[[i]] <- best.f.rel.max.i;

        best.f.rel.sd.i <- sd(best.f.rel);
        best.f.rel.sd.i <- force(best.f.rel.sd.i);
        stopifnot(best.f.rel.sd.i >= 0,
                  is.finite(best.f.rel.sd.i));
        best.f.rel.sd[[i]] <- best.f.rel.sd.i;
        stopifnot(xor((best.f.rel.sd.i <= 0), (best.f.rel.max.i > best.f.rel.min.i)),
                  xor((best.f.rel.sd.i  > 0), (best.f.rel.max.i == best.f.rel.min.i)),
                  (best.f.rel.sd.i == 0) == (best.f.sd.i == 0));


# now computing stats for the last improvement time
        last.improvement.time <- unname(unlist(endResults$last.improvement.time[sel]));
        stopifnot(is.integer(last.improvement.time),
                  length(last.improvement.time) == count);

        last.improvement.time.min.i <- min(last.improvement.time);
        last.improvement.time.min.i <- force(last.improvement.time.min.i);
        stopifnot(last.improvement.time.min.i >= 0,
                  is.finite(last.improvement.time.min.i));
        last.improvement.time.min.i.int <- as.integer(last.improvement.time.min.i);
        last.improvement.time.min.i.int <- force(last.improvement.time.min.i.int);
        stopifnot(last.improvement.time.min.i == last.improvement.time.min.i.int,
                  is.finite(last.improvement.time.min.i.int),
                  last.improvement.time.min.i.int >= 0L);
        last.improvement.time.min[[i]] <- last.improvement.time.min.i.int;

        last.improvement.time.mean.i <- mean(last.improvement.time);
        last.improvement.time.mean.i <- force(last.improvement.time.mean.i);
        stopifnot(last.improvement.time.mean.i >= last.improvement.time.min.i,
                  is.finite(last.improvement.time.mean.i));
        last.improvement.time.mean[[i]] <- last.improvement.time.mean.i;

        last.improvement.time.med.i <- median(last.improvement.time);
        last.improvement.time.med.i <- force(last.improvement.time.med.i);
        stopifnot(last.improvement.time.med.i >= last.improvement.time.min.i,
                  is.finite(last.improvement.time.med.i));
        last.improvement.time.med.i.int <- as.integer(last.improvement.time.med.i);
        last.improvement.time.med.i.int <- force(last.improvement.time.med.i.int);
        stopifnot(last.improvement.time.med.i == last.improvement.time.med.i.int,
                  is.finite(last.improvement.time.med.i.int),
                  last.improvement.time.med.i.int >= last.improvement.time.min.i.int);
        last.improvement.time.med[[i]] <- last.improvement.time.med.i.int;

        last.improvement.time.max.i <- max(last.improvement.time);
        last.improvement.time.max.i <- force(last.improvement.time.max.i);
        stopifnot(last.improvement.time.max.i >= last.improvement.time.min.i,
                  last.improvement.time.max.i >= last.improvement.time.med.i,
                  last.improvement.time.max.i >= last.improvement.time.mean.i,
                  is.finite(last.improvement.time.max.i));
        last.improvement.time.max.i.int <- as.integer(last.improvement.time.max.i);
        last.improvement.time.max.i.int <- force(last.improvement.time.max.i.int);
        stopifnot(last.improvement.time.max.i == last.improvement.time.max.i.int,
                  is.finite(last.improvement.time.max.i.int),
                  last.improvement.time.max.i.int >= last.improvement.time.min.i.int,
                  last.improvement.time.max.i.int >= last.improvement.time.med.i.int,
                  last.improvement.time.max.i.int >= last.improvement.time.mean.i);
        last.improvement.time.max[[i]] <- last.improvement.time.max.i.int;

        last.improvement.time.sd.i <- sd(last.improvement.time);
        last.improvement.time.sd.i <- force(last.improvement.time.sd.i);
        stopifnot(last.improvement.time.sd.i >= 0,
                  is.finite(last.improvement.time.sd.i));
        last.improvement.time.sd[[i]] <- last.improvement.time.sd.i;
        stopifnot(xor((last.improvement.time.sd.i <= 0), (last.improvement.time.max.i.int > last.improvement.time.min.i.int)),
                  xor((last.improvement.time.sd.i  > 0), (last.improvement.time.max.i == last.improvement.time.min.i)));

# now computing stats for the last improvement FEs
        last.improvement.fes <- unname(unlist(endResults$last.improvement.fes[sel]));
        stopifnot(is.integer(last.improvement.fes),
                  length(last.improvement.fes) == count);

        last.improvement.fes.min.i <- min(last.improvement.fes);
        last.improvement.fes.min.i <- force(last.improvement.fes.min.i);
        stopifnot(last.improvement.fes.min.i > 0,
                  is.finite(last.improvement.fes.min.i));
        last.improvement.fes.min.i.int <- as.integer(last.improvement.fes.min.i);
        last.improvement.fes.min.i.int <- force(last.improvement.fes.min.i.int);
        stopifnot(last.improvement.fes.min.i == last.improvement.fes.min.i.int,
                  is.finite(last.improvement.fes.min.i.int),
                  last.improvement.fes.min.i.int > 0L);
        last.improvement.fes.min[[i]] <- last.improvement.fes.min.i.int;

        last.improvement.fes.mean.i <- mean(last.improvement.fes);
        last.improvement.fes.mean.i <- force(last.improvement.fes.mean.i);
        stopifnot(last.improvement.fes.mean.i >= last.improvement.fes.min.i,
                  is.finite(last.improvement.fes.mean.i));
        last.improvement.fes.mean[[i]] <- last.improvement.fes.mean.i;

        last.improvement.fes.med.i <- median(last.improvement.fes);
        last.improvement.fes.med.i <- force(last.improvement.fes.med.i);
        stopifnot(last.improvement.fes.med.i >= last.improvement.fes.min.i,
                  is.finite(last.improvement.fes.med.i));
        last.improvement.fes.med.i.int <- as.integer(last.improvement.fes.med.i);
        last.improvement.fes.med.i.int <- force(last.improvement.fes.med.i.int);
        stopifnot(last.improvement.fes.med.i == last.improvement.fes.med.i.int,
                  is.finite(last.improvement.fes.med.i.int),
                  last.improvement.fes.med.i.int >= last.improvement.fes.min.i.int);
        last.improvement.fes.med[[i]] <- last.improvement.fes.med.i.int;

        last.improvement.fes.max.i <- max(last.improvement.fes);
        last.improvement.fes.max.i <- force(last.improvement.fes.max.i);
        stopifnot(last.improvement.fes.max.i >= last.improvement.fes.min.i,
                  last.improvement.fes.max.i >= last.improvement.fes.med.i,
                  last.improvement.fes.max.i >= last.improvement.fes.mean.i,
                  is.finite(last.improvement.fes.max.i));
        last.improvement.fes.max.i.int <- as.integer(last.improvement.fes.max.i);
        last.improvement.fes.max.i.int <- force(last.improvement.fes.max.i.int);
        stopifnot(last.improvement.fes.max.i == last.improvement.fes.max.i.int,
                  is.finite(last.improvement.fes.max.i.int),
                  last.improvement.fes.max.i.int >= last.improvement.fes.min.i.int,
                  last.improvement.fes.max.i.int >= last.improvement.fes.med.i.int,
                  last.improvement.fes.max.i.int >= last.improvement.fes.mean.i);
        last.improvement.fes.max[[i]] <- last.improvement.fes.max.i.int;

        last.improvement.fes.sd.i <- sd(last.improvement.fes);
        last.improvement.fes.sd.i <- force(last.improvement.fes.sd.i);
        stopifnot(last.improvement.fes.sd.i >= 0,
                  is.finite(last.improvement.fes.sd.i));
        last.improvement.fes.sd[[i]] <- last.improvement.fes.sd.i;
        stopifnot(xor((last.improvement.fes.sd.i <= 0), (last.improvement.fes.max.i.int > last.improvement.fes.min.i.int)),
                  xor((last.improvement.fes.sd.i  > 0), (last.improvement.fes.max.i == last.improvement.fes.min.i)));

# now compute opt bound upper
        sel.opt.bound.upper <- (opt.bound.upper >= best.f);
        stopifnot(length(sel.opt.bound.upper) == count);

        n.opt.bound.upper.i <- as.integer(sum(sel.opt.bound.upper));
        n.opt.bound.upper.i <- force(n.opt.bound.upper.i);
        stopifnot(n.opt.bound.upper.i >= 0L,
                  n.opt.bound.upper.i <= count,
                  xor(best.f.min.i.int > opt.bound.upper, n.opt.bound.upper.i > 0L),
                  all(sel.opt.bound.upper == endResults$reached.opt.bound.upper[sel]),
                  n.opt.bound.upper.i == sum(endResults$reached.opt.bound.upper[sel]));
        n.opt.bound.upper[[i]] <- n.opt.bound.upper.i;

        if(n.opt.bound.upper.i > 0L) {
          reached.opt.bound.upper.time.ii <- endResults$reached.opt.bound.upper.time[sel];
          stopifnot(is.integer(reached.opt.bound.upper.time.ii),
                    length(reached.opt.bound.upper.time.ii) == count);

          reached.opt.bound.upper.time.ii <- force(reached.opt.bound.upper.time.ii);
          ert.opt.bound.upper.time.i <- .ert(reached.opt.bound.upper.time.ii,
                                             sel.opt.bound.upper,
                                             n.opt.bound.upper.i,
                                             count,
                                             total.time.max.i);
          ert.opt.bound.upper.time.i <- force(ert.opt.bound.upper.time.i);
          stopifnot(ert.opt.bound.upper.time.i > 0L,
                    is.finite(ert.opt.bound.upper.time.i),
                    ert.opt.bound.upper.time.i >= min(reached.opt.bound.upper.time.ii, na.rm=TRUE));
          ert.opt.bound.upper.time[[i]] <- ert.opt.bound.upper.time.i;

          reached.opt.bound.upper.fes.ii <- endResults$reached.opt.bound.upper.fes[sel];
          reached.opt.bound.upper.fes.ii <- force(reached.opt.bound.upper.fes.ii);
          stopifnot(is.integer(reached.opt.bound.upper.fes.ii),
                    length(reached.opt.bound.upper.fes.ii) == count);
          ert.opt.bound.upper.fes.i <- .ert(reached.opt.bound.upper.fes.ii,
                                            sel.opt.bound.upper,
                                            n.opt.bound.upper.i,
                                            count,
                                            total.fes.max.i);
          ert.opt.bound.upper.fes.i <- force(ert.opt.bound.upper.fes.i);
          stopifnot(ert.opt.bound.upper.fes.i > 0L,
                    is.finite(ert.opt.bound.upper.fes.i),
                    ert.opt.bound.upper.fes.i >= min(reached.opt.bound.upper.fes.ii, na.rm=TRUE));
          ert.opt.bound.upper.fes[[i]] <- ert.opt.bound.upper.fes.i;

# now compute opt bound lower
          sel.opt.bound.lower <- (opt.bound.lower >= best.f);
          n.opt.bound.lower.i <- as.integer(sum(sel.opt.bound.lower));
          n.opt.bound.lower.i <- force(n.opt.bound.lower.i);
          stopifnot(n.opt.bound.lower.i >= 0L,
                    xor(best.f.min.i.int > opt.bound.lower, n.opt.bound.lower.i > 0L),
                    n.opt.bound.lower.i <= n.opt.bound.upper.i,
                    all(sel.opt.bound.lower == endResults$reached.opt.bound.lower[sel]),
                    n.opt.bound.lower.i == sum(endResults$reached.opt.bound.lower[sel]));
          n.opt.bound.lower[[i]] <- n.opt.bound.lower.i;

          if(n.opt.bound.lower.i > 0L) {
            ert.opt.bound.lower.time.i <- .ert(last.improvement.time,
                                               sel.opt.bound.lower,
                                               n.opt.bound.lower.i,
                                               count,
                                               total.time.max.i);
            ert.opt.bound.lower.time.i <- force(ert.opt.bound.lower.time.i);
            stopifnot(ert.opt.bound.lower.time.i > 0L,
                      is.finite(ert.opt.bound.lower.time.i),
                      ert.opt.bound.lower.time.i >= min(last.improvement.time));
            ert.opt.bound.lower.time[[i]] <- ert.opt.bound.lower.time.i;

            ert.opt.bound.lower.fes.i <- .ert(last.improvement.fes,
                                              sel.opt.bound.lower,
                                              n.opt.bound.lower.i,
                                              count,
                                              total.fes.max.i);
            ert.opt.bound.lower.fes.i <- force(ert.opt.bound.lower.fes.i);
            stopifnot(ert.opt.bound.lower.fes.i > 0L,
                      is.finite(ert.opt.bound.lower.fes.i),
                      ert.opt.bound.lower.fes.i >= min(last.improvement.fes));
            ert.opt.bound.lower.fes[[i]] <- ert.opt.bound.lower.fes.i;
          } else {
            ert.opt.bound.lower.time[[i]] <- +Inf;
            ert.opt.bound.lower.fes[[i]] <- +Inf;
          }
        } else {
          ert.opt.bound.upper.time[[i]] <- +Inf;
          ert.opt.bound.upper.fes[[i]] <- +Inf;
          n.opt.bound.lower[[i]] <- 0L;
          ert.opt.bound.lower.time[[i]] <- +Inf;
          ert.opt.bound.lower.fes[[i]] <- +Inf;
          n.opt.bound.upper[[i]] <- 0L;
        }

        stopifnot(n.opt.bound.lower[[i]] <= n.opt.bound.upper[[i]],
                  ert.opt.bound.upper.time[[i]] <= ert.opt.bound.lower.time[[i]],
                  ert.opt.bound.upper.fes[[i]] <= ert.opt.bound.lower.fes[[i]]);
      }
    }

    stopifnot(i == n);

# try to convert to integers
    total.time.mean <- .try.convert.to.int(total.time.mean);
    total.time.sd <- .try.convert.to.int(total.time.sd);
    total.fes.mean <- .try.convert.to.int(total.fes.mean);
    total.fes.sd <- .try.convert.to.int(total.fes.sd);
    best.f.mean <- .try.convert.to.int(best.f.mean);
    best.f.sd <- .try.convert.to.int(best.f.sd);
    last.improvement.time.mean <- .try.convert.to.int(last.improvement.time.mean);
    last.improvement.time.sd <- .try.convert.to.int(last.improvement.time.sd);
    last.improvement.fes.mean <- .try.convert.to.int(last.improvement.fes.mean);
    last.improvement.fes.sd <- .try.convert.to.int(last.improvement.fes.sd);
    ert.opt.bound.lower.time <- .try.convert.to.int(ert.opt.bound.lower.time);
    ert.opt.bound.lower.fes <- .try.convert.to.int(ert.opt.bound.lower.fes);
    ert.opt.bound.upper.time <- .try.convert.to.int(ert.opt.bound.upper.time);
    ert.opt.bound.upper.fes <- .try.convert.to.int(ert.opt.bound.upper.fes);
# done creating and checking the slim chance of integer conversion

# now running final sanity tests
    stopifnot(length(unique(n.runs)) == 1L,
              length(n.runs) == n,
              n.runs[[1L]] > 2L,
              #
              min(total.time.min) >= 0L,
              min(total.time.mean) >= 0L,
              min(total.time.med) >= 0L,
              min(total.time.max) >= 0L,
              all(total.time.max >= total.time.min),
              all(total.time.max >= total.time.mean),
              all(total.time.max >= total.time.med),
              all(total.time.mean >= total.time.min),
              all(total.time.med >= total.time.min),
              all(total.time.sd >= 0),
              all(xor(total.time.sd == 0, total.time.max > total.time.min)),
              #
              min(total.fes.min) > 0L,
              min(total.fes.mean) > 0L,
              min(total.fes.med) > 0L,
              min(total.fes.max) > 0L,
              all(total.fes.max >= total.fes.min),
              all(total.fes.max >= total.fes.mean),
              all(total.fes.max >= total.fes.med),
              all(total.fes.mean >= total.fes.min),
              all(total.fes.med >= total.fes.min),
              all(total.fes.sd >= 0),
              all(xor(total.fes.sd == 0, total.fes.max > total.fes.min)),
              #
              min(last.improvement.time.min) >= 0L,
              min(last.improvement.time.mean) >= 0L,
              min(last.improvement.time.med) >= 0L,
              min(last.improvement.time.max) >= 0L,
              all(last.improvement.time.max >= last.improvement.time.min),
              all(last.improvement.time.max >= last.improvement.time.mean),
              all(last.improvement.time.max >= last.improvement.time.med),
              all(last.improvement.time.mean >= last.improvement.time.min),
              all(last.improvement.time.med >= last.improvement.time.min),
              all(last.improvement.time.max <= total.time.max),
              all(last.improvement.time.min <= total.time.min),
              all(last.improvement.time.med <= total.time.med),
              all(last.improvement.time.mean <= total.time.mean),
              all(last.improvement.time.sd >= 0),
              all(xor(last.improvement.time.sd == 0, last.improvement.time.max > last.improvement.time.min)),
              #
              min(last.improvement.fes.min) > 0L,
              min(last.improvement.fes.mean) > 0L,
              min(last.improvement.fes.med) > 0L,
              min(last.improvement.fes.max) > 0L,
              all(last.improvement.fes.max >= last.improvement.fes.min),
              all(last.improvement.fes.max >= last.improvement.fes.mean),
              all(last.improvement.fes.max >= last.improvement.fes.med),
              all(last.improvement.fes.mean >= last.improvement.fes.min),
              all(last.improvement.fes.med >= last.improvement.fes.min),
              all(last.improvement.fes.max <= total.fes.max),
              all(last.improvement.fes.min <= total.fes.min),
              all(last.improvement.fes.med <= total.fes.med),
              all(last.improvement.fes.mean <= total.fes.mean),
              all(last.improvement.fes.sd >= 0),
              all(xor(last.improvement.fes.sd == 0, last.improvement.fes.max > last.improvement.fes.min)),
              #
              min(best.f.min) > 0L,
              min(best.f.mean) > 0L,
              min(best.f.med) > 0L,
              min(best.f.max) > 0L,
              all(best.f.max >= best.f.min),
              all(best.f.max >= best.f.mean),
              all(best.f.max >= best.f.med),
              all(best.f.mean >= best.f.min),
              all(best.f.med >= best.f.min),
              all(best.f.sd >= 0),
              all(xor(best.f.sd == 0, best.f.max > best.f.min)),
              #
              all(n.opt.bound.upper >= 0L),
              all(n.opt.bound.upper >= n.opt.bound.upper),
              all(n.opt.bound.upper >= 0L),
              all(n.opt.bound.upper <= n.runs),
              all(n.opt.bound.lower <= n.runs),
              #
              all( (n.opt.bound.lower == 0L) == (ert.opt.bound.lower.time >= Inf)),
              all( (n.opt.bound.lower == 0L) == (ert.opt.bound.lower.fes >= Inf)),
              all( (n.opt.bound.upper == 0L) == (ert.opt.bound.upper.time >= Inf)),
              all( (n.opt.bound.upper == 0L) == (ert.opt.bound.upper.fes >= Inf)),
              #
              all(ert.opt.bound.lower.time >= 0),
              all(ert.opt.bound.lower.fes > 0),
              all(ert.opt.bound.upper.time >= 0),
              all(ert.opt.bound.upper.fes > 0),
              #
              all(ert.opt.bound.lower.time >= ert.opt.bound.upper.time),
              all(ert.opt.bound.lower.fes >= ert.opt.bound.upper.fes),
              #
              all(frame.algorithm == algorithms.unique[frame.algorithm.index]),
              all(frame.instance == instances.unique[frame.instance.index])
      );

    config$logger("done computing basic statistics, now computing more statistics.");

# the algorithm ert ranking function
    .algo.ranks.on.inst.ert <- function(source) {
      l <- lapply(seq_along(instances.unique),
                  function(i) {
                    r <- frame.instance.index == i;
                    stopifnot(sum(r) > 0L,
                              length(r) == length(source));
                    r <- rank(source[r]);
                    stopifnot(length(r) > 0L,
                              all(r > 0),
                              all(is.finite(r)),
                              all(r <= length(r)));
                    r <- .try.convert.to.int(r);
                    stopifnot(length(r) > 0L,
                              all(r > 0),
                              all(is.finite(r)),
                              all(r <= length(r)));
                    return(r);
                  });
      ranks <- vapply(seq_len(n),
                      function(i) {
                        ll <- l[[frame.instance.index[[i]]]];
                        return(ll[[frame.algorithm.index[[i]]]]);
                      }, NA_real_);
      ranks <- force(ranks);
      stopifnot(all(ranks > 0),
                all(ranks <= length(algorithms.unique)),
                all(is.finite(ranks)));
      ranks <- .try.convert.to.int(ranks);
      ranks <- force(ranks);
      stopifnot(all(ranks > 0),
                all(ranks <= length(algorithms.unique)),
                all(is.finite(ranks)));
      return(ranks);
    }

    algo.ranks.on.inst.ert.opt.bound.upper.time <- .algo.ranks.on.inst.ert(ert.opt.bound.upper.time);
    algo.ranks.on.inst.ert.opt.bound.upper.time <- force(algo.ranks.on.inst.ert.opt.bound.upper.time);
    algo.ranks.on.inst.ert.opt.bound.upper.fes <- .algo.ranks.on.inst.ert(ert.opt.bound.upper.fes);
    algo.ranks.on.inst.ert.opt.bound.upper.fes <- force(algo.ranks.on.inst.ert.opt.bound.upper.fes);
    algo.ranks.on.inst.n.opt.bound.upper <- .algo.ranks.on.inst.ert(-n.opt.bound.upper);
    algo.ranks.on.inst.n.opt.bound.upper <- force(algo.ranks.on.inst.n.opt.bound.upper);

    algo.ranks.on.inst.ert.opt.bound.lower.time <- .algo.ranks.on.inst.ert(ert.opt.bound.lower.time);
    algo.ranks.on.inst.ert.opt.bound.lower.time <- force(algo.ranks.on.inst.ert.opt.bound.lower.time);
    algo.ranks.on.inst.ert.opt.bound.lower.fes <- .algo.ranks.on.inst.ert(ert.opt.bound.lower.fes);
    algo.ranks.on.inst.ert.opt.bound.lower.fes <- force(algo.ranks.on.inst.ert.opt.bound.lower.fes);
    algo.ranks.on.inst.n.opt.bound.lower <- .algo.ranks.on.inst.ert(-n.opt.bound.lower);
    algo.ranks.on.inst.n.opt.bound.lower <- force(algo.ranks.on.inst.n.opt.bound.lower);

    # the instance ert ranking function
    .inst.ranks.for.algos.ert <- function(source) {
      l <- lapply(seq_along(algorithms.unique),
                  function(i) {;
                    r <- frame.algorithm.index == i;
                    stopifnot(sum(r) > 0L,
                              length(r) == length(source));
                    r <- rank(source[r]);
                    stopifnot(length(r) > 0L,
                              all(r > 0),
                              all(is.finite(r)),
                              all(r <= length(r)));
                    r <- .try.convert.to.int(r);
                    stopifnot(length(r) > 0L,
                              all(r > 0),
                              all(is.finite(r)),
                              all(r <= length(r)));
                    return(r);
                  });
      ranks <- vapply(seq_len(n),
                      function(i) {
                        ll <- l[[frame.algorithm.index[[i]]]];
                        return(ll[[frame.instance.index[[i]]]]);
                      }, NA_real_);
      ranks <- force(ranks);
      stopifnot(all(ranks > 0),
                all(ranks <= length(algorithms.unique)),
                all(is.finite(ranks)));
      ranks <- .try.convert.to.int(ranks);
      ranks <- force(ranks);
      stopifnot(all(ranks > 0),
                all(ranks <= length(algorithms.unique)),
                all(is.finite(ranks)));
      return(ranks);
    }

    inst.ranks.for.algo.ert.opt.bound.lower.time <- .inst.ranks.for.algos.ert(ert.opt.bound.upper.time);
    inst.ranks.for.algo.ert.opt.bound.lower.time <- force(inst.ranks.for.algo.ert.opt.bound.lower.time);
    inst.ranks.for.algo.ert.opt.bound.lower.fes <- .inst.ranks.for.algos.ert(ert.opt.bound.upper.fes);
    inst.ranks.for.algo.ert.opt.bound.lower.fes <- force(inst.ranks.for.algo.ert.opt.bound.lower.fes);
    inst.ranks.for.algo.n.opt.bound.upper <- .inst.ranks.for.algos.ert(-n.opt.bound.upper);
    inst.ranks.for.algo.n.opt.bound.upper <- force(inst.ranks.for.algo.n.opt.bound.upper);

    inst.ranks.for.algo.ert.opt.bound.upper.time <- .inst.ranks.for.algos.ert(ert.opt.bound.upper.time);
    inst.ranks.for.algo.ert.opt.bound.upper.time <- force(inst.ranks.for.algo.ert.opt.bound.upper.time);
    inst.ranks.for.algo.ert.opt.bound.upper.fes <- .inst.ranks.for.algos.ert(ert.opt.bound.upper.fes);
    inst.ranks.for.algo.ert.opt.bound.upper.fes <- force(inst.ranks.for.algo.ert.opt.bound.upper.fes);
    inst.ranks.for.algo.n.opt.bound.lower <- .inst.ranks.for.algos.ert(-n.opt.bound.lower);
    inst.ranks.for.algo.n.opt.bound.lower <- force(inst.ranks.for.algo.n.opt.bound.lower);

# the algorithm best result ranking function
    .algo.ranks.on.inst <- function(source) {
      l <- lapply(seq_along(instances.unique),
                  function(i) {
                    r <- instances.indices == i;
                    stopifnot(sum(r) > 0L,
                              length(r) == length(source));
                    r <- rank(source[r]);
                    stopifnot(all(r > 0),
                              all(is.finite(r)),
                              all(r <= length(r)));
                    r <- .try.convert.to.int(r);
                    stopifnot(all(r > 0),
                              all(is.finite(r)),
                              all(r <= length(r)));
                    return(r);
                  });
      ranks <- vapply(seq_len(n),
                      function(i) {
                        inst <- frame.instance.index[[i]];
                        ll <- l[[inst]];
                        stopifnot(length(ll) > 0L);

                        r <- instances.indices == inst;
                        stopifnot(sum(r) > 0L,
                                  length(r) == length(source));
                        r <- algorithms.indices[r] == frame.algorithm.index[[i]];
                        stopifnot(sum(r) > 0L,
                                  length(r) == length(ll));
                        r <- sum(ll[r]);
                        r <- force(r);
                        stopifnot(is.finite(r),
                                  r > 0);
                        return(r);
                      }, NA_real_);
      ranks <- force(ranks);
      stopifnot(all(ranks > 0),
                all(is.finite(ranks)));
      ranks <- .try.convert.to.int(ranks);
      ranks <- force(ranks);
      stopifnot(all(ranks > 0),
                all(is.finite(ranks)));

      ranks2 <- .algo.ranks.on.inst.ert(ranks);
      ranks2 <- force(ranks2);

      for(i in seq_along(instances.unique)) {
        stopifnot(order(ranks2[frame.instance.index == i]) ==
                    order(ranks[frame.instance.index == i]));
      }

      return(ranks2);
    }
    algo.ranks.on.inst.best.f <- .algo.ranks.on.inst(endResults$best.f);

# the instance  best result ranking function
    .inst.ranks.for.algos <- function(source) {
      l <- lapply(seq_along(algorithms.unique),
                  function(i) {
                    r <- algorithms.indices == i;
                    stopifnot(sum(r) > 0L,
                              length(r) == length(source));
                    r <- rank(source[r]);
                    stopifnot(all(r > 0),
                              all(is.finite(r)),
                              all(r <= length(r)));
                    r <- .try.convert.to.int(r);
                    stopifnot(all(r > 0),
                              all(is.finite(r)),
                              all(r <= length(r)));
                    return(r);
                  });
      ranks <- vapply(seq_len(n),
                      function(i) {
                        algo = frame.algorithm.index[[i]];
                        ll <- l[[algo]];
                        stopifnot(length(ll) > 0L);

                        r <- algorithms.indices == algo;
                        stopifnot(sum(r) > 0L,
                                  length(r) == length(source));
                        r <- instances.indices[r] == frame.instance.index[[i]];
                        stopifnot(sum(r) > 0L,
                                  length(r) == length(ll));
                        r <- sum(ll[r]);
                        r <- force(r);
                        stopifnot(is.finite(r),
                                  r > 0);
                        return(r);
                      }, NA_real_);
      ranks <- force(ranks);
      stopifnot(all(ranks > 0),
                all(is.finite(ranks)));
      ranks <- .try.convert.to.int(ranks);
      ranks <- force(ranks);
      stopifnot(all(ranks > 0),
                all(is.finite(ranks)));

      ranks2 <- .inst.ranks.for.algos.ert(ranks);
      ranks2 <- force(ranks2);

      for(i in seq_along(algorithms.unique)) {
        stopifnot(order(ranks2[frame.algorithm.index == i]) ==
                    order(ranks[frame.algorithm.index == i]));
      }

      return(ranks2);
    }
    inst.ranks.for.algo.best.f <- .inst.ranks.for.algos(endResults$best.f);
    stopifnot(all(is.finite(endResults.best.f.rel)),
              all(endResults.best.f.rel >= 0));
    inst.ranks.for.algo.best.f.rel <- .inst.ranks.for.algos(endResults.best.f.rel);
    rm("endResults.best.f.rel");

    stopifnot(#
              all(algo.ranks.on.inst.ert.opt.bound.upper.time > 0),
              all(algo.ranks.on.inst.ert.opt.bound.upper.time <= length(algorithms.unique)),
              all(is.finite(algo.ranks.on.inst.ert.opt.bound.upper.time)),
              length(algo.ranks.on.inst.ert.opt.bound.upper.time) == n,
#
              all(algo.ranks.on.inst.ert.opt.bound.upper.fes > 0),
              all(algo.ranks.on.inst.ert.opt.bound.upper.fes <= length(algorithms.unique)),
              all(is.finite(algo.ranks.on.inst.ert.opt.bound.upper.fes)),
              length(algo.ranks.on.inst.ert.opt.bound.upper.fes) == n,
#
              all(algo.ranks.on.inst.n.opt.bound.upper > 0),
              all(algo.ranks.on.inst.n.opt.bound.upper <= length(algorithms.unique)),
              all(is.finite(algo.ranks.on.inst.n.opt.bound.upper)),
              length(algo.ranks.on.inst.n.opt.bound.upper) == n,
#
              all(algo.ranks.on.inst.ert.opt.bound.lower.time > 0),
              all(algo.ranks.on.inst.ert.opt.bound.lower.time <= length(algorithms.unique)),
              all(is.finite(algo.ranks.on.inst.ert.opt.bound.lower.time)),
              length(algo.ranks.on.inst.ert.opt.bound.lower.time) == n,
              #
              all(algo.ranks.on.inst.ert.opt.bound.lower.fes > 0),
              all(algo.ranks.on.inst.ert.opt.bound.lower.fes <= length(algorithms.unique)),
              all(is.finite(algo.ranks.on.inst.ert.opt.bound.lower.fes)),
              length(algo.ranks.on.inst.ert.opt.bound.lower.fes) == n,
#
              all(algo.ranks.on.inst.n.opt.bound.lower > 0),
              all(algo.ranks.on.inst.n.opt.bound.lower <= length(algorithms.unique)),
              all(is.finite(algo.ranks.on.inst.n.opt.bound.lower)),
              length(algo.ranks.on.inst.n.opt.bound.lower) == n,
              ##
              all(inst.ranks.for.algo.ert.opt.bound.upper.time > 0),
              all(inst.ranks.for.algo.ert.opt.bound.upper.time <= length(instances.unique)),
              all(is.finite(inst.ranks.for.algo.ert.opt.bound.upper.time)),
              length(inst.ranks.for.algo.ert.opt.bound.upper.time) == n,
              #
              all(inst.ranks.for.algo.ert.opt.bound.upper.fes > 0),
              all(inst.ranks.for.algo.ert.opt.bound.upper.fes <= length(instances.unique)),
              all(is.finite(inst.ranks.for.algo.ert.opt.bound.upper.fes)),
              length(inst.ranks.for.algo.ert.opt.bound.upper.fes) == n,
#
              all(inst.ranks.for.algo.n.opt.bound.upper > 0),
              all(inst.ranks.for.algo.n.opt.bound.upper <= length(instances.unique)),
              all(is.finite(inst.ranks.for.algo.n.opt.bound.upper)),
              length(inst.ranks.for.algo.n.opt.bound.upper) == n,
              #
              all(inst.ranks.for.algo.ert.opt.bound.lower.time > 0),
              all(inst.ranks.for.algo.ert.opt.bound.lower.time <= length(instances.unique)),
              all(is.finite(inst.ranks.for.algo.ert.opt.bound.lower.time)),
              length(inst.ranks.for.algo.ert.opt.bound.lower.time) == n,
              #
              all(inst.ranks.for.algo.ert.opt.bound.lower.fes > 0),
              all(inst.ranks.for.algo.ert.opt.bound.lower.fes <= length(instances.unique)),
              all(is.finite(inst.ranks.for.algo.ert.opt.bound.lower.fes)),
              length(inst.ranks.for.algo.ert.opt.bound.lower.fes) == n,
#
              all(inst.ranks.for.algo.n.opt.bound.lower > 0),
              all(inst.ranks.for.algo.n.opt.bound.lower <= length(instances.unique)),
              all(is.finite(inst.ranks.for.algo.n.opt.bound.lower)),
              length(inst.ranks.for.algo.n.opt.bound.lower) == n,
#
              all(is.finite(algo.ranks.on.inst.best.f)),
              all(algo.ranks.on.inst.best.f > 0L),
              length(algo.ranks.on.inst.best.f) == n,
#
              all(is.finite(inst.ranks.for.algo.best.f.rel)),
              all(inst.ranks.for.algo.best.f.rel > 0L),
              length(inst.ranks.for.algo.best.f.rel) == n
              );

    config$logger("done computing statistics, now writing csv file '", file, "'.");

    write.csv(data.frame(algorithm=frame.algorithm,
                         instance=frame.instance,
                         n.runs,
                         total.time.min,
                         total.time.mean,
                         total.time.med,
                         total.time.max,
                         total.time.sd,
                         total.fes.min,
                         total.fes.mean,
                         total.fes.med,
                         total.fes.max,
                         total.fes.sd,
                         best.f.min,
                         best.f.min.file,
                         best.f.mean,
                         best.f.mean.file,
                         best.f.med,
                         best.f.med.file,
                         best.f.max,
                         best.f.max.file,
                         best.f.sd,
                         best.f.rel.min,
                         best.f.rel.mean,
                         best.f.rel.med,
                         best.f.rel.max,
                         best.f.rel.sd,
                         last.improvement.time.min,
                         last.improvement.time.mean,
                         last.improvement.time.med,
                         last.improvement.time.max,
                         last.improvement.time.sd,
                         last.improvement.fes.min,
                         last.improvement.fes.mean,
                         last.improvement.fes.med,
                         last.improvement.fes.max,
                         last.improvement.fes.sd,
                         n.opt.bound.upper,
                         ert.opt.bound.upper.time,
                         ert.opt.bound.upper.fes,
                         n.opt.bound.lower,
                         ert.opt.bound.lower.time,
                         ert.opt.bound.lower.fes,
                         algo.ranks.on.inst.best.f,
                         inst.ranks.for.algo.best.f,
                         inst.ranks.for.algo.best.f.rel,
                         algo.ranks.on.inst.ert.opt.bound.upper.time,
                         algo.ranks.on.inst.ert.opt.bound.upper.fes,
                         algo.ranks.on.inst.n.opt.bound.upper,
                         algo.ranks.on.inst.ert.opt.bound.lower.time,
                         algo.ranks.on.inst.ert.opt.bound.lower.fes,
                         algo.ranks.on.inst.n.opt.bound.lower,
                         inst.ranks.for.algo.ert.opt.bound.upper.time,
                         inst.ranks.for.algo.ert.opt.bound.upper.fes,
                         inst.ranks.for.algo.n.opt.bound.upper,
                         inst.ranks.for.algo.ert.opt.bound.lower.time,
                         inst.ranks.for.algo.ert.opt.bound.lower.fes,
                         inst.ranks.for.algo.n.opt.bound.lower,
                         check.names = FALSE),
              file=file,
              row.names=FALSE,
              quote=FALSE);

    stopifnot(file.exists(file),
              file.size(file) > 50L*n);
    config$logger("file '", file, "' created successfully.");

    rm("endResults");
    rm("algorithms.all");
    rm("algorithms.unique");
    rm("instances.all");
    rm("instances.unique");
    rm("instances.names");
    rm("instances.opt.bound.lower");
    rm("instances.opt.bound.upper");
    rm("frame.algorithm");
    rm("frame.instance");
    rm("n.runs");
    rm("total.time.min");
    rm("total.time.mean");
    rm("total.time.med");
    rm("total.time.max");
    rm("total.time.sd");
    rm("total.fes.min");
    rm("total.fes.mean");
    rm("total.fes.med");
    rm("total.fes.max");
    rm("total.fes.sd");
    rm("best.f.min");
    rm("best.f.min.file");
    rm("best.f.mean");
    rm("best.f.mean.file");
    rm("best.f.med");
    rm("best.f.med.file");
    rm("best.f.max");
    rm("best.f.max.file");
    rm("best.f.sd");
    rm("best.f.rel.min");
    rm("best.f.rel.mean");
    rm("best.f.rel.med");
    rm("best.f.rel.max");
    rm("best.f.rel.sd");
    rm("last.improvement.time.min");
    rm("last.improvement.time.mean");
    rm("last.improvement.time.med");
    rm("last.improvement.time.max");
    rm("last.improvement.time.sd");
    rm("last.improvement.fes.min");
    rm("last.improvement.fes.mean");
    rm("last.improvement.fes.med");
    rm("last.improvement.fes.max");
    rm("last.improvement.fes.sd");
    rm("n.opt.bound.upper");
    rm("ert.opt.bound.upper.time");
    rm("ert.opt.bound.upper.fes");
    rm("n.opt.bound.lower");
    rm("ert.opt.bound.lower.time");
    rm("ert.opt.bound.lower.fes");
    rm("sel");
    rm("algo.ranks.on.inst.best.f");
    rm("inst.ranks.for.algo.best.f");
    rm("inst.ranks.for.algo.best.f.rel");
    rm("algo.ranks.on.inst.ert.opt.bound.upper.time");
    rm("algo.ranks.on.inst.ert.opt.bound.upper.fes");
    rm("algo.ranks.on.inst.n.opt.bound.upper");
    rm("algo.ranks.on.inst.ert.opt.bound.lower.time");
    rm("algo.ranks.on.inst.ert.opt.bound.lower.fes");
    rm("algo.ranks.on.inst.n.opt.bound.lower");
    rm("inst.ranks.for.algo.ert.opt.bound.lower.time");
    rm("inst.ranks.for.algo.ert.opt.bound.lower.fes");
    rm("inst.ranks.for.algo.n.opt.bound.lower");
    rm("inst.ranks.for.algo.ert.opt.bound.upper.time");
    rm("inst.ranks.for.algo.ert.opt.bound.upper.fes");
    rm("inst.ranks.for.algo.n.opt.bound.upper");
    rm("frame.algorithm.index");
    rm("frame.instance.index");
  }

  stopifnot(file.exists(file),
            file.size(file) > 100L);
  config$logger("now loading end results statistics from file '", file, "'.");
  result <- read.csv(file, check.names = FALSE);
  result <- force(result);

  stopifnot(is.data.frame(result),
            nrow(result) > 0L,
            ncol(result) == 58L,
            colnames(result) == c("algorithm",
                                  "instance",
                                  "n.runs",
                                  "total.time.min",
                                  "total.time.mean",
                                  "total.time.med",
                                  "total.time.max",
                                  "total.time.sd",
                                  "total.fes.min",
                                  "total.fes.mean",
                                  "total.fes.med",
                                  "total.fes.max",
                                  "total.fes.sd",
                                  "best.f.min",
                                  "best.f.min.file",
                                  "best.f.mean",
                                  "best.f.mean.file",
                                  "best.f.med",
                                  "best.f.med.file",
                                  "best.f.max",
                                  "best.f.max.file",
                                  "best.f.sd",
                                  "best.f.rel.min",
                                  "best.f.rel.mean",
                                  "best.f.rel.med",
                                  "best.f.rel.max",
                                  "best.f.rel.sd",
                                  "last.improvement.time.min",
                                  "last.improvement.time.mean",
                                  "last.improvement.time.med",
                                  "last.improvement.time.max",
                                  "last.improvement.time.sd",
                                  "last.improvement.fes.min",
                                  "last.improvement.fes.mean",
                                  "last.improvement.fes.med",
                                  "last.improvement.fes.max",
                                  "last.improvement.fes.sd",
                                  "n.opt.bound.upper",
                                  "ert.opt.bound.upper.time",
                                  "ert.opt.bound.upper.fes",
                                  "n.opt.bound.lower",
                                  "ert.opt.bound.lower.time",
                                  "ert.opt.bound.lower.fes",
                                  "algo.ranks.on.inst.best.f",
                                  "inst.ranks.for.algo.best.f",
                                  "inst.ranks.for.algo.best.f.rel",
                                  "algo.ranks.on.inst.ert.opt.bound.upper.time",
                                  "algo.ranks.on.inst.ert.opt.bound.upper.fes",
                                  "algo.ranks.on.inst.n.opt.bound.upper",
                                  "algo.ranks.on.inst.ert.opt.bound.lower.time",
                                  "algo.ranks.on.inst.ert.opt.bound.lower.fes",
                                  "algo.ranks.on.inst.n.opt.bound.lower",
                                  "inst.ranks.for.algo.ert.opt.bound.upper.time",
                                  "inst.ranks.for.algo.ert.opt.bound.upper.fes",
                                  "inst.ranks.for.algo.n.opt.bound.upper",
                                  "inst.ranks.for.algo.ert.opt.bound.lower.time",
                                  "inst.ranks.for.algo.ert.opt.bound.lower.fes",
                                  "inst.ranks.for.algo.n.opt.bound.lower"),
#
            length(unique(result$n.runs)) == 1L,
            result$n.runs[[1L]] > 2L,
            #
            min(result$total.time.min) >= 0L,
            min(result$total.time.mean) >= 0L,
            min(result$total.time.med) >= 0L,
            min(result$total.time.max) >= 0L,
            all(result$total.time.max >= result$total.time.min),
            all(result$total.time.max >= result$total.time.mean),
            all(result$total.time.max >= result$total.time.med),
            all(result$total.time.mean >= result$total.time.min),
            all(result$total.time.med >= result$total.time.min),
            all(result$total.time.sd >= 0),
            all(xor(result$total.time.sd == 0, result$total.time.max > result$total.time.min)),
            #
            min(result$total.fes.min) > 0L,
            min(result$total.fes.mean) > 0L,
            min(result$total.fes.med) > 0L,
            min(result$total.fes.max) > 0L,
            all(result$total.fes.max >= result$total.fes.min),
            all(result$total.fes.max >= result$total.fes.mean),
            all(result$total.fes.max >= result$total.fes.med),
            all(result$total.fes.mean >= result$total.fes.min),
            all(result$total.fes.med >= result$total.fes.min),
            all(result$total.fes.sd >= 0),
            all(xor(result$total.fes.sd == 0, result$total.fes.max > result$total.fes.min)),
            #
            min(result$last.improvement.time.min) >= 0L,
            min(result$last.improvement.time.mean) >= 0L,
            min(result$last.improvement.time.med) >= 0L,
            min(result$last.improvement.time.max) >= 0L,
            all(result$last.improvement.time.max >= result$last.improvement.time.min),
            all(result$last.improvement.time.max >= result$last.improvement.time.mean),
            all(result$last.improvement.time.max >= result$last.improvement.time.med),
            all(result$last.improvement.time.mean >= result$last.improvement.time.min),
            all(result$last.improvement.time.med >= result$last.improvement.time.min),
            all(result$last.improvement.time.max <= result$total.time.max),
            all(result$last.improvement.time.min <= result$total.time.min),
            all(result$last.improvement.time.med <= result$total.time.med),
            all(result$last.improvement.time.mean <= result$total.time.mean),
            all(result$last.improvement.time.sd >= 0),
            all(xor(result$last.improvement.time.sd == 0,
                    result$last.improvement.time.max > result$last.improvement.time.min)),
            #
            min(result$last.improvement.fes.min) > 0L,
            min(result$last.improvement.fes.mean) > 0L,
            min(result$last.improvement.fes.med) > 0L,
            min(result$last.improvement.fes.max) > 0L,
            all(result$last.improvement.fes.max >= result$last.improvement.fes.min),
            all(result$last.improvement.fes.max >= result$last.improvement.fes.mean),
            all(result$last.improvement.fes.max >= result$last.improvement.fes.med),
            all(result$last.improvement.fes.mean >= result$last.improvement.fes.min),
            all(result$last.improvement.fes.med >= result$last.improvement.fes.min),
            all(result$last.improvement.fes.max <= result$total.fes.max),
            all(result$last.improvement.fes.min <= result$total.fes.min),
            all(result$last.improvement.fes.med <= result$total.fes.med),
            all(result$last.improvement.fes.mean <= result$total.fes.mean),
            all(result$last.improvement.fes.sd >= 0),
            all(xor(result$last.improvement.fes.sd == 0,
                    result$last.improvement.fes.max > result$last.improvement.fes.min)),
            #
            min(result$best.f.min) > 0L,
            min(result$best.f.mean) > 0L,
            min(result$best.f.med) > 0L,
            min(result$best.f.max) > 0L,
            all(result$best.f.max >= result$best.f.min),
            all(result$best.f.max >= result$best.f.mean),
            all(result$best.f.max >= result$best.f.med),
            all(result$best.f.mean >= result$best.f.min),
            all(result$best.f.med >= result$best.f.min),
            all(result$best.f.sd >= 0),
            all(xor(result$best.f.sd == 0,
                    result$best.f.max > result$best.f.min)),
            #
            all(result$n.opt.bound.upper >= 0L),
            all(result$n.opt.bound.upper >= result$n.opt.bound.upper),
            all(result$n.opt.bound.upper >= 0L),
            all(result$n.opt.bound.upper <= result$n.runs),
            all(result$n.opt.bound.lower <= result$n.runs),
            #
            all( (result$n.opt.bound.lower == 0L) == (result$ert.opt.bound.lower.time >= Inf)),
            all( (result$n.opt.bound.lower == 0L) == (result$ert.opt.bound.lower.fes >= Inf)),
            all( (result$n.opt.bound.upper == 0L) == (result$ert.opt.bound.upper.time >= Inf)),
            all( (result$n.opt.bound.upper == 0L) == (result$ert.opt.bound.upper.fes >= Inf)),
            #
            all(result$ert.opt.bound.lower.time >= 0),
            all(result$ert.opt.bound.lower.fes > 0),
            all(result$ert.opt.bound.upper.time >= 0),
            all(result$ert.opt.bound.upper.fes > 0),
            #
            all(result$ert.opt.bound.lower.time >= result$ert.opt.bound.upper.time),
            all(result$ert.opt.bound.lower.fes >= result$ert.opt.bound.upper.fes),
#
            all(result$algo.ranks.on.inst.ert.opt.bound.upper.time > 0),
            all(is.finite(result$algo.ranks.on.inst.ert.opt.bound.upper.time)),
            #
            all(result$algo.ranks.on.inst.ert.opt.bound.upper.fes > 0),
            all(is.finite(result$algo.ranks.on.inst.ert.opt.bound.upper.fes)),
            #
            all(result$algo.ranks.on.inst.n.opt.bound.upper > 0),
            all(is.finite(result$algo.ranks.on.inst.n.opt.bound.upper)),
            #
            all(result$algo.ranks.on.inst.ert.opt.bound.lower.time > 0),
            all(is.finite(result$algo.ranks.on.inst.ert.opt.bound.lower.time)),
            #
            all(result$algo.ranks.on.inst.ert.opt.bound.lower.fes > 0),
            all(is.finite(result$algo.ranks.on.inst.ert.opt.bound.lower.fes)),
            #
            all(result$algo.ranks.on.inst.n.opt.bound.lower > 0),
            all(is.finite(result$algo.ranks.on.inst.n.opt.bound.lower)),
            ##
            all(result$inst.ranks.for.algo.ert.opt.bound.upper.time > 0),
            all(is.finite(result$inst.ranks.for.algo.ert.opt.bound.upper.time)),
            #
            all(result$inst.ranks.for.algo.ert.opt.bound.upper.fes > 0),
            all(is.finite(result$inst.ranks.for.algo.ert.opt.bound.upper.fes)),
            #
            all(result$inst.ranks.for.algo.n.opt.bound.upper > 0),
            all(is.finite(result$inst.ranks.for.algo.n.opt.bound.upper)),
            #
            all(result$inst.ranks.for.algo.ert.opt.bound.lower.time > 0),
            all(is.finite(result$inst.ranks.for.algo.ert.opt.bound.lower.time)),
            #
            all(result$inst.ranks.for.algo.ert.opt.bound.lower.fes > 0),
            all(is.finite(result$inst.ranks.for.algo.ert.opt.bound.lower.fes)),
            #
            all(result$inst.ranks.for.algo.n.opt.bound.lower > 0),
            all(is.finite(result$inst.ranks.for.algo.n.opt.bound.lower)),
            #
            all(is.finite(result$algo.ranks.on.inst.best.f)),
            all(result$algo.ranks.on.inst.best.f > 0L),
            #
            all(is.finite(result$inst.ranks.for.algo.best.f.rel)),
            all(result$inst.ranks.for.algo.best.f.rel > 0L)
  );

  config$logger("done loading end result statistics from file '", file, "'.");
  gc();
  options(old.options);
  return(result);
}
