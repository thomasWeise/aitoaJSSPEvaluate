
#' @title Obtain a Data Frame with the End Results
#' @description We create a data frame with the end results from the log files,
#'   which can then be condensed into statistics.
#' @param config the configuration
#' @return a data frame with the information extracted from the files
#' @include utils.R
#' @include config.R
#' @export aitoa.end.results.frame
#' @importFrom utils read.csv write.csv
#' @include setups.R
#' @include load_log_file.R
#' @include instance_features.R
aitoa.end.results.frame <- function(config=aitoa.config()) {
  old.options <- options(warn=2);
  file <- file.path(.dir.eval(config=config), "endResults.txt");

  if(!file.exists(file)) {
    config$logger("end results file '", file, "' does not yet exist, so we need to create it.");

    base <- config$dir.results;
    stopifnot(dir.exists(base));

    setups <- aitoa.setups.frame(config);
    setups <- force(setups);
    stopifnot(is.data.frame(setups));
    n <- nrow(setups);
    n <- force(n);
    stopifnot(n > 0L,
              ncol(setups) >= 5L,
              sum(colnames(setups) %in% c("id", "algorithm", "instance", "seed", "file")) == 5L);
    config$logger("obtained a list of ", n, " log file paths, which will be parsed one by one.");

    features <- aitoa.instance.features.frame(config);
    features <- force(features);
    stopifnot(is.data.frame(features),
              nrow(features) >= 1L,
              ncol(features) >= 4L,
              sum(colnames(features) %in% c("inst.name",
                                            "inst.opt.bound.lower",
                                            "inst.opt.bound.upper")) == 3L);

    setups.instance <- as.character(unname(unlist(setups$instance)));
    setups.instance <- force(setups.instance);

    inst.name <- as.character(unname(unlist(features$inst.name)));
    inst.name <- force(inst.name);
    stopifnot(sort(unique(setups.instance)) == sort(unique(inst.name)));

    inst.opt.bound.lower <- unname(unlist(features$inst.opt.bound.lower));
    inst.opt.bound.lower <- force(inst.opt.bound.lower);
    stopifnot(is.integer(inst.opt.bound.lower),
              all(inst.opt.bound.lower > 0L),
              all(is.finite(inst.opt.bound.lower)));

    inst.opt.bound.upper <- unname(unlist(features$inst.opt.bound.upper));
    inst.opt.bound.upper <- force(inst.opt.bound.upper);
    stopifnot(is.integer(inst.opt.bound.upper),
              all(inst.opt.bound.upper >= inst.opt.bound.lower),
              all(is.finite(inst.opt.bound.upper)));
    rm("features");

    config$logger("loaded the instance bounds, now beginning to parse log files.");

    frame.total.time <- integer(n);
    frame.total.fes <- integer(n);
    frame.best.f <- integer(n);
    frame.last.improvement.time <- integer(n);
    frame.last.improvement.fes <- integer(n);
    frame.reached.opt.bound.lower <- logical(n);
    frame.reached.opt.bound.upper <- logical(n);
    frame.reached.opt.bound.upper.f <- integer(n);
    frame.reached.opt.bound.upper.fes <- integer(n);
    frame.reached.opt.bound.upper.time <- integer(n);

    for(i in seq_len(n)) {
      path <- file.path(base, setups$file[[i]]);
      stopifnot(file.exists(path));

      results <- aitoa.load.log.file(file=path, config=config, makeTimeUnique=FALSE);
      results <- force(results);
      stopifnot(is.data.frame(results));
      len <- nrow(results);
      len <- force(len);
      stopifnot(len > 0L);
      f <- unname(unlist(results$f));
      f <- force(f);
      fes <- unname(unlist(results$fes));
      fes <- force(fes);
      t <- unname(unlist(results$t));
      t <- force(t);

      rm("results");

      f <- force(f);
      fes <- force(fes);
      t <- force(t);
      len <- force(len);

      stopifnot(is.vector(f));
      stopifnot(is.integer(f));
      stopifnot(length(f) == len);
      stopifnot(is.vector(fes));
      stopifnot(is.integer(fes));
      stopifnot(length(fes) == len);
      stopifnot(is.vector(t));
      stopifnot(is.integer(t));
      stopifnot(length(t) == len);

      total.time <- t[[len]];
      total.time <- force(total.time);
      total.fes <- fes[[len]];
      total.fes <- force(total.fes);
      best.f <- f[[len]];
      best.f <- force(best.f);
      stopifnot(total.time >= 0L,
                is.integer(total.time),
                is.finite(total.time),
                total.fes >= 0L,
                is.integer(total.fes),
                is.finite(total.fes),
                best.f > 0L,
                is.integer(best.f),
                is.finite(best.f));
      last.improvement.time <- total.time;
      last.improvement.time <- force(last.improvement.time);
      last.improvement.fes <- total.fes;
      last.improvement.fes <- force(last.improvement.fes);

      while((len > 1L) && (f[[len - 1L]] <= best.f)) {
        len <- len - 1L;
        len <- force(len);
        x <- f[[len]];
        x <- force(x);
        stopifnot(x == best.f);
        last.improvement.fes <- fes[[len]];
        last.improvement.fes <- force(last.improvement.fes);
        last.improvement.time <- t[[len]];
        last.improvement.time <- force(last.improvement.time);
        stopifnot(last.improvement.time >= 0L,
                  last.improvement.time <= total.time,
                  is.integer(last.improvement.time),
                  is.finite(last.improvement.time),
                  last.improvement.fes >= 0L,
                  last.improvement.fes < total.fes,
                  is.integer(last.improvement.fes),
                  is.finite(last.improvement.fes));
      }

      inst <- (setups.instance[[i]] == inst.name);
      inst <- force(inst);
      stopifnot(sum(inst) == 1L);
      inst <- which(inst);
      stopifnot(length(inst) == 1L);

      opt.bound.lower <- inst.opt.bound.lower[inst];
      stopifnot(is.integer(opt.bound.lower),
                length(opt.bound.lower) == 1L,
                is.finite(opt.bound.lower),
                opt.bound.lower > 0L);
      opt.bound.upper <- inst.opt.bound.upper[inst];
      stopifnot(is.integer(opt.bound.upper),
                length(opt.bound.upper) == 1L,
                is.finite(opt.bound.upper),
                opt.bound.upper >= opt.bound.lower);
      rm("inst");

      reached.opt.bound.lower <- (best.f <= opt.bound.lower);
      reached.opt.bound.lower <- force(reached.opt.bound.lower);
      if(reached.opt.bound.lower) {
        reached.opt.bound.upper <- TRUE;
        reached.opt.bound.upper.f <- best.f;
        reached.opt.bound.upper.fes <- last.improvement.fes;
        reached.opt.bound.upper.time <- last.improvement.time;
      } else {
        reached.opt.bound.upper <- (best.f <= opt.bound.upper);
        reached.opt.bound.upper <- force(reached.opt.bound.upper);
        if(reached.opt.bound.upper) {
          reached.opt.bound.upper.f <- best.f;
          reached.opt.bound.upper.fes <- last.improvement.fes;
          reached.opt.bound.upper.time <- last.improvement.time;

          while((len > 1L) && (f[[len - 1L]] <= opt.bound.upper)) {
            len <- len - 1L;
            len <- force(len);
            reached.opt.bound.upper.f <- f[[len]];
            reached.opt.bound.upper.f <- force(reached.opt.bound.upper.f);
            stopifnot(reached.opt.bound.upper.f >= best.f,
                      reached.opt.bound.upper.f <= opt.bound.upper);
            reached.opt.bound.upper.fes <- fes[[len]];
            reached.opt.bound.upper.fes <- force(reached.opt.bound.upper.fes);
            reached.opt.bound.upper.time <- t[[len]];
            reached.opt.bound.upper.time <- force(reached.opt.bound.upper.time);
            stopifnot(reached.opt.bound.upper.time >= 0L,
                      reached.opt.bound.upper.time <= total.time,
                      reached.opt.bound.upper.time <= last.improvement.time,
                      is.integer(reached.opt.bound.upper.time),
                      is.finite(reached.opt.bound.upper.time),
                      reached.opt.bound.upper.fes >= 0L,
                      reached.opt.bound.upper.fes < total.fes,
                      reached.opt.bound.upper.fes <= last.improvement.fes,
                      is.integer(reached.opt.bound.upper.fes),
                      is.finite(reached.opt.bound.upper.fes));
          }

        } else {
          reached.opt.bound.upper.f <- NA_integer_;
          reached.opt.bound.upper.fes <- NA_integer_;
          reached.opt.bound.upper.time <- NA_integer_;
        }
      }

      rm("f");
      rm("t");
      rm("fes");

      total.time <- force(total.time);
      frame.total.time[[i]] <- total.time;
      total.fes <- force(total.fes);
      frame.total.fes[[i]] <- total.fes;
      best.f <- force(best.f);
      frame.best.f[[i]] <- best.f;
      last.improvement.time <- force(last.improvement.time);
      frame.last.improvement.time[[i]] <- last.improvement.time;
      last.improvement.fes <- force(last.improvement.fes);
      frame.last.improvement.fes[[i]] <- last.improvement.fes;
      reached.opt.bound.lower <- force(reached.opt.bound.lower);
      frame.reached.opt.bound.lower[[i]] <- reached.opt.bound.lower;
      reached.opt.bound.upper <- force(reached.opt.bound.upper);
      frame.reached.opt.bound.upper[[i]] <- reached.opt.bound.upper;
      reached.opt.bound.upper.f <- force(reached.opt.bound.upper.f);
      frame.reached.opt.bound.upper.f[[i]] <- reached.opt.bound.upper.f;
      reached.opt.bound.upper.fes <- force(reached.opt.bound.upper.fes);
      frame.reached.opt.bound.upper.fes[[i]] <- reached.opt.bound.upper.fes;
      reached.opt.bound.upper.time <- force(reached.opt.bound.upper.time);
      frame.reached.opt.bound.upper.time[[i]] <- reached.opt.bound.upper.time;
    }

    frame.total.time <- force(frame.total.time);
    frame.total.fes <- force(frame.total.fes);
    frame.best.f <- force(frame.best.f);
    frame.last.improvement.time <- force(frame.last.improvement.time);
    frame.last.improvement.fes <- force(frame.last.improvement.fes);
    stopifnot(all(is.finite(frame.total.time)),
              all(frame.total.time >= 0L),
              all(is.integer(frame.total.time)),

              all(is.finite(frame.total.fes)),
              all(frame.total.fes > 0L),
              all(is.integer(frame.total.fes)),

              all(is.finite(frame.best.f)),
              all(frame.best.f > 0L),
              all(is.integer(frame.best.f)),

              all(is.finite(frame.last.improvement.fes)),
              all(frame.last.improvement.fes > 0L),
              all(frame.last.improvement.fes <= frame.total.fes),
              all(is.integer(frame.last.improvement.fes)),

              all(is.finite(frame.last.improvement.time)),
              all(frame.last.improvement.time > 0L),
              all(frame.last.improvement.time <= frame.total.time),
              all(is.integer(frame.last.improvement.time)),

              all(is.logical(frame.reached.opt.bound.lower)),
              all(!is.na(frame.reached.opt.bound.lower)),

              all(is.logical(frame.reached.opt.bound.upper)),
              all(!is.na(frame.reached.opt.bound.upper)),

              all(is.integer(frame.reached.opt.bound.upper.f)),
              all((frame.reached.opt.bound.upper.f > 0L) | is.na(frame.reached.opt.bound.upper.f)),

              all(is.integer(frame.reached.opt.bound.upper.fes)),
              all((frame.reached.opt.bound.upper.fes > 0L) | is.na(frame.reached.opt.bound.upper.fes)),

              all(is.integer(frame.reached.opt.bound.upper.time)),
              all((frame.reached.opt.bound.upper.time > 0L) | is.na(frame.reached.opt.bound.upper.time))
              );


    config$logger("done parsing log files, now writing results to file '", file, "'.");

    write.csv(data.frame(id=setups$id,
                         algorithm=setups$algorithm,
                         instance=setups$inst,
                         seed=setups$seed,
                         file=setups$file,
                         total.time=frame.total.time,
                         total.fes=frame.total.fes,
                         best.f=frame.best.f,
                         last.improvement.time=frame.last.improvement.time,
                         last.improvement.fes=frame.last.improvement.fes,
                         reached.opt.bound.lower=frame.reached.opt.bound.lower,
                         reached.opt.bound.upper=frame.reached.opt.bound.upper,
                         reached.opt.bound.upper.f=frame.reached.opt.bound.upper.f,
                         reached.opt.bound.upper.fes=frame.reached.opt.bound.upper.fes,
                         reached.opt.bound.upper.time=frame.reached.opt.bound.upper.time,
                         check.names = FALSE),
              file=file,
              row.names=FALSE,
              quote=FALSE);

    stopifnot(file.exists(file),
              file.size(file) > 15L*n);
    config$logger("file '", file, "' created successfully.");
  }

  stopifnot(file.exists(file),
            file.size(file) > 100L);
  config$logger("now loading end results from file '", file, "'.");
  result <- read.csv(file, check.names = FALSE);
  result <- force(result);
  stopifnot(nrow(result) > 0L,
            ncol(result) == 15L,
            colnames(result) == c("id", "algorithm", "instance", "seed",
                                  "file",
                                  "total.time", "total.fes", "best.f",
                                  "last.improvement.time",
                                  "last.improvement.fes",
                                  "reached.opt.bound.lower",
                                  "reached.opt.bound.upper",
                                  "reached.opt.bound.upper.f",
                                  "reached.opt.bound.upper.fes",
                                  "reached.opt.bound.upper.time"),
            all(result$last.improvement.time <= result$total.time),
            all(result$last.improvement.fes <= result$total.fes),
            all(result$last.improvement.time >= 0L),
            all(result$last.improvement.fes > 0L),
            all(is.integer(result$id)),
            all(is.factor(result$algorithm)),
            all(is.factor(result$instance)),
            all(is.factor(result$seed)),
            all(is.integer(result$total.time)),
            all(result$total.time > 0L),
            all(is.integer(result$total.fes)),
            all(result$total.fes > 0L),
            all(is.integer(result$best.f)),
            all(is.finite(result$best.f)),
            all(is.integer(result$last.improvement.fes)),
            all(is.finite(result$last.improvement.fes)),
            all(result$last.improvement.fes > 0L),
            all(result$last.improvement.time >= 0L),
            all(is.finite(result$last.improvement.time)),
            all(is.logical(result$reached.opt.bound.lower)),
            all(is.logical(result$reached.opt.bound.upper)),
            xor(is.na(result$reached.opt.bound.upper.f), result$reached.opt.bound.upper),
            xor(is.na(result$reached.opt.bound.upper.fes), result$reached.opt.bound.upper),
            xor(is.na(result$reached.opt.bound.upper.time), result$reached.opt.bound.upper));

  config$logger("done loading end results from file '", file, "'.");
  options(old.options);
  return(result);
}
