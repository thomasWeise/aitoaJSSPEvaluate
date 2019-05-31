
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
aitoa.end.results.frame <- function(config=aitoa.config()) {
  file <- file.path(.dir.eval(config=config), "endResults.txt");

  if(!file.exists(file)) {
    config$logger("end results file '", file, "' does not yet exist, so we need to create it.");

    base <- config$dir.results;
    stopifnot(dir.exists(base));

    setups <- aitoa.setups.frame(config);
    n <- nrow(setups);
    stopifnot(is.data.frame(setups),
              n > 0L,
              ncol(setups) >= 5L,
              sum(colnames(setups) %in% c("id", "algorithm", "instance", "seed", "file")) == 5L);
    config$logger("obtained ", n, " log file paths, which will now be parsed one by one.");

    frame.total.time <- integer(n);
    frame.total.fes <- integer(n);
    frame.best.f <- integer(n);
    frame.last.improvement.time <- integer(n);
    frame.last.improvement.fes <- integer(n);

    for(i in seq_len(n)) {
      path <- file.path(base, setups$file[[i]]);
      stopifnot(file.exists(path));

      results <- aitoa.load.log.file(path, config);
      n <- nrow(results);
      stopifnot(is.data.frame(results),
                n > 0L);
      f <- unname(unlist(results$f));
      fes <- unname(unlist(results$fes));
      t <- unname(unlist(results$t));
      rm(results);

      total.time <- t[[n]];
      total.fes <- fes[[n]];
      best.f <- f[[n]];
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
      last.improvement.fes <- total.fes;

      while((n > 1L) && (f[[n - 1L]] <= best.f)) {
        n <- n - 1L;
        stopifnot(f[[n]] == best.f);
        last.improvement.fes <- fes[n];
        last.improvement.time <- t[n];
        stopifnot(last.improvement.time >= 0L,
                  last.improvement.time <= total.time,
                  is.integer(last.improvement.time),
                  is.finite(last.improvement.time),
                  last.improvement.fes >= 0L,
                  last.improvement.fes < total.fes,
                  is.integer(last.improvement.fes),
                  is.finite(last.improvement.fes));
      }

      frame.total.time[i] <- total.time;
      frame.total.fes[i] <- total.fes;
      frame.best.f[i] <- best.f;
      frame.last.improvement.time[i] <- last.improvement.time;
      frame.last.improvement.fes[i] <- last.improvement.fes;
    }

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
              all(is.integer(frame.last.improvement.time)));


    config$logger("done parsing log files, now writing results to file '", file, "'.");

    write.csv(data.frame(id=setups$id,
                         algorithm=setups$algorithm,
                         instance=setups$inst,
                         seed=setups$seed,
                         total.time=frame.total.time,
                         total.fes=frame.total.fes,
                         best.f=frame.best.f,
                         last.improvement.time=frame.last.improvement.time,
                         last.improvement.fes=frame.last.improvement.fes,
                         check.names = FALSE),
              file=file,
              row.names=FALSE,
              quote=FALSE);

    stopifnot(file.exists(file),
              file.size(file) > 10L*n);
    config$logger("file '", file, "' created successfully.");
  }

  stopifnot(file.exists(file),
            file.size(file) > 100L);
  config$logger("now loading end results from file '", file, "'.");
  result <- read.csv(file, check.names = FALSE);
  stopifnot(nrow(result) > 0L,
            ncol(result) == 9L,
            colnames(result) == c("id", "algorithm", "instance", "seed",
                                  "total.time", "total.fes", "best.f",
                                  "last.improvement.time",
                                  "last.improvement.fes"));
  config$logger("done loading end results from file '", file, "'.");
  return(result);
}
