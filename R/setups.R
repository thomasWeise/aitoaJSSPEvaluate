
.seeds.make.file.list <- function(base, a, i) {
  list.files(file.path(base, a, i), pattern=".+\\.txt", recursive=FALSE, include.dirs=FALSE, no..=TRUE,
             full.names = FALSE);
}

.seeds.check.file <- function(base, a, i, f) {
  p <- file.path(base, a, i, f);
  if(file.exists(p) && (file.size(p) > 10L)) {
    x <- gregexpr("_", f, fixed=TRUE);
    if(length(x) <= 0L) { return(FALSE); }
    x <- x[[1L]];
    if(length(x) <= 0L) { return(FALSE); }
    x <- x[length(x)];
    if((x <= 0L) || (x >= (nchar(f)-4L))) { return(FALSE); }
    return(TRUE);
  }
  return(FALSE);
}

#' @title Obtain the List of Setup and Log files
#' @description This function extends best effort towards obtaining a list of
#'   reasonable setups and log files. The user can provide minimum numbers of
#'   instances and runs that are required per experiment via the configuration
#'   object. Here, we ensure that for all algorithms we have data for exactly
#'   the same instances from runs starting with exactly the same seeds. This
#'   ensures that all data is comparable. Additional runs will be ignored, as
#'   well as algorithms or instances for which insufficient data was collected.
#' @param config the configuration
#' @return a data frame with the following columns:
#' \describe{
#'   \item{id}{the id of the executed setup + random seed combination}
#'   \item{algorithm}{the algorithm directory part}
#'   \item{instance}{the instance directory part}
#'   \item{dir}{the sub-directory path, relative to \code{config$dir.results}}
#'   \item{seed}{the random seed for the run}
#'   \item{file}{the file of the run, relative to \code{config$dir.results}}
#' }
#' @include utils.R
#' @include config.R
#' @export aitoa.setups.frame
#' @importFrom utils read.csv write.csv
aitoa.setups.frame <- function(config=aitoa.config()) {
  old.options <- options(warn=2);
  file <- file.path(.dir.eval(config=config), "setups.txt");

  if(!file.exists(file)) {
    config$logger("setups file '", file, "' does not yet exist, so we need to create it.");

    base <- config$dir.results;
    stopifnot(dir.exists(base));

    algorithms <- sort(unique(unlist(list.dirs(path=base, full.names=FALSE, recursive=FALSE))));
    config$logger("found list of ", length(algorithms), " potential algorithms.");
    stopifnot(length(algorithms) > 0L);

    instances <- sort(unique(unlist(lapply(algorithms, function(a) { list.dirs(path=file.path(base, a), full.names=FALSE, recursive=FALSE) }))));
    config$logger("found list of ", length(instances), " potential instances");
    stopifnot(length(instances) > 0L);


    instances.len  <- length(instances);
    algorithms.len <- length(algorithms);
    repeat {
      repeat {
        repeat {
          repeat {
            instances.len <- length(instances);
            instances.len <- force(instances.len);
            algorithms.len <- length(algorithms);
            algorithms.len <- force(algorithms.len);

            algorithms <- algorithms[vapply(algorithms,
              function(a) {
                return(sum(vapply(instances, function(i) {
                  dir.exists(file.path(base, a, i))
                }, 0L))
                >= config$min.instances);
              },
              TRUE)];
            config$logger(length(algorithms), " algorithms have at least ", config$min.instances, " potential instance directories");
            stopifnot(length(algorithms) > 0L);

            instances <- instances[vapply(instances, function(i) {
              all(vapply(algorithms, function(a) dir.exists(file.path(base, a, i)), TRUE)) }, TRUE)];
            config$logger(length(instances), " instances exist for all of these algorithms");
            stopifnot(length(instances) >= config$min.instances);

            algorithms <- algorithms[vapply(algorithms,
                                            function(a) {
                                              return(sum(vapply(instances, function(i) {
                                                return(sum(vapply(.seeds.make.file.list(base, a, i),
                                                       function(f) .seeds.check.file(base, a, i, f), FALSE))
                                                       >= config$min.runs);
                                              }, FALSE)) >= config$min.instances) }, FALSE)];
            config$logger(length(algorithms), " algorithms remain with at least ", config$min.runs, " runs for at least ",
                          config$min.instances, " instances");
            stopifnot(length(algorithms) > 0L);

            instances <- instances[vapply(instances, function(i) {
                      all(vapply(algorithms, function(a) {
                        return(sum(vapply(.seeds.make.file.list(base, a, i),
                                          function(f) .seeds.check.file(base, a, i, f), FALSE)) >= config$min.runs);
                      }, FALSE)) }, FALSE)];
            config$logger(length(instances), " instances exist for all of these algorithms with the minimum number of log files");
            stopifnot(length(instances) >= config$min.instances);
            if(((length(instances) == instances.len) && (length(algorithms) == algorithms.len))) {
              break;
            }
          }
          instances.len  <- length(instances);
          algorithms.len <- length(algorithms);

          seeds <- lapply(instances, function(i) {
            return(lapply(algorithms, function(a) {
              files <- .seeds.make.file.list(base, a, i);
              files <- files[vapply(files, function(f) .seeds.check.file(base, a, i, f), FALSE)];
              res2 <- (sort(unlist(unique(vapply(files,
                                               function(f) {
                                                 x <- gregexpr("_", f, fixed=TRUE)[[1L]];
                                                 x <- x[length(x)];
                                                 return(substr(f, (x + 1L), nchar(f) - 4L));
                                               }, "")))));
              config$logger("got ", length(res2), " seed candidates for instance ", i, " on algorithm ", a);
              return(res2);
            }));
          });

          seeds.unique <- lapply(seq_along(instances),
                                 function(i) sort(unique(unlist(seeds[[i]]))));
          seeds.score <- lapply(seq_along(instances),
                                function(i) {
                                  vapply(seeds.unique[[i]],
                                         function(ss) sum(vapply(
                                           seq_along(seeds[[i]]),
                                           function(k) sum(seeds[[i]][[k]] %in% ss),
                                           0L)), 0L)
                                });
          seeds <- lapply(seq_along(instances),
                          function(i) {
                            seeds.unique[[i]][order(-seeds.score[[i]])[1L:min(config$min.runs, length(seeds.score[[i]]))]]
                          });

          sel <- vapply(seeds, length, 0L);
          v <- sel >= config$min.runs;
          instances <- instances[v];
          seeds <- seeds[v];
          sel <- sel[v];
          stopifnot(length(seeds) > 0L);
          config$logger(length(instances), " instances with sufficiently long seed list exist");

          if(((length(instances) == instances.len) && (length(algorithms) == algorithms.len))) {
            break;
          }
        }
        instances.len  <- length(instances);
        algorithms.len <- length(algorithms);

        frame.inst  <- unlist(lapply(seq_along(sel), function(i) rep(instances[i], sel[i])));
        frame.seeds <- unlist(seeds);
        stopifnot(length(frame.inst) == length(frame.seeds));
        frame.algo <- unlist(lapply(algorithms, function(a) rep(a, length(frame.inst))));
        frame.inst <- unlist(rep(frame.inst, length(algorithms)));
        stopifnot(length(frame.algo) == length(frame.inst));
        frame.seeds <- unlist(rep(frame.seeds, length(algorithms)));
        stopifnot(length(frame.algo) == length(frame.seeds));

        frame.dir <- vapply(seq_along(frame.inst),
                              function(i) {
                                file.path(frame.algo[i], frame.inst[i])
                              }, "");
        stopifnot(length(frame.algo) == length(frame.dir));
        dirs <- unique(frame.dir);
        config$logger("got a list of ", length(dirs), " directories.");

        dir.ex <- !vapply(dirs, function(f) dir.exists(file.path(base, f)), FALSE);
        if(any(dir.ex)) {
          no.algos     <- unique(frame.algo[dir.ex]);
          no.instances <- unique(frame.inst[dir.ex]);
          if(length(no.algos) <= length(no.instances)) {
            algorithms <- algorithms[!(algorithms %in% no.algos)];
            stopifnot(length(algorithms) < algorithms.len);
          } else {
            instances <- instances[!(instances %in% no.instances)];
            stopifnot(length(instances) < instances.len);
          }
        }

        if(((length(instances) == instances.len) && (length(algorithms) == algorithms.len))) {
          break;
        }
      }
      instances.len  <- length(instances);
      algorithms.len <- length(algorithms);

      stopifnot(all(vapply(dirs,
                           function(f) dir.exists(file.path(base, f)),
                           FALSE)));

      frame.files <- vapply(seq_along(frame.seeds),
                            function(i) {
                              file.path(frame.algo[i], frame.inst[i],
                                        paste(frame.algo[i], "_", frame.inst[i], "_", frame.seeds[i], ".txt", sep="", collapse=""))
                            }, "");
      stopifnot(length(frame.algo) == length(frame.files));
      config$logger("got a list of ", length(frame.files), " interesting log files.");

      files.ex <- !vapply(frame.files, function(f) file.exists(file.path(base, f)), FALSE);
      if(any(files.ex)) {
        no.algos     <- unique(frame.algo[files.ex]);
        no.instances <- unique(frame.inst[files.ex]);
        if(length(no.algos) <= length(no.instances)) {
          algorithms <- algorithms[!(algorithms %in% no.algos)];
          stopifnot(length(algorithms) < algorithms.len);
        } else {
          instances <- instances[!(instances %in% no.instances)];
          stopifnot(length(instances) < instances.len);
        }
      }

      if(((length(instances) == instances.len) && (length(algorithms) == algorithms.len))) {
        break;
      }
    }

    frame.id <- (1L:(length(frame.algo)));
    frame.seeds <- vapply(frame.seeds, function(s) paste("$", s, sep="", collapse=""), "");
    stopifnot(all(vapply(frame.files,
                         function(f) file.exists(file.path(base, f)),
                         FALSE)),
              length(frame.algo) > 0L,
              length(frame.id) == length(frame.algo),
              length(frame.algo) == length(frame.inst),
              length(frame.inst) == length(frame.dir),
              length(frame.dir) == length(frame.seeds),
              length(frame.seeds) == length(frame.files),
              length(unique(frame.inst)) == instances.len,
              instances.len >= config$min.instances,
              length(unique(frame.algo)) == algorithms.len,
              (length(frame.id)/(instances.len*algorithms.len)) >= config$min.runs);

    write.csv(data.frame(id=frame.id,
                         algorithm=frame.algo,
                         instance=frame.inst,
                         dir=frame.dir,
                         seed=frame.seeds,
                         file=frame.files,
                         check.names = FALSE),
              file=file,
              row.names=FALSE,
              quote=FALSE);

    rm("frame.algo");
    rm("frame.id");
    rm("frame.inst");
    rm("frame.seeds");
    rm("frame.dir");
    rm("frame.files");
    gc();

    stopifnot(file.exists(file),
              file.size(file) > 100L);
    config$logger("file '", file, "' created successfully.");
  }

  stopifnot(file.exists(file),
            file.size(file) > 100L);
  config$logger("now loading list of setups from file '", file, "'.");
  result <- read.csv(file, check.names = FALSE);
  result <- force(result);
  stopifnot(nrow(result) > 0L,
            ncol(result) == 6L,
            colnames(result) == c("id", "algorithm", "instance", "dir", "seed", "file"),
            all(is.integer(result$id)),
            all(result$id == (1L:nrow(result))),
            all(is.factor(result$algorithm)),
            all(is.factor(result$instance) | is.integer(result$instance)),
            all(is.factor(result$seed) | is.character(result$seed)),
            all(startsWith(as.character(result$seed), "$")),
            all(is.factor(result$dir) | is.character(result$dir)),
            all(is.factor(result$file) | is.character(result$file)));
  instances.len <- length(unique(result$instance));
  algorithms.len <- length(unique(result$algorithm));
  stopifnot(instances.len >= config$min.instances,
            (nrow(result)/(instances.len*algorithms.len)) == as.integer((nrow(result)/(instances.len*algorithms.len))),
            (nrow(result)/(instances.len*algorithms.len)) >= config$min.runs);
  gc();
  config$logger("done loading list of setups from file '", file, "'.");
  options(old.options);
  return(result);
}
