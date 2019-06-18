
#' @title Load a Single Log File
#' @description Load a log file and return the results as a data frame.
#' @param file the log file
#' @param config the configuration
#' @param makeTimeUnique should we make the time indices unique (except maybe
#'   for the first and last point)? This makes sense when we want to plot
#'   diagrams over a time axis, asa we then have removed redundant points right
#'   away. If \code{makeTimeUnique==FALSE}, then there may be multiple
#'   improvements at the same time index due to the resolution of the computer
#'   clock (while each improvement will definitely have a unique FE).
#' @return a data frame with the columns \code{t} (time in ms), \code{fes}
#'   (function evaluations), and \code{f} (objective value), all of which are
#'   integer valued
#' @include config.R
#' @seealso aitoa.config
#' @export aitoa.load.log.file
aitoa.load.log.file <- function(file,
                                config=aitoa.config(),
                                makeTimeUnique=FALSE) {
  old.options <- options(warn=2);
  stopifnot(is.character(file),
            is.list(config),
            is.logical(makeTimeUnique));

  if(file.exists(file)) {
    file <- normalizePath(file, mustWork=TRUE);
  } else {
    file <- normalizePath(file.path(config$dir.results, file), mustWork=TRUE);
  }
  file <- force(file);
  stopifnot(file.exists(file),
            file.size(file) > 100L);

  config$logger("now loading file '", file, "'.");

  # read file as text file, one line = one element
  data <- readLines(con=file, warn=FALSE);
  data <- force(data);

  # detect consumed FEs
  consumedFEs <- grep("# CONSUMED_FES:", data, fixed = TRUE);
  consumedFEs <- force(consumedFEs);
  stopifnot(length(consumedFEs) > 0L);
  consumedFEs <- as.integer(trimws(strsplit(data[consumedFEs[[1L]]],":", fixed=TRUE)[[1L]][2L]));
  consumedFEs <- force(consumedFEs);
  check <- ((consumedFEs > 0L) && is.finite(consumedFEs));
  check <- force(check);
  stopifnot(check);

  # detect consumed time
  consumedTime <- grep("# CONSUMED_TIME:", data, fixed = TRUE);
  consumedTime <- force(consumedTime);
  stopifnot(length(consumedTime) > 0L);
  consumedTime <- as.integer(trimws(strsplit(data[consumedTime[[1L]]],":", fixed=TRUE)[[1L]][2L]));
  consumedTime <- force(consumedTime);
  check <- ((consumedTime >= 0L) && is.finite(consumedTime));
  check <- force(check);
  stopifnot(check);

  # extract the correct lines
  start <- grep("# BEGIN_LOG", data, fixed=TRUE)[[1L]];
  start <- force(start);
  end <- grep("# END_OF_LOG", data, fixed=TRUE)[[1L]];
  end <- force(end);
  stopifnot(start > 0L,
            end > (start + 2L),
            is.finite(start),
            is.finite(end));

  # load data as CSV
  data <- strsplit(data[(start+2L):(end-1L)], ";", fixed=TRUE);
  data <- force(data);
  f   <- as.integer(vapply(data, `[[`, "", 1L));
  f   <- force(f);
  fes <- as.integer(vapply(data, `[[`, "", 2L));
  fes <- force(fes);
  t   <- as.integer(vapply(data, `[[`, "", 3L));
  t   <- force(t);
  rm("data");

  stopifnot(length(unique(fes)) == length(fes));

  startPointAdded <- FALSE;
  if(isTRUE(makeTimeUnique)) {
    # take care of re-occuring time values
    t.unique.indexes <- findInterval(unique(t), t);
    if(t.unique.indexes[1L] != 1L) {
      t.unique.indexes <- c(1L, t.unique.indexes);
      startPointAdded <- TRUE;
    }
    t <- t[t.unique.indexes];
    t <- force(t);
    f <- f[t.unique.indexes];
    f <- force(f);
    fes <- fes[t.unique.indexes];
    fes <- force(fes);
  }

  l <- length(t);
  stopifnot(l >= 1L);

  # add an end point if necessary
  endPointAdded <- FALSE;
  if(fes[l] < consumedFEs) {
    stopifnot(consumedTime >= t[l]);
    l <- l + 1L;
    f[l]   <- f[l - 1L];
    fes[l] <- consumedFEs;
    t[l]   <- consumedTime;
    endPointAdded <- TRUE;
  } else {
    stopifnot(t[l] <= consumedTime);
    consumedTime <- t[l];
  }

  stopifnot(fes[l] == consumedFEs,
              t[l] == consumedTime);

  minLength <- 1L;
  if(startPointAdded) { minLength <- minLength + 1L; }
  if(endPointAdded) { minLength <- minLength + 1L; }
  stopifnot(length(t) >= minLength,
            length(t) == length(f),
            length(f) == length(fes),
            length(t) == l,
            all(is.finite(t)),
            all(is.finite(f)),
            all(is.finite(fes)),
            all(is.integer(t)),
            all(is.integer(f)),
            all(is.integer(fes)),
            all(fes > 0L),
            all(fes <= consumedFEs),
            all(t >= 0L),
            all(t <= consumedTime),
            all(f >= 0L));

  if(l > 1L) {
    before <- 1L:(l-1L);
    after  <- 2L:l;
    stopifnot(all(f[before] >= f[after]),
              all(fes[before] <= fes[after]),
              all(t[before] <= t[after]));
  }

  if(endPointAdded) {
    if(l > 2L) {
      stopifnot(fes[1L:(l-2L)] < fes[2L:(l-1L)]);
    }
    stopifnot(f[l] == f[l-1L]);
  }
  if(startPointAdded) {
    stopifnot(t[1L] == t[2L],
              f[1L] >= f[2L]);
  }

  t <- force(t);
  fes <- force(fes);
  f <- force(f);
  data <- data.frame(t=t, fes=fes, f=f);
  data <- force(data);
  rm("f");
  rm("fes");
  rm("t");
  gc();

# The odd and semantically useless force and dummy stuff is to
# prevent the "restarting interrupted promise evaluation" that
# tends to appear without any good reason down the road when
# using this function sometimes. I do not know a reason why that
# should be necessary, but it seemingly is.
  data <- force(data);
  dummy <- data$fes;
  dummy <- force(dummy);
  dummy <- data$fes[[l]];
  dummy <- force(dummy);
  dummy <- data$t;
  dummy <- force(dummy);
  dummy <- data$t[[l]];
  dummy <- force(dummy);
  dummy <- data$f;
  dummy <- force(dummy);
  dummy <- data$f[[l]];
  dummy <- force(dummy);
  stopifnot(is.data.frame(data),
            nrow(data) > 0L,
            ncol(data) == 3L,
            names(data) == c("t", "fes", "f"),
            is.integer(data$t),
            is.integer(data$fes),
            is.integer(data$f));
  options(old.options);

  return(data);
}
