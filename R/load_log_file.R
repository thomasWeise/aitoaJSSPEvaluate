
#' @title Load a Single Log File
#' @description Load a log file and return the results as a data frame.
#' @param file the log file
#' @param config the configuration
#' @return a data frame with the columns \code{t} (time in ms), \code{fes}
#'   (function evaluations), and \code{f} (objective value), all of which are
#'   integer valued
#' @include config.R
#' @seealso aitoa.config
#' @export aitoa.load.log.file
aitoa.load.log.file <- function(file,
                                config=aitoa.config()) {
  stopifnot(is.character(file));

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

  # detect consumed FEs
  consumedFEs <- grep("# CONSUMED_FES:", data, fixed = TRUE);
  stopifnot(length(consumedFEs) > 0L);
  consumedFEs <- as.integer(trimws(strsplit(data[consumedFEs[[1]]],":", fixed=TRUE)[[1]][2]));
  stopifnot(consumedFEs > 0L,
            is.finite(consumedFEs));

  consumedTime <- grep("# CONSUMED_TIME:", data, fixed = TRUE);
  stopifnot(length(consumedTime) > 0L);
  consumedTime <- as.integer(trimws(strsplit(data[consumedTime[[1]]],":", fixed=TRUE)[[1]][2]));
  stopifnot(consumedTime > 0L,
            is.finite(consumedTime));

  # extract the correct lines
  start <- grep("# BEGIN_LOG", data, fixed=TRUE)[[1L]];
  end <- grep("# END_OF_LOG", data, fixed=TRUE)[[1L]];
  stopifnot(start > 0L,
            end > (start + 2L),
            is.finite(start),
            is.finite(end));

  # load data as CSV
  data <- strsplit(data[(start+2L):(end-1L)], ";", fixed=TRUE);
  f   <- as.integer(vapply(data, `[[`, "", 1L));
  fes <- as.integer(vapply(data, `[[`, "", 2L));
  t   <- as.integer(vapply(data, `[[`, "", 3L));
  rm(data);

  stopifnot(length(unique(fes)) == length(fes));

  # take care of re-occuring time values
  startPointAdded <- FALSE;
  t.unique.indexes <- findInterval(unique(t), t);
  if(t.unique.indexes[1L] != 1L) {
    t.unique.indexes <- c(1L, t.unique.indexes);
    startPointAdded <- TRUE;
  }
  t <- t[t.unique.indexes];
  f <- f[t.unique.indexes];
  fes <- fes[t.unique.indexes];


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

  before <- 1L:(l-1L);
  after  <- 2L:l;

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
            all(f >= 0L),
            all(f[before] >= f[after]),
            all(fes[before] <= fes[after]),
            all(t[before] <= t[after]));

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

  data <- data.frame(t=t, fes=fes, f=f);
  data <- force(data);
  rm(f);
  rm(fes);
  rm(t);
  gc();
  return(data);
}
