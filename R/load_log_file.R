
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
  old.options <- options(warn=2);
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
  check <- ((consumedTime > 0L) && is.finite(consumedTime));
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
  rm("f");
  rm("fes");
  rm("t");
  gc();
  options(old.options);
  return(data);
}
