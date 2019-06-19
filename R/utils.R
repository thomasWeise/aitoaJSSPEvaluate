
# internal utility functions

# internally repeat gsub until all occurences of a pattern have been removed
.internal.gsub <- function(expr, repl, text, fixed=FALSE) {
  l2 <- nchar(text);
  stopifnot(l2 > 0L);
  repeat {
    text <- gsub(expr, repl, text, fixed=fixed);
    l1 <- l2;
    l2 <- nchar(text);
    stopifnot(l2 > 0L);
    if(l1 == l2) { break; }
  }
  return(text);
}


# Make an evaluation directory
#' @include config.R
.dir.eval <- function(..., config) {
  path <- file.path(config$dir.evaluation, ...);
  if(!(dir.exists(path))) {
    config$logger("creating directory '", path, "'.");
    suppressWarnings(dir.create(path, showWarnings = FALSE, recursive = TRUE));
  }
  path <- normalizePath(path, mustWork = TRUE);
  stopifnot(dir.exists(path));
  return(path);
}


.try.convert.to.int <- function(a) {
  if(all(is.finite(a) & (a >= (-.Machine$integer.max)) & (a <= .Machine$integer.max))) {
    a.i <- as.integer(a);
    a.i <- force(a.i);
    if(all(a.i == a)) {
      return(a.i);
    }
  }
  return(a);
}

# make a graphics directory
.dir.plots <- function(..., config) {
  dir <- .dir.eval("plots", ..., config=config);
  stopifnot(dir.exists(dir));
  return(dir);
}
