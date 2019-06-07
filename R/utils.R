
# internal utility functions

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
