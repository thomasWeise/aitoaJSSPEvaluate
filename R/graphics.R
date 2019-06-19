# obtain a graphics name
.graphics.name <- function(base) {
  stopifnot(!is.null(base), !is.na(base));
  base <- as.character(base);
  stopifnot(nchar(base) > 0L);

  base <- gsub(" ", "_", base, fixed=TRUE);
  base <- gsub(".", "_", base, fixed=TRUE);
  base <- gsub("%", "_", base, fixed=TRUE);
  l2 <- nchar(base);
  stopifnot(l2 > 0L);
  repeat {
    base <- gsub("__", "_", base, fixed=TRUE);
    l1 <- l2;
    l2 <- nchar(base);
    stopifnot(l2 > 0L);
    if(l2 >= l1) { break; }
  }

  base <- paste0(base, ".svg");
  stopifnot(nchar(base) == (l2 + 4L));
  return(base);
}


#' @include config.R
#' @importFrom grDevices svg
#' @importFrom graphics par
.graphic <- function(config, path, width, height, expr) {
  if(!file.exists(path)) {
    config$logger("file '", path, "' does not exist - so we need to plot to it.");

    svg(file=path, width=width, height=height, antialias="subpixel");
    pp <- par(ljoin=0);
    eval(expr);
    par(pp);
    dev.off();
    path <- normalizePath(path, mustWork = TRUE);
    path <- force(path);

    config$logger("finished plotting to file ", path);
    gc();
  }
  return(path);
}
