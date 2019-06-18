.graphics.name <- function(base) {
  base <- gsub("%", "_", base, fixed=TRUE);
  l2 <- nchar(base);
  repeat {
    base <- gsub("__", "_", base, fixed=TRUE);
    l1 <- l2;
    l2 <- nchar(base);
    if(l2 >= l1) { break; }
  }
  paste0(base, ".svg");
}


#' @include config.R
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
