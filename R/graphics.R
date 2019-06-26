
.golden.ratio <- 2 / (1 + sqrt(5))

# obtain a graphics name
#' @include utils.R
.graphics.name <- function(config, base) {
  stopifnot(!is.null(base), !is.na(base));
  base <- as.character(base);
  stopifnot(nchar(base) > 0L);

  base <- .internal.gsub(" ", "_", base, fixed=TRUE);
  base <- .internal.gsub(".", "_", base, fixed=TRUE);
  base <- .internal.gsub("%", "_", base, fixed=TRUE);
  base <- .internal.gsub("__", "_", base, fixed=TRUE);
  l2 <- nchar(base);
  stopifnot(l2 > 0L);

  base <- paste0(base, ".", config$graphics.ext);
  stopifnot(nchar(base) == (l2 + 4L));
  return(base);
}


#' @include config.R
#' @importFrom grDevices svg
#' @importFrom graphics par
.graphic <- function(config, path, width, height, expr) {
  if(!file.exists(path)) {
    config$logger("file '", path, "' does not exist - so we need to plot to it.");

    if(config$graphics.ext == "svg") {
      f <- svg;
    } else {
      if(config$graphics.ext == "eps") {
        f <- cairo_ps;
      }  else {
        if(config$graphics.ext == "pdf") {
          f <- cairo_pdf;
        } else {
          stop(paste0("illegal graphics extension: ", config$graphics.ext));
        }
      }
    }

    f(file=path, width=width, height=height, antialias="subpixel");
    pp <- par(ljoin=0L);
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
