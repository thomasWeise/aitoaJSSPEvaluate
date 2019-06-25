#' @include utils.R
.post.process.plot.progress <- function(file) {
  stopifnot(file.exists(file));
  text <- readLines(con=file);
  stopifnot(length(text) > 0L);
  text <- paste(trimws(text), sep="", collapse="");
  text <- .internal.gsub('g clip-path=\\".*?\\" clip-rule=\\"nonzero\\"', "g", text);
  text <- .internal.gsub('<clipPath.*?</clipPath>', "", text);
  text <- .internal.gsub('</g>\\s*<g>', "", text);
  writeLines(text, con=file);
  return(file);
}

#' @include utils.R
#' @include load_log_file.R
#' @importFrom graphics axis legend lines
.plot.progress.inst <- function(config, instance,
                                names.setup,
                                names.name,
                                files.setup,
                                files.instance,
                                files.file,
                                colors,
                                ...,
                                max.t) {
  stopifnot(is.integer(max.t),
            max.t > 0L,
            is.finite(max.t),
            length(names.setup) > 0L,
            all(is.character(names.setup)),
            length(names.name) == length(names.setup),
            all(is.character(names.name)),
            length(files.setup) > 0L,
            all(is.character(files.setup)),
            length(files.instance) == length(files.setup),
            all(is.character(files.instance)),
            length(files.file) == length(files.instance),
            all(is.character(files.file)),
            length(colors) > 0L,
            all(is.character(colors)));
  config$logger("now processing instance '", instance, "': loading data.");
  data <- lapply(names.setup,
                 function(setup) {
                   sel <- (files.setup == setup) & (files.instance == instance);
                   stopifnot(sum(sel) >= config$min.runs);
                   files <- files.file[sel];
                   stopifnot(length(files) >= config$min.runs);
                   res <- lapply(files, function(f) {
                     r <- aitoa.load.log.file(f, config, makeTimeUnique=TRUE);
                     stopifnot(is.list(r), length(r) == 3L);
                     t <- r$t;
                     stopifnot(all(is.integer(t)), all(t >= 0L),
                               length(t) >= 1L);
                     l <- length(t);
                     if(l > 1L) {
                       stopifnot(length(unique(t[-1L])) == (length(t)-1L));
                     }
                     f <- r$f;
                     stopifnot(all(is.integer(f)), all(f > 0L));
                     if((l > 1L) && (t[[l]] != max.t)) {
                       if(f[[l - 1L]] == f[[l]]) {
                         t[[l]] <- max.t;
                       }
                     }
                     r <- list(t=t, f=f);
                     r <- force(r);
                     return(r);
                   });

                   l <- length(res);
                   stopifnot(l == length(files),
                             l >= config$min.runs,
                             all(vapply(res, length, 0L) == 2L));
                   res <- unique(res);
                   l2 <- length(res);
                   stopifnot(l2 <= l, l2 > 0L);
                   if(l2 < l) {
                     config$logger("reduced line count for setup '", setup,
                                   "' on instance '", instance, " from ", l,
                                   " to ", l2);
                   }
                   return(res);
                 });

  config$logger("done loading data, now trying to reduce data.");

  for(i in seq.int(1L, length(data)-1L)) {
    dl <- data[[i]];
    need <- !vapply(dl,
                  function(d1) {
                    any(vapply(seq.int(i+1L, length(data)),
                           function(j) {
                             any(vapply(data[[j]],
                                        function(d2) identical(d1, d2),
                                        FALSE))
                           }, FALSE))
                  }, FALSE);
    l1 <- length(dl);
    l2 <- sum(need);
    if(l2 < l1) {
      config$logger("reduced line count due to overlap from ", l1, " to ", l2);
      dl <- dl[need];
      dl <- force(dl);
      data[[i]] <- dl;
      data[[i]] <- force(data[[i]]);
    }
  }

  config$logger("finished trying to reduce data.");

  f.range <- range(unlist(lapply(data,
                                 function(d) {
#                                  stopifnot(length(d) >= config$min.runs);
                                   lapply(d,
                                          function(dd) {
                                            stopifnot(length(dd$f) >= 1L);
                                            range(dd$f);
                                          })
                                 }), recursive=TRUE));
  stopifnot(length(f.range) == 2L, all(is.finite(f.range)));
  config$logger("found f range ", paste(f.range, sep=":", collapse=":"));

  pars <- list(...);
  if(is.null(pars$x)) {
    pars$x <- c(1L, max.t);
  }
  if(is.null(pars$y)) {
    pars$y <- f.range;
  }
  if(is.null(pars$type)) {
    pars$type <- "n";
  }
  if(is.null(pars$xaxs)) {
    pars$xaxs <- "i";
  }
  if((!(is.null(pars$log))) && (grepl("x", pars$log)) && is.null(pars$xaxt)) {
    pars$xaxt <- "n";
  }

  do.call(plot, pars);

  if((!is.null(pars$xaxt)) && (identical(pars$xaxt, "n"))) {
    seq <- as.integer(10L ^ seq.int(from=as.integer(log10(min(pars$x))),
                                    to=as.integer(log10(max(pars$x)))));
    axis(side = 1L, at = seq, labels = as.character(seq));
  }

  for(i in seq_along(data)) {
    algo <- data[[i]];
    color <- colors[[i]];

    for(a in algo) {
      lines(x=a$t, y=a$f,
            lty=1L, lwd=1L, type="s",
            col=color);
    }
  }

  labels <- unlist(c(names.name, instance));
  legend(x="topright",
         cex=1.1,
         legend=labels,
         col = c(colors, "white"),
         text.col = c(colors, "black"),
         lwd=c(rep(1L, length(colors)), 0L),
         lty=c(rep(1L, length(colors)), 0L),
         bty="n",
         box.lwd=0L);

  legend(x="topleft",
         cex=1.1,
         seg.len=0,
         legend="f",
         bty="n");

  legend(x="bottomright",
         cex=1.1,
         legend="t in ms",
         bty="n");
}

#' @title Plot the Progress over Time for a Set of Algorithms on all Instances
#' @description Plot the progress over time for a set of algorithms on all
#'   available instances.
#' @param config the configuration
#' @param setups the available setups
#' @param ... any parameters to be passed to the \code{plot} routine
#' @param name the file name (omit for auto-generate)
#' @param max.t the maximum time index (omit for 3 minutes, as in the AITOA
#'   book)
#' @return the fully-qualified path to the file
#' @include algorithm_parameters_frame.R
#' @include setups.R
#' @include utils.R
#' @importFrom plotteR colors.distinct
#' @importFrom graphics par
#' @export aitoa.plot.progress
aitoa.plot.progress <- function(config, setups, ..., name=NULL,
                                max.t=(3L*60L*1000L)) {
  stopifnot(length(setups) > 0L,
            is.integer(max.t),
            is.finite(max.t),
            max.t > 0L);
  setups <- unique(as.character(setups));
  stopifnot(length(setups) > 0L,
            all(is.character(setups)),
            all(nchar(setups) > 0));

  files <- aitoa.setups.frame(config);
  stopifnot(is.data.frame(files), nrow(files) > 0L);
  files.setup <- as.character(unname(unlist(files$algorithm)));
  stopifnot(length(files.setup) == nrow(files),
            all(nchar(files.setup) > 0L));
  files.instance <- as.character(unname(unlist(files$instance)));
  stopifnot(length(files.instance) == nrow(files),
            all(nchar(files.instance) > 0L));
  files.file <- as.character(unname(unlist(files$file)));
  stopifnot(length(files.file) == nrow(files),
            all(nchar(files.file) > 0L));
  rm("files");

  names <- aitoa.algorithm.parameters.frame(config);
  stopifnot(is.data.frame(names), nrow(names) > 0L);
  names.name <- as.character(unname(unlist(names$algo.name)));
  stopifnot(length(names.name) == nrow(names));
  pick <- !is.na(names.name);
  stopifnot(sum(pick) > 0L);
  names.name <- names.name[pick];
  stopifnot(length(names.name) > 0L,
            all(nchar(names.name) > 0L));
  names.setup <- as.character(unname(unlist(names$algo.setup)))[pick];
  stopifnot(length(names.setup) == length(names.name),
            all(nchar(names.setup) > 0L));
  rm("names");
  rm("pick");

  found <- vapply(setups, function(setup) {
    sel <- names.name == setup;
    stopifnot(sum(sel) == 1L);
    sel <- which(sel);
    stopifnot(length(sel) == 1L);
    return(sel[[1L]]);
  }, 1L);
  stopifnot(length(found) == length(setups));
  names.setup <- names.setup[found];
  names.name <- names.name[found];
  stopifnot(length(names.setup) == length(setups));

  found <- unname(unlist(lapply(names.setup,
                  function(setup) {
                    sel <- files.setup == setup;
                    stopifnot(sum(sel) >= (config$min.runs*config$min.instances));
                    sel <- which(sel);
                    stopifnot(length(sel) >= (config$min.runs*config$min.instances));
                    return(sort(sel));
                  })));

  files.setup <- files.setup[found];
  files.file <- files.file[found];
  files.instance <- files.instance[found];
  rm("found");
  gc();

  instances <- sort(unique(files.instance));
  stopifnot(length(instances) >= config$min.instances);
  i <- length(files.file)/(length(instances) * length(names.setup));
  stopifnot(as.integer(i) == i, i >= config$min.runs);

  stopifnot(length(instances) > 0L);
  config$logger("found ", length(instances), " instances for ",
                length(setups), " algorithm setups with in total ",
                length(files.file), " log files to plot.");

  if(is.null(name)) {
    name <- paste(setups, sep="_", collapse="_");
    name <- paste0("jssp_progress_", name);
    l <- list(...);
    if(length(l) > 0L) {
      name <- paste0(name, "_", paste(vapply(names(l), function(n) {
        paste(n, "=", l[[n]], sep="", collapse="")
      }, ""), sep="_", collapse="_"));
    }
  }
  stopifnot(nchar(name) > 0L);

  file <- file.path(.dir.plots("progress", config=config),
                    .graphics.name(config, name));

  colors <- colors.distinct(n=length(setups));

  res <- .graphic(config=config,
           path=file,
           width=6L,
           height=(2.15*length(instances)),
           expr = {
             mar <- 0.5*par()$mar;
             mar[3L] <- 0.25 * mar[3L];
             mar[1L] <- 0.85 * mar[1L];
             mar[4L] <- 0.15 * mar[4L];
             p1 <- par(cex=0.78, mar=mar);
             p2 <- par(mfrow=c(length(instances), 1L));
             for(instance in instances) {
               .plot.progress.inst(config,
                                   instance,
                                   names.setup,
                                   names.name,
                                   files.setup,
                                   files.instance,
                                   files.file,
                                   colors,
                                   ...,
                                   max.t=max.t);
             }
             par(p2);
             par(p1);
           });

  stopifnot(file.exists(res));
  config$logger("done plotting to file '", res, "', now post-processing.");
  res <- .post.process.plot.progress(res);
  stopifnot(file.exists(res));
  config$logger("done post-processing file '", res, "'.");
  return(res);
}
