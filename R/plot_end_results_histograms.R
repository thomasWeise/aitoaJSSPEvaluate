
#' @importFrom stats dnorm ks.test median quantile sd shapiro.test
#' @importFrom tseries jarque.bera.test
#' @importFrom graphics hist legend lines
#' @importFrom grDevices rgb
.end.result.histogram <- function(v, inst, put.leg=TRUE) {
  v <- as.integer(unname(unlist(v)));
  v.unique <- sort(unique(v));
  v.count <- vapply(v.unique, FUN=function(i) sum(v==i), 1L);

  stopifnot(sum(v.count) == length(v));
  breaks <- sort(unique(c((v.unique-0.5), (v.unique+0.5))));

  y.lim <- c(0, (max(v.count)/length(v)));

  mean <- mean(v);
  stopifnot(is.finite(mean),
            mean > 0);
  sd <- sd(v);
  stopifnot(is.finite(sd),
            sd >= 0);

  quan <- quantile(v, c(16/100, 0.5, 84/100));
  stopifnot(quan[[3L]] >= quan[[2L]],
            quan[[2L]] >= quan[[1L]],
            quan[[1L]] >= 0);
  median <- median(v);
  stopifnot(length(quan)==3L,
            all(is.finite(quan)),
            all(quan >= 0L),
            median == quan[2L]);

  mean.msd <- mean - sd;
  mean.psd <- mean + sd;

  x.lim <- range(c(v.unique, mean.msd, mean.psd, mean, quan));
  x.lim <- range(c(x.lim, x.lim[1] - (0.05*(x.lim[2]-x.lim[1])),
                   x.lim[2] + (0.05*(x.lim[2]-x.lim[1]))));

  norm.x <- seq(from=x.lim[1L], to=x.lim[2L], by=1/3);
  stopifnot(all(is.finite(norm.x)));
  norm.y <- dnorm(norm.x, mean=mean, sd=sd);
  stopifnot(all(is.finite(norm.y)));
  y.lim <- range(c(y.lim, norm.y));
  stopifnot(y.lim[[2L]] > y.lim[[1L]]);

  hist(v, breaks=breaks, freq=FALSE,
       xlim=x.lim, ylim=y.lim, xlab=NA, ylab=NA,
       main=NA,
       col="darkgrey",
       border=NA);

  .li <- function(text, color, lwd=3L) {
    return(list(text=text, col=color, lwd=lwd));
  }
  .lt <- function(l) l$text;
  .lc <- function(l) l$col;
  .lw <- function(l) l$lwd;

  l <- NULL;
  l$norm <- .li(expression(italic(N)(mean, sd)), "darkgreen");
  lines(norm.x, norm.y, col=.lc(l$norm), lwd=.lw(l$norm));

#  l$density <- .li(expression(paste("density estimate")), "lightgreen");
#  lines(density(v), col=.lc(l$density), lwd=.lw(l$density));

  l$mean.msd <- .li(expression(mean-sd), "orange");
  abline(v=mean.msd, col=.lc(l$mean.msd), lwd=.lw(l$mean.msd));
  l$mean <- .li(expression(mean), "red");
  abline(v=mean, col=.lc(l$mean), lwd=.lw(l$mean));
  l$mean.psd <- .li(expression(mean+sd), "violet");
  abline(v=mean.psd, col=.lc(l$mean.psd), lwd=.lw(l$mean.psd));

  l$q14 <- .li(expression(quantile[100L]^16L), "#4444ff");
  abline(v=quan[1], col=.lc(l$q14), lwd=.lw(l$q14));
  l$q24 <- .li(expression(median), "#0000ff");
  abline(v=quan[2], col=.lc(l$q24), lwd=.lw(l$q24));
  l$q34 <- .li(expression(quantile[100L]^84L), "#000088");
  abline(v=quan[3], col=.lc(l$q34), lwd=.lw(l$q34));


  col <- rgb(1, 1, 1, alpha=0.75);
  if(put.leg) {
    legend(x="topright",
           cex=0.9,
           legend=sapply(l, .lt),
           col = vapply(l, .lc, ""),
           text.col = vapply(l, .lc, ""),
           lwd=vapply(l, .lw, 1L),
           box.col=col,
           box.lwd=0L,
           bg=col);
  }

# legend(x="topleft",
#        cex=1.1,
#        seg.len=0,
#        legend="relative freqency",
#        box.col=col,
#        box.lwd=0L,
#        bg=col);

  legend(x="bottomright",
         cex=1.1,
         legend=expression(f[B]),
         box.col=col,
         box.lwd=0L,
         bg=col);

  is.norm <- "\u2714";
  is.not.norm <- "\u2718";

  shapiro.result <- shapiro.test(v)$p.value;
  stopifnot(is.finite(shapiro.result),
            shapiro.result >= 0,
            shapiro.result <= 1);
  shapiro <- paste0("Shapiro-Wilk: ", signif(shapiro.result, 3L), " ");
  if(shapiro.result > 0.05) {
    shapiro <- paste0(shapiro, is.norm);
  } else {
    shapiro <- paste0(shapiro, is.not.norm);
  }

  ks.result <- suppressWarnings(ks.test(v, "pnorm", mean, sd, exact=TRUE))$p.value;
  stopifnot(is.finite(ks.result),
            ks.result >= 0,
            ks.result <= 1);
  ks <- paste0("Kolmogorov-Smirnov: ", signif(ks.result, 3L), " ");
  if(ks.result > 0.05) {
    ks <- paste0(ks, is.norm);
  } else {
    ks <- paste0(ks, is.not.norm);
  }

  jb.result <- jarque.bera.test(v)$p.value;
  stopifnot(is.finite(jb.result),
            jb.result >= 0,
            jb.result <= 1);
  jb <- paste0("Jarque-Bera: ", signif(jb.result, 3L), " ");
  if(jb.result > 0.05) {
    jb <- paste0(jb, is.norm);
  } else {
    jb <- paste0(jb, is.not.norm);
  }


  legend(x="topleft",
         cex=0.9,
         legend=as.character(c("", "", "Normality tests: ", shapiro, ks, jb)),
         seg.len=0L,
         box.col=col,
         box.lwd=0L,
         bg=col);

  legend(x="topleft",
         cex=1.1,
         legend=substitute(expression(count(f[B]) / n), list(n=length(v)))[[2L]],
         seg.len=0L,
         box.col=NA,
         box.lwd=0L,
         bg=col,
         adj=c(0.15, 0.5));

  if(!(is.na(inst) || is.null(inst) || (nchar(inst) <= 0L))) {
    col <- rgb(0.89, 0.89, 0.89, alpha=0.75);
    legend(x=0.5*(x.lim[1]+x.lim[2]),
           y=0.07*(y.lim[1]+y.lim[2]),
           xjust=0.5,
           yjust=0.5,
           adj=c(0.3,0.4),
           cex=1.66,
           legend=inst,
           box.col=col,
           box.lwd=0L,
           bg=col,
           seg.len=0L);
  }
}


#' @title Plot End-Result Histograms
#' @description Plot histograms showing the relative frequencies of the end
#'   result solution qualities.
#' @param config the configuration object
#' @return a list of files that represent these diagrams
#' @include end_results.R
#' @include utils.R
#' @include graphics.R
#' @include get_names.R
#' @importFrom plotteR colors.distinct
#' @export aitoa.plot.end.results.histograms
aitoa.plot.end.results.histograms <- function(config=aitoa.config()) {
  config$logger("beginning to plot end result histograms");

  end.results <- aitoa.end.results.frame(config);
  stopifnot(is.data.frame(end.results),
            nrow(end.results) > 0L);

  setups <- unique(unname(unlist(end.results$algorithm)));
  stopifnot(length(setups) > 0L,
            nrow(end.results) >= (config$min.runs*config$min.instances*length(setups)));
  names <- .get.setup.names(setups, config);
  stopifnot(length(names) == length(setups));
  sel <- !is.na(names);
  names <- names[sel];
  setups <- setups[sel];
  stopifnot(length(names) > 0L,
            length(setups) == length(names));
  rm("sel");

  config$logger("found ", length(setups), " setups");

  goal.length <- length(setups);
  stopifnot(goal.length > 0L);
  paths <- character(goal.length);
  index <- 0L;
  for(i in seq_along(setups)) {
    setup <- setups[[i]];
    name <- names[[i]];

    end.results.setup <- end.results[end.results$algorithm == setup, ];
    stopifnot(nrow(end.results.setup) > 0L,
              nrow(end.results.setup) >= config$min.runs*config$min.instances);

    instances <- unique(unlist(end.results.setup$instance));
    length.instances <- length(instances);

    stopifnot(length.instances > 0L,
              length.instances >= config$min.instances);
    config$logger("found ", length.instances, " instances for setup ", name);

    file <- file.path(.dir.plots("endResultHistograms", config=config),
                      .graphics.name(config=config,
                                     paste0(name, "_endResultHistogram")));
    file <- .graphic(config=config,
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
               put.legend <- TRUE;
               for(inst in instances) {
                 .end.result.histogram(end.results.setup[end.results.setup$instance == inst,]$best.f,
                                       as.character(inst),
                                       put.legend);
                 put.legend <- FALSE;
               }
               par(p2);
               par(p1);
             });

    index <- index + 1L;
    paths[[index]] <- file;
    config$logger("done plotting ", file, " for setup ", name);
    rm(end.results.setup);
    gc();
  }

  stopifnot(index == goal.length);
  return(paths);
}
