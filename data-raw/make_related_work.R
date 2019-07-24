# Do the related work results

# make sure that 'make_instances.R' was included first and succeeded
stopifnot(exists("jsspInstances"),
          is.data.frame(jsspInstances),
          nrow(jsspInstances) > 0L,
          identical(colnames(jsspInstances),
                    c("inst.name",
                      "inst.jobs",
                      "inst.machines",
                      "inst.opt.bound.lower",
                      "inst.opt.bound.upper",
                      "inst.solutions.num")));

logger("setting related works directory.");
# create paths to directories
dir.related.work <- normalizePath(file.path(dir.data.raw, "related_work"), mustWork = TRUE);
stopifnot(dir.exists(dir.related.work));
dir.results <- normalizePath(file.path(dir.related.work, "results"), mustWork = TRUE);
stopifnot(dir.exists(dir.results));

# load list of algorithms
logger("loading algorithms data frame.");
algorithms.file <- normalizePath(file.path(dir.related.work, "algorithms.txt"), mustWork = TRUE);
stopifnot(file.exists(algorithms.file));
algorithms <- read.csv(algorithms.file, check.names = FALSE);
rm("algorithms.file");
stopifnot(is.data.frame(algorithms),
          nrow(algorithms) > 0L,
          identical(colnames(algorithms),
                    c("algorithm",
                      "paper",
                      "year",
                      "programming.language",
                      "system.processor.name",
                      "system.processor.cores",
                      "system.processor.threads",
                      "system.processor.MHz",
                      "system.ram.MB",
                      "system.os")),
          length(unique(algorithms$algorithm)) == nrow(algorithms),
          sum(is.na(algorithms$algorithm)) == 0L,
# cores
          all(is.integer(algorithms$system.processor.cores)),
          all(is.na(algorithms$system.processor.cores) |
              is.finite(algorithms$system.processor.cores)),
          all(is.na(algorithms$system.processor.cores) |
                (algorithms$system.processor.cores > 0L)),
# threads
          all(is.integer(algorithms$system.processor.threads)),
          all(is.na(algorithms$system.processor.threads) |
                is.finite(algorithms$system.processor.threads)),
          all(is.na(algorithms$system.processor.threads) |
                (algorithms$system.processor.threads > 0L)),
# mhz
          all(is.integer(algorithms$system.processor.MHz)),
          all(is.na(algorithms$system.processor.MHz) |
                is.finite(algorithms$system.processor.MHz)),
          all(is.na(algorithms$system.processor.MHz) |
                (algorithms$system.processor.MHz > 0L)),
# ram
          all(is.integer(algorithms$system.ram.MB)),
          all(is.na(algorithms$system.ram.MB) |
                is.finite(algorithms$system.ram.MB)),
          all(is.na(algorithms$system.ram.MB) |
                (algorithms$system.ram.MB > 0L)));
logger("loaded and validated algorithms frame, found ", nrow(algorithms), " algorithms.");


# load a single algorithm
load.algorithm <- function(algorithm.row) {
  stopifnot(is.factor(algorithm.row$algorithm),
            !is.na(algorithm.row$algorithm));
  # load algorithm
  algorithm <- as.character(algorithm.row$algorithm);
  logger("now loading algorithm '", algorithm, "'.");
  stopifnot(is.character(algorithm));
  algorithm.file <- normalizePath(file.path(dir.results, paste0(algorithm, ".txt")), mustWork = TRUE);
  stopifnot(file.exists(algorithm.file));
  algorithm.data <- read.csv(algorithm.file, check.names = FALSE, stringsAsFactors = FALSE);

  # check algorithm data
  stopifnot(is.data.frame(algorithm.data));
  cols.in <- colnames(algorithm.data);
  logger("loaded algorithm '", algorithm, "', found cols: ",
         paste(cols.in, sep=", ", collapse=", "));

  stopifnot(length(unique(cols.in)) == length(cols.in),
            length(cols.in) > 0L,
            sum(cols.in == "inst.name") == 1L,
            all(vapply(algorithm.data$inst.name,
                       function(inst) {
                         isTRUE(sum(jsspInstances$inst.name == inst) == 1L)
                       }, TRUE)));

  rows <- nrow(algorithm.data);
  stopifnot(rows > 0L);
  frame <- data.frame(inst.name=rep(NA_character_,rows),
                      inst.jobs=
                        vapply(algorithm.data$inst,
                               function(inst) {
                                 jsspInstances$inst.jobs[jsspInstances$inst.name == inst]
                               }, 0L),
                      inst.machines=
                        vapply(algorithm.data$inst,
                               function(inst) {
                                 jsspInstances$inst.machines[jsspInstances$inst.name == inst]
                               }, 0L),
                      inst.opt.bound.lower=
                        vapply(algorithm.data$inst,
                               function(inst) {
                                  jsspInstances$inst.opt.bound.lower[jsspInstances$inst.name == inst]
                                  }, 0L),
                      inst.opt.bound.upper=
                        vapply(algorithm.data$inst,
                               function(inst) {
                                 jsspInstances$inst.opt.bound.upper[jsspInstances$inst.name == inst]
                               }, 0L),
                      do.call(rbind, lapply(seq_len(rows), function(i) algorithm.row)),
                      n.runs=rep(NA_integer_,rows),
                      best.f.min=rep(NA_integer_, rows),
                      best.f.mean=rep(NA_real_, rows),
                      best.f.med=rep(NA_real_, rows),
                      best.f.max=rep(NA_integer_, rows),
                      best.f.sd=rep(NA_real_, rows),
                      last.improvement.time.min=rep(NA_integer_, rows),
                      last.improvement.time.mean=rep(NA_real_, rows),
                      last.improvement.time.med=rep(NA_real_, rows),
                      last.improvement.time.max=rep(NA_integer_, rows),
                      last.improvement.time.sd=rep(NA_real_, rows),
                      total.time.min=rep(NA_integer_, rows),
                      total.time.mean=rep(NA_integer_, rows),
                      total.time.med=rep(NA_real_, rows),
                      total.time.max=rep(NA_integer_, rows),
                      total.time.sd=rep(NA_real_, rows),
                      last.improvement.fes.min=rep(NA_integer_, rows),
                      last.improvement.fes.mean=rep(NA_real_, rows),
                      last.improvement.fes.med=rep(NA_real_, rows),
                      last.improvement.fes.max=rep(NA_integer_, rows),
                      last.improvement.fes.sd=rep(NA_real_, rows),
                      total.fes.min=rep(NA_integer_, rows),
                      total.fes.mean=rep(NA_integer_, rows),
                      total.fes.med=rep(NA_integer_, rows),
                      total.fes.max=rep(NA_integer_, rows),
                      total.fes.sd=rep(NA_real_, rows));

  cols.out <- colnames(frame);
  stopifnot(all(vapply(cols.in, function(cc) isTRUE(sum(cols.out == cc) == 1L), FALSE)));

  logger("constructing frame for algorithm '", algorithm, "'.");
  for(col in cols.in) {
    frame[col] <- unname(unlist(algorithm.data[col]));
  }

  logger("sanity-checking algorithm '", algorithm, "'.");
  stopifnot(all(frame$inst.jobs > 0L),
            all(frame$inst.machines > 0L),
            all(frame$inst.opt.bound.upper > 0L),
            all(frame$inst.opt.bound.lower > 0L),
            all(frame$inst.opt.bound.lower <= frame$inst.opt.bound.upper),
            all(is.finite(frame$inst.jobs)),
            all(is.integer(frame$inst.jobs)),
            all(is.finite(frame$inst.machines)),
            all(is.integer(frame$inst.machines)),
            all(is.finite(frame$inst.opt.bound.lower)),
            all(is.integer(frame$inst.opt.bound.lower)),
            all(is.finite(frame$inst.opt.bound.upper)),
            all(is.integer(frame$inst.opt.bound.upper)));

  cols.int <- c("n.runs",
                "best.f.min",
                "best.f.max",
                "last.improvement.time.min",
                "last.improvement.time.max",
                "total.time.min",
                "total.time.max",
                "last.improvement.fes.min",
                "last.improvement.fes.max",
                "total.fes.min",
                "total.fes.max");
  cols.real<- c("best.f.mean",
                "best.f.med",
                "best.f.sd",
                "last.improvement.time.mean",
                "last.improvement.time.med",
                "last.improvement.time.sd",
                "total.time.mean",
                "total.time.med",
                "total.time.sd",
                "last.improvement.fes.mean",
                "last.improvement.fes.med",
                "last.improvement.fes.sd",
                "total.fes.mean",
                "total.fes.med",
                "total.fes.sd");
  for(col in cols.real) {
    if(col %in% cols.in) {
      cc <- unlist(frame[col]);
      stopifnot(all(is.numeric(cc) | is.integer(cc)),
                all(is.finite(cc)),
                all(cc>= 0),
                all(!is.na(cc)));
    }
  }
  for(col in cols.int) {
    if(col %in% cols.in) {
      cc <- unlist(frame[col]);
      stopifnot(all(is.integer(cc)),
                all(is.finite(cc)),
                all(cc>= 0),
                all(!is.na(cc)));
    }
  }

  # check soundness of column groups
  # auto-insert values if possible
  for(choice in c("best.f", "last.improvement.time", "last.improvement.fes",
                  "total.time", "total.fes")) {
    col.min <- paste0(choice, ".min");
    col.mean <- paste0(choice, ".mean");
    col.med <- paste0(choice, ".med");
    col.max <- paste0(choice, ".max");
    col.sd <- paste0(choice, ".sd");

    should.repeat <- TRUE;
    while(should.repeat) {
      logger("checking data in group '", choice, "' for algorithm '", algorithm, "'.");
      should.repeat <- FALSE;
      if(col.min %in% cols.in) {

        if(col.mean %in% cols.in) {
          stopifnot(all(frame[col.mean] >= frame[col.min]));
        }
        if(col.med %in% cols.in) {
          stopifnot(all(frame[col.med] >= frame[col.min]));
        }
      }
      if(col.max %in% cols.in) {
        if(col.mean %in% cols.in) {
          stopifnot(all(frame[col.mean] <= frame[col.max]));
        }
        if(col.med %in% cols.in) {
          stopifnot(all(frame[col.med] <= frame[col.max]));
        }
        if(col.min %in% cols.in) {
          stopifnot(all(frame[col.max] >= frame[col.min]));

          same <- (frame[col.min] == frame[col.max]);
          same[is.na(same)] <- FALSE;

          if(any(same)) {
            if(!(col.mean %in% cols.in)) {
              old <- frame[col.mean][same];
              frame[col.mean][same] <- frame[col.min][same];
              if(!(identical(old, frame[col.mean][same]))) {
                should.repeat <- TRUE;
                should.repeat <- force(should.repeat);
                logger("added values for ", col.mean, " since ",
                       col.min, "==", col.max, " for algorithm '",
                       algorithm);
              }
            }
            if(!(col.med %in% cols.in)) {
              old <- frame[col.med][same];
              frame[col.med][same] <- frame[col.min][same];
              if(!(identical(old, frame[col.med][same]))) {
                should.repeat <- TRUE;
                should.repeat <- force(should.repeat);
                logger("added values for ", col.med, " since ",
                       col.min, "==", col.max, " for algorithm '",
                       algorithm);
              }
            }
            if(col.sd %in% cols.in) {
              stopifnot(frame[col.sd][same] == 0,
                        frame[col.sd][!same] > 0);
            } else {
              old <- frame[col.sd][same];
              frame[col.sd][same] <- 0L;
              if(!(identical(old, frame[col.sd][same]))) {
                should.repeat <- TRUE;
                should.repeat <- force(should.repeat);
                logger("added values for ", col.sd, " since ",
                       col.min, "==", col.max, " for algorithm '",
                       algorithm);
              }
            }
          }
        }
      }
      if(col.sd %in% cols.in) {
        same <- (frame[col.sd] <= 0);
        if( (col.min %in% cols.in) && (col.max %in% cols.in)) {
          stopifnot(all(frame[col.min][same] == frame[col.max][same]),
                        all(frame[col.min][!same] < frame[col.max][!same]));
        }
        if(any(same)) {
          col.exp <- c(col.min, col.max, col.mean, col.med);
          for(col.a in col.exp) {
            if(col.a %in% cols.in) {
              for(col.b in col.exp) {
                if(col.b %in% cols.in) {
                  stopifnot(all(frame[col.a][same] == frame[col.b][same]));
                } else {
                  old <- frame[col.b][same];
                  frame[col.b][same] <- frame[col.a][same];
                  if(!identical(old, frame[col.b][same])) {
                    should.repeat <- TRUE;
                    should.repeat <- force(should.repeat);
                    logger("added values for ", col.b, " from ", col.a,
                           " since ",
                           col.sd, "==0 for algorithm '",
                           algorithm);
                  }
                }
              }
            }
          }
        }
      }

      if(should.repeat) {
        logger("data in column group '", choice, "' for algorithm '",
               algorithm, "' was artificially expanded, repeating sanity check.");
      }
    }
  }

# sanity check against bounds
  for(choice in c("best.f.min", "best.f.mean", "best.f.med", "best.f.max")) {
    if(choice %in% cols.in) {
      stopifnot(frame[choice] >= frame$inst.opt.bound.lower,
                frame[choice] >= frame$inst.opt.bound.upper);
    }
  }


  logger("finished loading and processing data for algorithm '", algorithm, "'.");
  frame$inst.name <- as.character(unname(unlist(frame$inst.name)));
  rownames(frame) <- NULL;
  frame <- force(frame);
  return(frame);
}

# load all algorithms
jsspRelatedWorkResults <- lapply(seq_len(nrow(algorithms)), function(i) load.algorithm(algorithms[i,]));
rm("load.algorithm");
rm("dir.results");
rm("dir.related.work");
logger("all data loaded, now post-processng");

# merge all the data
jsspRelatedWorkResults <- do.call(rbind, jsspRelatedWorkResults);
jsspRelatedWorkResults <- force(jsspRelatedWorkResults);
rownames(jsspRelatedWorkResults) <- NULL;

stopifnot(is.data.frame(jsspRelatedWorkResults),
          nrow(jsspRelatedWorkResults) >= nrow(algorithms),
          all(vapply(algorithms$algorithm, function(a) isTRUE(sum(jsspRelatedWorkResults$algorithm == a) > 0L), TRUE)));
rm("algorithms");

# post processing

## bring columns into a nice order
.sort.order <- c("inst.name",
                 "best.f.mean",
                 "best.f.min",
                 "best.f.med",
                 "best.f.sd",
                 "best.f.max",
                 "last.improvement.time.mean",
                 "last.improvement.time.min",
                 "last.improvement.time.med",
                 "last.improvement.time.max",
                 "total.time.mean",
                 "total.time.min",
                 "total.time.med",
                 "total.time.max",
                 "system.processor.name",
                 "system.processor.MHz",
                 "system.ram.MB",
                 "system.processor.cores",
                 "system.processor.threads");
.sort.cols <- lapply(.sort.order[.sort.order %in% colnames(jsspRelatedWorkResults)],
                     function(n) jsspRelatedWorkResults[n]);

jsspRelatedWorkResults <- jsspRelatedWorkResults[do.call(order, .sort.cols),];
rownames(jsspRelatedWorkResults) <- NULL;
jsspRelatedWorkResults <- force(jsspRelatedWorkResults);

logger("completed construction of JSSP related work dataset.")
