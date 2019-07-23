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

# create paths to directories
dir.related.work <- normalizePath(file.path(dir.data.raw, "related_work"), mustWork = TRUE);
stopifnot(dir.exists(dir.related.work));
dir.results <- normalizePath(file.path(dir.related.work, "results"), mustWork = TRUE);
stopifnot(dir.exists(dir.results));

# load list of algorithms
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
          sum(is.na(algorithms$algorithm)) == 0L);

# load a single algorithm
load.algorithm <- function(algorithm) {
  stopifnot(is.factor(algorithm),
            !is.na(algorithm));
  # load algorithm
  algorithm <- as.character(algorithm);
  stopifnot(is.character(algorithm));
  algorithm.file <- normalizePath(file.path(dir.results, paste0(algorithm, ".txt")), mustWork = TRUE);
  stopifnot(file.exists(algorithm.file));
  algorithm.data <- read.csv(algorithm.file, check.names = FALSE, stringsAsFactors = FALSE);

  # check algorithm data
  stopifnot(is.data.frame(algorithm.data),
            nrow(algorithm.data)>0L);
  cols <- colnames(algorithm.data);
  stopifnot(length(unique(cols)) == length(cols),
            length(cols) > 0L,
            sum(cols == "instance") == 1L);
  stopifnot(all(vapply(algorithm.data$instance,
                   function(inst) {
                     isTRUE(sum(jsspInstances$inst.name == inst) == 1L)
                   }, TRUE)));

  inst.opt.bound.lower <- vapply(algorithm.data$instance,
                             function(inst) {
                               jsspInstances$inst.opt.bound.lower[jsspInstances$inst.name == inst]
                             }, 0L);

  inst.opt.bound.upper <- vapply(algorithm.data$instance,
                                 function(inst) {
                                   jsspInstances$inst.opt.bound.upper[jsspInstances$inst.name == inst]
                                 }, 0L);

  inst.jobs <- vapply(algorithm.data$instance,
                                 function(inst) {
                                   jsspInstances$inst.jobs[jsspInstances$inst.name == inst]
                                 }, 0L);

  inst.machines <- vapply(algorithm.data$instance,
                      function(inst) {
                        jsspInstances$inst.machines[jsspInstances$inst.name == inst]
                      }, 0L);

  stopifnot(all(inst.jobs > 0L),
            all(inst.machines > 0L),
            all(inst.opt.bound.upper > 0L),
            all(inst.opt.bound.lower > 0L),
            all(inst.opt.bound.lower <= inst.opt.bound.upper),
            all(is.finite(inst.jobs)),
            all(is.integer(inst.jobs)),
            all(is.finite(inst.machines)),
            all(is.integer(inst.machines)),
            all(is.finite(inst.opt.bound.lower)),
            all(is.integer(inst.opt.bound.lower)),
            all(is.finite(inst.opt.bound.upper)),
            all(is.integer(inst.opt.bound.upper)));

  if(any(colnames(algorithm.data) == "n.runs")) {
    stopifnot(all(is.integer(algorithm.data$n.runs)),
              all(algorithm.data$n.runs > 0L),
              all(!is.na(algorithm.data$n.runs)),
              all(is.finite(algorithm.data$n.runs)));
  }
  if(any(colnames(algorithm.data) == "best.f.min")) {
    stopifnot(all(is.integer(algorithm.data$best.f.min)),
              all(algorithm.data$best.f.min > 0L),
              all(algorithm.data$best.f.min >= inst.opt.bound.upper),
              all(!is.na(algorithm.data$best.f.min)),
              all(is.finite(algorithm.data$best.f.min)));
  }
  if(any(colnames(algorithm.data) == "best.f.max")) {
    stopifnot(all(is.integer(algorithm.data$best.f.max)),
              all(algorithm.data$best.f.max > 0L),
              all(algorithm.data$best.f.max >= inst.opt.bound.upper),
              all(!is.na(algorithm.data$best.f.max)),
              all(is.finite(algorithm.data$best.f.max)));
    if(any(colnames(algorithm.data) == "best.f.min")) {
      stopifnot(all(algorithm.data$best.f.min <= algorithm.data$best.f.max));
    }
  }
  if(any(colnames(algorithm.data) == "best.f.med")) {
    stopifnot(all(algorithm.data$best.f.med > 0L),
              all(algorithm.data$best.f.med >= inst.opt.bound.upper),
              all(!is.na(algorithm.data$best.f.med)),
              all(is.finite(algorithm.data$best.f.med)));
    if(any(colnames(algorithm.data) == "best.f.min")) {
      stopifnot(all(algorithm.data$best.f.min <= algorithm.data$best.f.med));
    }
    if(any(colnames(algorithm.data) == "best.f.max")) {
      stopifnot(all(algorithm.data$best.f.max >= algorithm.data$best.f.med));
    }
  }
  if(any(colnames(algorithm.data) == "best.f.mean")) {
    stopifnot(all(algorithm.data$best.f.mean > 0L),
              all(algorithm.data$best.f.mean >= inst.opt.bound.upper),
              all(!is.na(algorithm.data$best.f.mean)),
              all(is.finite(algorithm.data$best.f.mean)));
    if(any(colnames(algorithm.data) == "best.f.min")) {
      stopifnot(all(algorithm.data$best.f.min <= algorithm.data$best.f.mean));
    }
    if(any(colnames(algorithm.data) == "best.f.max")) {
      stopifnot(all(algorithm.data$best.f.max >= algorithm.data$best.f.mean));
    }
  }

  if(any(colnames(algorithm.data) == "best.f.sd")) {
    stopifnot(all(algorithm.data$best.f.sd >= 0),
              all(!is.na(algorithm.data$best.f.sd)),
              all(is.finite(algorithm.data$best.f.sd)));
    if(any(colnames(algorithm.data) == "best.f.min") && any(colnames(algorithm.data) == "best.f.max")) {
      stopifnot(all((algorithm.data$best.f.sd > 0) == (algorithm.data$best.f.min < algorithm.data$best.f.max)));
    }
  }

  algorithm.data <- cbind(data.frame(inst.jobs, inst.machines,
                                     #inst.opt.bound.lower,
                                     inst.opt.bound.upper),
                                     algorithm.data);
  algorithm.data$instance <- as.character(unname(unlist(algorithm.data$instance)));
  rownames(algorithm.data) <- NULL;
  algorithm.data <- force(algorithm.data);

  return(algorithm.data);
}

# load all algorithms
algorithms.data <- lapply(algorithms$algorithm, load.algorithm);
rm("load.algorithm");
rm("dir.results");
rm("dir.related.work");

# merge the columns
algorithms.data.cols <- unique(as.character(unlist(lapply(algorithms.data, colnames))));
stopifnot(length(algorithms.data.cols) > 0L,
          sum(algorithms.data.cols == "instance") == 1L);
algorithms <- algorithms[, vapply(colnames(algorithms),
                                  function(col) sum(!is.na(algorithms[col])) > 0L,
                                  TRUE)];
stopifnot(ncol(algorithms) > 0L,
          sum(colnames(algorithms) == "algorithm") == 1L,
          sum(colnames(algorithms) == "paper") == 1L);

# merge the data
merge.data <- function(i) {
  data <- algorithms.data[[i]];
  stopifnot(is.data.frame(data),
            nrow(data) > 0L,
            !is.null(data),
            ncol(data) <= length(algorithms.data.cols));
  cols <- colnames(data);
  for(use in algorithms.data.cols) {
    if(sum(cols == use) <= 0L) {
      data[use] <- rep(NA, nrow(data));
      data <- force(data);
    }
  }
  stopifnot(ncol(data) == length(algorithms.data.cols));
  algo.col <- algorithms[i, ];
  stopifnot(is.data.frame(algo.col));
  algo.col <- do.call(rbind, lapply(seq_len(nrow(data)), function(x) algo.col));
  algo.col <- force(algo.col);
  stopifnot(nrow(algo.col) == nrow(data));
  result <- cbind(algo.col, data);
  result <- force(result);

  stopifnot(nrow(result) == nrow(data),
            nrow(result) > 0L,
            length(result$algorithm) == nrow(result),
            length(unique(result$algorithm)) == 1L,
            sum(is.na(result$algorithm)) == 0L,
            length(result$instance) == nrow(result),
            length(unique(result$instance)) == nrow(result));
  return(result);
}

# merge all the data
jsspRelatedWorkResults <- do.call(rbind, lapply(seq_len(nrow(algorithms)), merge.data));
jsspRelatedWorkResults <- force(jsspRelatedWorkResults);
rownames(jsspRelatedWorkResults) <- NULL;
rm("merge.data");

stopifnot(is.data.frame(jsspRelatedWorkResults),
          ncol(jsspRelatedWorkResults) == (length(algorithms.data.cols) + ncol(algorithms)),
          nrow(jsspRelatedWorkResults) >= nrow(algorithms),
          all(vapply(algorithms$algorithm, function(a) isTRUE(sum(jsspRelatedWorkResults$algorithm == a) > 0L), TRUE)));

rm("algorithms.data.cols");
rm("algorithms");

# post processing

## bring columns into a nice order
.ideal.order <- c("instance",
                  "inst.jobs",
                  "inst.machines",
                  "inst.opt.bound.lower",
                  "inst.opt.bound.upper",
                  "algorithm",
                  "paper",
                  "year",
                  "programming.language",
                  "system.processor.name",
                  "system.processor.cores",
                  "system.processor.threads",
                  "system.processor.MHz",
                  "system.ram.MB",
                  "system.os",
                  "n.runs",
                  "best.f.min",
                  "best.f.mean",
                  "best.f.med",
                  "best.f.max",
                  "best.f.sd",
                  "last.improvement.time.min",
                  "total.time.mean");

cols <- colnames(jsspRelatedWorkResults);
stopifnot(length(cols) <= .ideal.order,
          all(cols %in% .ideal.order));
cols.o <- vapply(cols, function(x) which(.ideal.order == x)[[1L]], 1L);
stopifnot(length(cols.o) == length(cols),
          length(cols.o) == length(unique(cols.o)));
jsspRelatedWorkResults <- jsspRelatedWorkResults[, order(cols.o)];
jsspRelatedWorkResults <- force(jsspRelatedWorkResults);
colnames(jsspRelatedWorkResults)[[1L]] <- "inst.name";
jsspRelatedWorkResults <- force(jsspRelatedWorkResults);

rm("cols");
rm("cols.o");
rm(".ideal.order");

## bring columns into a nice order
.sort.order <- c("inst.name",
                 "algorithm",
                 "best.f.mean",
                 "best.f.min",
                 "best.f.sd",
                 "best.f.med",
                 "best.f.max",
                 "last.improvement.time.min",
                 "total.time.mean",
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
