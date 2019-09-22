library("aitoaEvaluate");
library("testthat");
context("aitoa.setups.frame");

.make.log.file <- function(dir, values, consumedFEs, consumedTime, algo, inst, seed) {
  file <- file.path(tmpdir = dir, paste(algo, "_", inst, "_", seed, ".txt", sep="", collapse=""));
  writeLines(text=c(
    "",
    "# ALGORITHM_SETUP",
    "algo.id: rs",
    "algorithm_class: aitoa.algorithms.RandomSampling",
    "# END_ALGORITHM_SETUP",
    "",
    "# BEGIN_LOG",
    "# fbest;consumedFEs;consumedTimeMS",
    vapply(values, function(v) paste(v, sep=";", collapse=";"), ""),
    "# END_OF_LOG",
    "",
    "# BEGIN_SETUP",
    "# SEARCH_SPACE: jssp:int[100]:abz5:aitoa.examples.jssp.JSSPSearchSpace",
    "# NULLARY_OP: uniform",
    "# UNARY_OP: null",
    "# BINARY_OP: null",
    "# TERNARY_OP: null",
    "# END_SYSTEM",
    "# BEGIN_STATE",
    paste("# CONSUMED_FES: ", consumedFEs, sep="", collapse=""),
    "# LAST_IMPROVEMENT_FE: 26622668",
    paste("# CONSUMED_TIME: ", consumedTime, sep="", collapse=""),
    "# LAST_IMPROVEMENT_TIME: 126902",
    "# BEST_F: 1356.0",
    "# END_STATE",
    "",
    "# BEST_X",
    "new int[]{ 8, 5, 9, 3, 8, 0, 0, 9, 2, 1, 7, 4, 1, 9, 2, 6, 4, 5, 3, 8, 1, 8, 5, 0, 2, 7, 9, 6, 6, 5, 8, 7, 0, 2, 6, 4, 9, 1, 8, 7, 9, 6, 4, 3, 7, 2, 2, 2, 3, 3, 7, 0, 9, 5, 5, 0, 4, 3, 5, 7, 8, 6, 3, 3, 5, 2, 1, 0, 1, 3, 1, 5, 6, 8, 4, 6, 4, 8, 0, 3, 7, 8, 9, 7, 6, 1, 4, 6, 9, 1, 4, 1, 2, 7, 2, 5, 0, 0, 9, 4}",
    "# END_BEST_X",
    "# BEST_Y",
    "plot.gantt(list(",
    " list( list(job=8L,start=0L,end=94L)",
    ",list(job=9L,start=94L,end=153L)",
    ",list(job=2L,start=217L,end=300L)",
    ",list(job=6L,start=583L,end=678L)",
    "), prefix.job=\"\");",
    "# END_BEST_Y"
  ), con=file);
  return(file);
}

.make.log.file.rnd <- function(dir,  algo, inst, seed) {
  consumedFEs <- as.integer(runif(n=1L, min=100, max=2000000));
  consumedTime <- as.integer(runif(n=1L, min=100000, max=2000000));

  t <- sort(unique(as.integer(runif(n=as.integer(runif(n=1, min=1L, max=100L)), min=1L, max=consumedTime))));
  fes <- sort(unique(as.integer(runif(n=as.integer(runif(n=1, min=1L, max=length(t))), min=1L, max=consumedFEs))));
  f <- unique(as.integer(runif(n=as.integer(runif(n=1, min=1L, max=length(fes))), min=100L, max=3425532L)));

  t <- t[1L:length(f)];
  fes <- fes[1L:length(f)];
  f <- f[order(-f)];

  return(.make.log.file(dir, values=lapply(seq_along(f), function(i) c(f[[i]], fes[[i]], t[[i]])),
                        consumedFEs = consumedFEs+1L, consumedTime=consumedTime+1L,
                        algo=algo, inst=inst, seed=seed));
}

test_that("Test aitoa.setups.frame", {
  root <- tempfile();
  dir.create(root, recursive=TRUE);
  results <- tempfile(tmpdir = root);
  dir.create(results, recursive=TRUE);
  eval <- tempfile(tmpdir = root);
  dir.create(eval, recursive=TRUE);


  algos <- c("a", "b", "c");
  insts <- c("1", "2", "3");

  config <- aitoa.config(dir.results=results, dir.evaluation = eval,
                         min.runs = 5L, min.instances = length(insts));
  repeat {
    seeds <- unique(as.integer(runif(n=config$min.runs, min=1L, max=120034L)));
    if(length(seeds) == config$min.runs) { break; }
  }

  for(algo in algos) {
    for(inst in insts) {
      d <- file.path(results, algo, inst);
      dir.create(d, recursive = TRUE);

      for(i in seeds) {
        f <- .make.log.file.rnd(d, algo, inst, i);
        stopifnot(file.exists(f),
                  startsWith(f, d));
      }
    }
  }

  algo <- "x";
  for(inst in c(insts, "q")) {
    d <- file.path(results, algo, inst);
    dir.create(d, recursive = TRUE);

    for(i in seeds) {
      f <- .make.log.file.rnd(d, algo, inst, i);
      stopifnot(file.exists(f),
                startsWith(f, d));
    }
  }

  algo <- "y";
  for(inst in insts[2L:length(insts)]) {
    d <- file.path(results, algo, inst);
    dir.create(d, recursive = TRUE);

    for(i in seeds) {
      f <- .make.log.file.rnd(d, algo, inst, i);
      stopifnot(file.exists(f),
                startsWith(f, d));
    }
  }

  algo <- "z";
  for(inst in c("v", insts)) {
    d <- file.path(results, algo, inst);
    dir.create(d, recursive = TRUE);

    for(i in c(max(seeds)+1L, seeds[2L:length(seeds)])) {
      f <- .make.log.file.rnd(d, algo, inst, i);
      stopifnot(file.exists(f),
                startsWith(f, d));
    }
  }

  setups <- aitoa.setups.frame(config=config);
  expect_identical(unique(as.character(setups$algo.id)),
                   c("a", "b", "c", "x"));


  expect_identical(unique(as.character(setups$inst.id)),
                   c("1", "2", "3"));

  expect_identical(unique(as.character(setups$dir)),
                   c("a/1", "a/2", "a/3",
                     "b/1", "b/2", "b/3",
                     "c/1", "c/2", "c/3",
                     "x/1", "x/2", "x/3"));

  unlink(root, force=TRUE, recursive=TRUE);
  expect_false(dir.exists(root));

})
