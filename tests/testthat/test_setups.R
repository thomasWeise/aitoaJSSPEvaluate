library("aitoaEvaluate");
library("testthat");
context("aitoa.setups.frame");

.make.log.file <- function(dir, values, consumedFEs, consumedTime, algo, inst, seed) {
  file <- file.path(tmpdir = dir, paste(algo, "_", inst, "_", seed, ".txt", sep="", collapse=""));
  writeLines(text=c(
    "",
    "# ALGORITHM_SETUP",
    "algorithm: rs",
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
    "# SOLUTION_SPACE: jssp:gantt:abz5:aitoa.examples.jssp.JSSPSolutionSpace",
    "# REPRESENTATION_MAPPING: jssp:int[]-to-Gantt:aitoa.examples.jssp.JSSPRepresentationMapping",
    "# OBJECTIVE_FUNCTION: jssp:makespan:aitoa.examples.jssp.JSSPMakespanObjectiveFunction",
    "# MAX_FES: 9223372036854775807",
    "# MAX_TIME: 240000",
    "# GOAL_F: 1000.0",
    "# RANDOM_SEED: 0x1d831acc2bbfcf63",
    "# END_SETUP",
    "# BEGIN_SYSTEM",
    "# JAVA_VERSION: 13",
    "# JAVA_VENDOR: Ubuntu",
    "# JAVA_VM_VERSION: 13+13-Ubuntu-0ubunt1",
    "# JAVA_VM_VENDOR: Ubuntu",
    "# JAVA_VM_NAME: OpenJDK 64-Bit Server VM",
    "# JAVA_SPECIFICATION_VERSION: 13",
    "# JAVA_SPECIFICATION_VENDOR: Oracle Corporation",
    "# JAVA_SPECIFICATION_NAME: Java Platform API Specification",
    "# JAVA_COMPILER: null",
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
    "new aitoa.examples.jssp.JSSPCandidateSolution(new int[][] {",
    "{ 8, 0, 94, 9, 94, 153, 2, 217, 300, 6, 583, 678, 4, 678, 777, 3, 821, 897, 1, 929, 1021, 7, 1021, 1083, 5, 1083, 1171, 0, 1171, 1257},",
    "{ 5, 0, 99, 9, 153, 235, 6, 235, 321, 8, 335, 401, 2, 401, 466, 3, 466, 527, 7, 542, 613, 0, 613, 680, 1, 1021, 1103, 4, 1264, 1350},",
    "{ 3, 94, 162, 8, 401, 491, 7, 491, 542, 5, 565, 645, 0, 680, 769, 1, 769, 863, 6, 863, 929, 4, 929, 996, 2, 996, 1051, 9, 1218, 1314},",
    "{ 9, 0, 50, 4, 50, 119, 1, 119, 169, 8, 169, 250, 7, 405, 487, 6, 487, 583, 3, 626, 680, 5, 843, 922, 2, 1051, 1128, 0, 1257, 1349},",
    "{ 0, 0, 88, 7, 88, 186, 4, 186, 274, 5, 274, 355, 6, 355, 452, 1, 452, 527, 3, 527, 626, 8, 626, 702, 2, 712, 797, 9, 797, 878},",
    "{ 1, 0, 72, 5, 355, 419, 0, 419, 518, 2, 549, 634, 7, 634, 728, 3, 755, 821, 8, 821, 879, 6, 929, 1028, 4, 1091, 1159, 9, 1159, 1218},",
    "{ 8, 94, 165, 1, 169, 238, 0, 238, 332, 7, 332, 405, 5, 419, 485, 2, 485, 549, 9, 549, 607, 3, 680, 755, 4, 996, 1091, 6, 1091, 1143},",
    "{ 3, 0, 94, 6, 94, 144, 8, 250, 335, 9, 335, 391, 2, 634, 712, 5, 712, 781, 7, 781, 866, 0, 920, 1019, 1, 1103, 1197, 4, 1197, 1264},",
    "{ 0, 88, 156, 2, 156, 217, 9, 235, 302, 4, 356, 451, 5, 485, 565, 6, 678, 775, 1, 863, 929, 8, 929, 1022, 3, 1022, 1089, 7, 1089, 1184},",
    "{ 2, 0, 83, 4, 274, 356, 9, 391, 487, 5, 781, 843, 0, 843, 920, 3, 920, 983, 8, 1022, 1119, 6, 1143, 1214, 1, 1214, 1277, 7, 1277, 1356}})",
    "",
    "if(!(require(\"plotteR\"))){",
    "if(!(require(\"devtools\"))){",
    "install.packages(\"devtools\");",
    "library(\"devtools\");",
    "};",
    "devtools::install_github(\"thomasWeise/plotteR\");",
    "library(\"plotteR\");",
    "};",
    "plot.gantt(list(",
    " list( list(job=8L,start=0L,end=94L)",
    ",list(job=9L,start=94L,end=153L)",
    ",list(job=2L,start=217L,end=300L)",
    ",list(job=6L,start=583L,end=678L)",
    ",list(job=4L,start=678L,end=777L)",
    ",list(job=3L,start=821L,end=897L)",
    ",list(job=1L,start=929L,end=1021L)",
    ",list(job=7L,start=1021L,end=1083L)",
    ",list(job=5L,start=1083L,end=1171L)",
    ",list(job=0L,start=1171L,end=1257L)",
    ")",
    ",list( list(job=5L,start=0L,end=99L)",
    ",list(job=9L,start=153L,end=235L)",
    ",list(job=6L,start=235L,end=321L)",
    ",list(job=8L,start=335L,end=401L)",
    ",list(job=2L,start=401L,end=466L)",
    ",list(job=3L,start=466L,end=527L)",
    ",list(job=7L,start=542L,end=613L)",
    ",list(job=0L,start=613L,end=680L)",
    ",list(job=1L,start=1021L,end=1103L)",
    ",list(job=4L,start=1264L,end=1350L)",
    ")",
    ",list( list(job=3L,start=94L,end=162L)",
    ",list(job=8L,start=401L,end=491L)",
    ",list(job=7L,start=491L,end=542L)",
    ",list(job=5L,start=565L,end=645L)",
    ",list(job=0L,start=680L,end=769L)",
    ",list(job=1L,start=769L,end=863L)",
    ",list(job=6L,start=863L,end=929L)",
    ",list(job=4L,start=929L,end=996L)",
    ",list(job=2L,start=996L,end=1051L)",
    ",list(job=9L,start=1218L,end=1314L)",
    ")",
    ",list( list(job=9L,start=0L,end=50L)",
    ",list(job=4L,start=50L,end=119L)",
    ",list(job=1L,start=119L,end=169L)",
    ",list(job=8L,start=169L,end=250L)",
    ",list(job=7L,start=405L,end=487L)",
    ",list(job=6L,start=487L,end=583L)",
    ",list(job=3L,start=626L,end=680L)",
    ",list(job=5L,start=843L,end=922L)",
    ",list(job=2L,start=1051L,end=1128L)",
    ",list(job=0L,start=1257L,end=1349L)",
    ")",
    ",list( list(job=0L,start=0L,end=88L)",
    ",list(job=7L,start=88L,end=186L)",
    ",list(job=4L,start=186L,end=274L)",
    ",list(job=5L,start=274L,end=355L)",
    ",list(job=6L,start=355L,end=452L)",
    ",list(job=1L,start=452L,end=527L)",
    ",list(job=3L,start=527L,end=626L)",
    ",list(job=8L,start=626L,end=702L)",
    ",list(job=2L,start=712L,end=797L)",
    ",list(job=9L,start=797L,end=878L)",
    ")",
    ",list( list(job=1L,start=0L,end=72L)",
    ",list(job=5L,start=355L,end=419L)",
    ",list(job=0L,start=419L,end=518L)",
    ",list(job=2L,start=549L,end=634L)",
    ",list(job=7L,start=634L,end=728L)",
    ",list(job=3L,start=755L,end=821L)",
    ",list(job=8L,start=821L,end=879L)",
    ",list(job=6L,start=929L,end=1028L)",
    ",list(job=4L,start=1091L,end=1159L)",
    ",list(job=9L,start=1159L,end=1218L)",
    ")",
    ",list( list(job=8L,start=94L,end=165L)",
    ",list(job=1L,start=169L,end=238L)",
    ",list(job=0L,start=238L,end=332L)",
    ",list(job=7L,start=332L,end=405L)",
    ",list(job=5L,start=419L,end=485L)",
    ",list(job=2L,start=485L,end=549L)",
    ",list(job=9L,start=549L,end=607L)",
    ",list(job=3L,start=680L,end=755L)",
    ",list(job=4L,start=996L,end=1091L)",
    ",list(job=6L,start=1091L,end=1143L)",
    ")",
    ",list( list(job=3L,start=0L,end=94L)",
    ",list(job=6L,start=94L,end=144L)",
    ",list(job=8L,start=250L,end=335L)",
    ",list(job=9L,start=335L,end=391L)",
    ",list(job=2L,start=634L,end=712L)",
    ",list(job=5L,start=712L,end=781L)",
    ",list(job=7L,start=781L,end=866L)",
    ",list(job=0L,start=920L,end=1019L)",
    ",list(job=1L,start=1103L,end=1197L)",
    ",list(job=4L,start=1197L,end=1264L)",
    ")",
    ",list( list(job=0L,start=88L,end=156L)",
    ",list(job=2L,start=156L,end=217L)",
    ",list(job=9L,start=235L,end=302L)",
    ",list(job=4L,start=356L,end=451L)",
    ",list(job=5L,start=485L,end=565L)",
    ",list(job=6L,start=678L,end=775L)",
    ",list(job=1L,start=863L,end=929L)",
    ",list(job=8L,start=929L,end=1022L)",
    ",list(job=3L,start=1022L,end=1089L)",
    ",list(job=7L,start=1089L,end=1184L)",
    ")",
    ",list( list(job=2L,start=0L,end=83L)",
    ",list(job=4L,start=274L,end=356L)",
    ",list(job=9L,start=391L,end=487L)",
    ",list(job=5L,start=781L,end=843L)",
    ",list(job=0L,start=843L,end=920L)",
    ",list(job=3L,start=920L,end=983L)",
    ",list(job=8L,start=1022L,end=1119L)",
    ",list(job=6L,start=1143L,end=1214L)",
    ",list(job=1L,start=1214L,end=1277L)",
    ",list(job=7L,start=1277L,end=1356L)",
    ")",
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
  expect_identical(unique(as.character(setups$algorithm)),
                   c("a", "b", "c", "x"));


  expect_identical(unique(as.character(setups$instance)),
                   c("1", "2", "3"));

  expect_identical(unique(as.character(setups$dir)),
                   c("a/1", "a/2", "a/3",
                     "b/1", "b/2", "b/3",
                     "c/1", "c/2", "c/3",
                     "x/1", "x/2", "x/3"));

  unlink(root, force=TRUE, recursive=TRUE);
  expect_false(dir.exists(root));

})

