# each algorithm setup method returns either NULL or a list of setup features

.algo.algorithm <- "algo.algorithm";
.algo.algorithm.rs <- "rs";
.algo.algorithm.hc <- "hc";
.algo.algorithm.sa <- "sa";
.algo.algorithm.ea <- "ea";
.algo.algorithm.ma <- "ma";
.algo.algorithm.eda <- "eda";
.algo.algorithm.heda <- "heda";
.algo.algorithm.hc2 <- "hc2";

.algo.representation <- "algo.representation";
.algo.representation.default <- "default";
.algo.representation.tree <- "tree";

.algo.operator.unary <- "algo.operator.unary";
.algo.operator.unary.tree <- "tree1";
.algo.operator.binary <- "algo.operator.binary";
.algo.operator.binary.tree <- "tree2";

.algo.restarts <- "algo.restarts";
.algo.restarts.strategy <- "algo.restarts.strategy";
.algo.restarts.strategyHC2 <- "after.enumeration";
.algo.restarts.parameter <- "algo.restarts.parameter";

.algo.temperature.schedule <- "algo.temperature.schedule";
.algo.temperature.start <- "algo.temperature.start";
.algo.temperature.parameter <- "algo.temperature.parameter";

.algo.mu <- "algo.mu";
.algo.lambda <- "algo.lambda";
.algo.binary.rate <- "algo.binary.rate";

.algo.fitness <- "algo.fitness";
#.algo.fitness.ffa <- "ffa";
.algo.fitness.direct <- "direct";
.algo.fitness.pruning <- "pruning";

.algo.is.hybrid <- "algo.is.hybrid";
.algo.hybrid.global.search <- "algo.hybrid.global.search";

.algo.model <- "algo.model";
.algo.max.tree.depth <- "algo.max.tree.depth";

.make.list <- function(keys, ...) {
  r <- list(...);
  names(r) <- as.character(keys);
  return(r);
}

.parse.double <- function(s) {
  s <- gsub("d", ".", s, fixed=TRUE);
  s <- as.double(s);
  stopifnot(is.finite(s),
            is.double(s),
            length(s) == 1L);
  if((s >= (-.Machine$integer.max)) && (s <= .Machine$integer.max)) {
    v <- as.integer(s);
    if(is.finite(v) && (v == s)) {
      return(v);
    }
  }
  return(s);
}

.try.parse.double <- function(s) {
  s <- gsub("d", ".", s, fixed=TRUE);
  t <- suppressWarnings(as.double(s));
  if(is.na(t) || (!is.finite(t))) {
    return(s);
  }
  s <- t;
  stopifnot(is.finite(s),
            is.double(s),
            length(s) == 1L);
  if((s >= (-.Machine$integer.max)) && (s <= .Machine$integer.max)) {
    v <- as.integer(s);
    if(is.finite(v) && (v == s)) {
      return(v);
    }
  }
  return(s);
}

.parse.int <- function(s) {
  s <- as.integer(s);
  stopifnot(is.finite(s),
            is.integer(s),
            length(s) == 1L);
  return(s);
}


#' @title Parse the Parameters of a Random Sampling Algorithm
#' @description Check whether an algorithm directory is a random sampling
#'   algorithm and return the corresponding parameters if yes or \code{NULL} if
#'   not.
#' @param algoDir the algorithm directory
#' @return the parameters of the algorithm if it is random sampling or
#'   \code{NULL} if it is not
#' @export aitoa.algorithm.setup.rs
aitoa.algorithm.setup.rs <- function(algoDir) {
  if(algoDir == "rs") {
    return(.make.list(c(.algo.algorithm,
                        .algo.representation,
                        .algo.is.hybrid,
                        .algo.fitness,
                        .algo.mu,
                        .algo.lambda,
                        .algo.binary.rate),
                      .algo.algorithm.rs,
                      .algo.representation.default,
                      FALSE,
                      .algo.fitness.direct,
                      0L, 1L, 0));
  }
  return(NULL);
}


#' @title Parse the Parameters of a Hill Climbing Algorithm
#' @description Check whether an algorithm directory is a hill climbing
#'   algorithm and return the corresponding parameters if yes or \code{NULL} if
#'   not.
#' @param algoDir the algorithm directory
#' @return the parameters of the algorithm if it is hill climbing or \code{NULL}
#'   if it is not
#' @export aitoa.algorithm.setup.hc
aitoa.algorithm.setup.hc <- function(algoDir) {
  if(startsWith(algoDir, "hc_")) {
    s <- strsplit(algoDir, "_", fixed=TRUE);
    stopifnot(length(s) == 1L);
    s <- trimws(s[[1L]]);
    if(length(s) == 2L) {
      return(.make.list(c(.algo.algorithm,
                          .algo.operator.unary,
                          .algo.restarts,
                          .algo.representation,
                          .algo.is.hybrid,
                          .algo.fitness,
                          .algo.mu,
                          .algo.lambda,
                          .algo.binary.rate),
                        .algo.algorithm.hc,
                        s[[2L]],
                        FALSE,
                        .algo.representation.default,
                        FALSE,
                        .algo.fitness.direct,
                        1L, 1L, 0));
    }
    stopifnot(s[[2L]] == "rs");
    if(length(s) == 5L) {
      return(.make.list(c(.algo.algorithm,
                          .algo.operator.unary,
                          .algo.restarts,
                          .algo.restarts.strategy,
                          .algo.restarts.parameter,
                          .algo.representation,
                          .algo.is.hybrid,
                          .algo.fitness,
                          .algo.mu,
                          .algo.lambda,
                          .algo.binary.rate),
                        .algo.algorithm.hc,
                        s[[5L]],
                        TRUE,
                        .try.parse.double(s[[3L]]),
                        .parse.double(s[[4L]]),
                        .algo.representation.default,
                        FALSE,
                        .algo.fitness.direct,
                        1L, 1L, 0));
    }
    stopifnot(length(s) == 4L);
    return(.make.list(c(.algo.algorithm,
                        .algo.operator.unary,
                        .algo.restarts,
                        .algo.restarts.strategy,
                        .algo.representation,
                        .algo.is.hybrid,
                        .algo.fitness,
                        .algo.mu,
                        .algo.lambda,
                        .algo.binary.rate),
                      .algo.algorithm.hc,
                      s[[4L]],
                      TRUE,
                      .try.parse.double(s[[3L]]),
                      .algo.representation.default,
                      FALSE,
                      .algo.fitness.direct,
                      1L, 1L, 0));
  }
  return(NULL);
}


#' @title Parse the Parameters of a Simulated Annealing Algorithm
#' @description Check whether an algorithm directory is a Simulated Annealing
#'   algorithm and return the corresponding parameters if yes or \code{NULL} if
#'   not.
#' @param algoDir the algorithm directory
#' @return the parameters of the algorithm if it is Simulated Annealing or \code{NULL}
#'   if it is not
#' @export aitoa.algorithm.setup.sa
aitoa.algorithm.setup.sa <- function(algoDir) {
  if(startsWith(algoDir, "sa_")) {
    s <- strsplit(algoDir, "_", fixed=TRUE);
    stopifnot(length(s) == 1L);
    s <- trimws(s[[1L]]);
    stopifnot(length(s) == 5L);
    ts <- .parse.double(s[[3L]]);
    stopifnot(ts > 0);
    eps <- .parse.double(s[[4L]]);
    stopifnot(eps >= 0);
    return(.make.list(c(.algo.algorithm,
                        .algo.operator.unary,
                        .algo.restarts,
                        .algo.representation,
                        .algo.temperature.schedule,
                        .algo.temperature.start,
                        .algo.temperature.parameter,
                        .algo.is.hybrid,
                        .algo.fitness,
                        .algo.mu,
                        .algo.lambda,
                        .algo.binary.rate),
                      .algo.algorithm.sa,
                      s[[5L]],
                      FALSE,
                      .algo.representation.default,
                      s[[2L]],
                      ts,
                      eps,
                      FALSE,
                      .algo.fitness.direct,
                      1L, 1L, 0));
  }
  return(NULL);
}


#' @title Parse the Parameters of an Evolutionary Algorithm
#' @description Check whether an algorithm directory is an Evolutionary
#'   Algorithm and return the corresponding parameters if yes or \code{NULL} if
#'   not.
#' @param algoDir the algorithm directory
#' @return the parameters of the algorithm if it is an Evolutionary Algorithm or
#'   \code{NULL} if it is not
#' @export aitoa.algorithm.setup.ea
aitoa.algorithm.setup.ea <- function(algoDir) {
  is.ea <- startsWith(algoDir, "ea_");
  if(is.ea) {
    is.pruning <- FALSE;
  } else {
    is.ea <- startsWith(algoDir, "eap_");
    is.pruning <- is.ea;
  }
  if(!is.ea) {
    return(NULL);
  }

  s <- strsplit(algoDir, "_", fixed=TRUE);
  stopifnot(length(s) == 1L);
  s <- trimws(s[[1L]]);

  if(length(s) == 5L) {
    stopifnot(!is.pruning);
    ofs <- 3L;
    fitness <- s[[2L]];
  } else {
    stopifnot(length(s) == 4L);
    ofs <- 2L;
    if(is.pruning) {
      fitness <- .algo.fitness.pruning;
    } else {
      fitness <- .algo.fitness.direct;
    }
  }

  mu.lambda.cr <- strsplit(s[[ofs]], "@", fixed=TRUE);
  stopifnot(length(mu.lambda.cr) == 1L);
  mu.lambda.cr <- trimws(mu.lambda.cr[[1L]]);
  stopifnot(length(mu.lambda.cr) == 2L);
  mu.lambda <- strsplit(mu.lambda.cr[1L], "+", fixed=TRUE);
  stopifnot(length(mu.lambda) == 1L);
  mu.lambda <- mu.lambda[[1L]];
  stopifnot(length(mu.lambda) == 2L);

  mu <- .parse.int(mu.lambda[[1L]]);
  stopifnot(mu > 0L);
  lambda <- .parse.int(mu.lambda[[2L]]);
  stopifnot(lambda > 0L);
  cr <- .parse.double(mu.lambda.cr[[2L]]);
  stopifnot(cr >= 0, cr <= 1);

  return(.make.list(c(.algo.algorithm,
                      .algo.operator.unary,
                      .algo.operator.binary,
                      .algo.restarts,
                      .algo.representation,
                      .algo.mu,
                      .algo.lambda,
                      .algo.binary.rate,
                      .algo.fitness,
                      .algo.is.hybrid),
                    .algo.algorithm.ea,
                    s[[ofs + 1L]],
                    s[[ofs + 2L]],
                    FALSE,
                    .algo.representation.default,
                    mu,
                    lambda,
                    cr,
                    fitness,
                    FALSE));
}



#' @title Parse the Parameters of a Memetic Algorithm
#' @description Check whether an algorithm directory is a Memetic Algorithm and
#'   return the corresponding parameters if yes or \code{NULL} if not.
#' @param algoDir the algorithm directory
#' @return the parameters of the algorithm if it is a Memetic Algorithm or
#'   \code{NULL} if it is not
#' @export aitoa.algorithm.setup.ma
aitoa.algorithm.setup.ma <- function(algoDir) {
  is.ma <- startsWith(algoDir, "ma_");
  if(is.ma) {
    is.pruning <- FALSE;
  } else {
    is.ma <- startsWith(algoDir, "map_");
    is.pruning <- is.ma;
  }
  if(!is.ma) {
    return(NULL);
  }

  s <- strsplit(algoDir, "_", fixed=TRUE);
  stopifnot(length(s) == 1L);
  s <- trimws(s[[1L]]);

  if(length(s) == 5L) {
    stopifnot(!is.pruning);
    ofs <- 3L;
    fitness <- s[[2L]];
  } else {
    stopifnot(length(s) == 4L);
    ofs <- 2L;
    if(is.pruning) {
      fitness <- .algo.fitness.pruning;
    } else {
      fitness <- .algo.fitness.direct;
    }
  }

  mu.lambda <- strsplit(s[[ofs]], "+", fixed=TRUE);
  stopifnot(length(mu.lambda) == 1L);
  mu.lambda <- mu.lambda[[1L]];
  stopifnot(length(mu.lambda) == 2L);

  mu <- .parse.int(mu.lambda[[1L]]);
  stopifnot(mu > 0L);
  lambda <- .parse.int(mu.lambda[[2L]]);
  stopifnot(lambda > 0L);

  return(.make.list(c(.algo.algorithm,
                      .algo.operator.unary,
                      .algo.operator.binary,
                      .algo.restarts,
                      .algo.representation,
                      .algo.mu,
                      .algo.lambda,
                      .algo.binary.rate,
                      .algo.fitness,
                      .algo.is.hybrid,
                      .algo.hybrid.global.search),
                    .algo.algorithm.ma,
                    s[[ofs + 1L]],
                    s[[ofs + 2L]],
                    FALSE,
                    .algo.representation.default,
                    mu,
                    lambda,
                    1L,
                    fitness,
                    TRUE,
                    .algo.algorithm.ea));
}


#' @title Parse the Parameters of an Estimation of Distribution Algorithm
#' @description Check whether an algorithm directory is an Estimation of
#'   Distribution Algorithm and return the corresponding parameters if yes or
#'   \code{NULL} if not.
#' @param algoDir the algorithm directory
#' @return the parameters of the algorithm if it is an Estimation of
#'   Distribution Algorithm or \code{NULL} if it is not
#' @export aitoa.algorithm.setup.eda
aitoa.algorithm.setup.eda <- function(algoDir) {
  if(!startsWith(algoDir, "eda_")) {
    return(NULL);
  }

  s <- strsplit(algoDir, "_", fixed=TRUE);
  stopifnot(length(s) == 1L);
  s <- trimws(s[[1L]]);
  if(length(s) == 4L) {
    ofs <- 4L;
    fitness <- s[3L];
  } else {
    stopifnot(length(s) == 3L);
    ofs <- 3L;
    fitness <- "direct";
  }

  mu.lambda <- strsplit(s[[ofs]], "+", fixed=TRUE);
  stopifnot(length(mu.lambda) == 1L);
  mu.lambda <- mu.lambda[[1L]];
  stopifnot(length(mu.lambda) == 2L);

  mu <- .parse.int(mu.lambda[[1L]]);
  stopifnot(mu > 0L);
  lambda <- .parse.int(mu.lambda[[2L]]);
  stopifnot(lambda > 0L);

  return(.make.list(c(.algo.algorithm,
                      .algo.restarts,
                      .algo.representation,
                      .algo.model,
                      .algo.mu,
                      .algo.lambda,
                      .algo.fitness,
                      .algo.is.hybrid),
                    .algo.algorithm.eda,
                    FALSE,
                    .algo.representation.default,
                    s[[2L]],
                    mu,
                    lambda,
                    fitness,
                    FALSE));
}



#' @title Parse the Parameters of an hybrid Estimation of Distribution Algorithm
#' @description Check whether an algorithm directory is an hybrid Estimation of
#'   Distribution Algorithm and return the corresponding parameters if yes or
#'   \code{NULL} if not.
#' @param algoDir the algorithm directory
#' @return the parameters of the algorithm if it is an hybrid Estimation of
#'   Distribution Algorithm or \code{NULL} if it is not
#' @export aitoa.algorithm.setup.heda
aitoa.algorithm.setup.heda <- function(algoDir) {
  if(!startsWith(algoDir, "heda_")) {
    return(NULL);
  }

  s <- strsplit(algoDir, "_", fixed=TRUE);
  stopifnot(length(s) == 1L);
  s <- trimws(s[[1L]]);
  if(length(s) == 5L) {
    ofs <- 4L;
    fitness <- s[3L];
  } else {
    stopifnot(length(s) == 4L);
    ofs <- 3L;
    fitness <- "direct";
  }

  mu.lambda <- strsplit(s[[ofs]], "+", fixed=TRUE);
  stopifnot(length(mu.lambda) == 1L);
  mu.lambda <- mu.lambda[[1L]];
  stopifnot(length(mu.lambda) == 2L);

  mu <- .parse.int(mu.lambda[[1L]]);
  stopifnot(mu > 0L);
  lambda <- .parse.int(mu.lambda[[2L]]);
  stopifnot(lambda > 0L);

  return(.make.list(c(.algo.algorithm,
                      .algo.operator.unary,
                      .algo.restarts,
                      .algo.representation,
                      .algo.model,
                      .algo.mu,
                      .algo.lambda,
                      .algo.fitness,
                      .algo.is.hybrid,
                      .algo.hybrid.global.search),
                    .algo.algorithm.eda,
                    s[[ofs+1L]],
                    FALSE,
                    .algo.representation.default,
                    s[[2L]],
                    mu,
                    lambda,
                    fitness,
                    TRUE,
                    .algo.algorithm.eda));
}



#' @title Parse the Parameters of a Hill Climbing Algorithm 2
#' @description Check whether an algorithm directory is a hill climbing
#'   algorithm 2 and return the corresponding parameters if yes or \code{NULL} if
#'   not.
#' @param algoDir the algorithm directory
#' @return the parameters of the algorithm if it is hill climbing 2 or \code{NULL}
#'   if it is not
#' @export aitoa.algorithm.setup.hc2
aitoa.algorithm.setup.hc2 <- function(algoDir) {
  if(startsWith(algoDir, "hc2f_")) {
    s <- strsplit(algoDir, "_", fixed=TRUE);
    stopifnot(length(s) == 1L);
    s <- trimws(s[[1L]]);
    if(length(s) == 2L) {
      return(.make.list(c(.algo.algorithm,
                          .algo.operator.unary,
                          .algo.restarts,
                          .algo.representation,
                          .algo.is.hybrid,
                          .algo.fitness,
                          .algo.mu,
                          .algo.lambda,
                          .algo.binary.rate),
                        .algo.algorithm.hc2,
                        s[[2L]],
                        FALSE,
                        .algo.representation.default,
                        FALSE,
                        .algo.fitness.direct,
                        1L,
                        1L,
                        0));
    }
    stopifnot(length(s) == 3L,
              s[[2L]] == "rs");
    return(.make.list(c(.algo.algorithm,
                        .algo.operator.unary,
                        .algo.restarts,
                        .algo.restarts.strategy,
                        .algo.representation,
                        .algo.is.hybrid,
                        .algo.fitness,
                        .algo.mu,
                        .algo.lambda,
                        .algo.binary.rate),
                      .algo.algorithm.hc2,
                      s[[3L]],
                      TRUE,
                      .algo.restarts.strategyHC2,
                      .algo.representation.default,
                      FALSE,
                      .algo.fitness.direct,
                      1L, 1L, 0));
  }
  return(NULL);
}


.algo.setup.functions.inner <- c(aitoa.algorithm.setup.rs,
                                 aitoa.algorithm.setup.hc,
                                 aitoa.algorithm.setup.sa,
                                 aitoa.algorithm.setup.ea,
                                 aitoa.algorithm.setup.ma,
                                 aitoa.algorithm.setup.eda,
                                 aitoa.algorithm.setup.heda,
                                 aitoa.algorithm.setup.hc2);
.algo.setup.functions.inner.add.par <- c(0L,#aitoa.algorithm.setup.rs,
                                         1L,#aitoa.algorithm.setup.hc,
                                         1L,#,aitoa.algorithm.setup.sa,
                                         2L,#aitoa.algorithm.setup.ea,
                                         2L,#aitoa.algorithm.setup.ma,
                                         0L,#aitoa.algorithm.setup.eda,
                                         1L,#aitoa.algorithm.setup.heda,
                                         2L#aitoa.algorithm.setup.hc2
                                         );



#' @title Parse the Parameters of an Algorithm utilizing the Tree Representation
#' @description Check whether an algorithm directory is a tree-representation
#'   based algorithm and return the corresponding parameters if yes or
#'   \code{NULL} if not.
#' @param algoDir the algorithm directory
#' @return the parameters of the algorithm if it is a tree representation-based
#'   algorithm or \code{NULL} if it is not
#' @export aitoa.algorithm.setup.gp
aitoa.algorithm.setup.gp <- function(algoDir) {
  if(!(grepl("^gp[1-9][0-9]*\\_", algoDir, fixed=FALSE))) {
    return(NULL);
  }
  s <- strsplit(algoDir, "_", fixed=TRUE);
  stopifnot(length(s) == 1L);
  s <- s[[1L]];
  depth <- .parse.int(substr(s[[1L]], 3L, nchar(s[[1L]])));
  stopifnot(depth > 1L);
  rest <- paste(s[-1L], sep="_", collapse="_");
  result <- NULL;
  for(i in seq_along(.algo.setup.functions.inner)) {
    x <- .algo.setup.functions.inner.add.par[[i]];
    if(x <= 0L) {
      stopifnot(x == 0L);
      p <- rest;
    } else {
      if(x <= 1L) {
        p <- paste(rest, .algo.operator.unary.tree, sep="_", collapse="_");
      } else {
        stopifnot(x == 2L);
        p <- paste(rest, .algo.operator.unary.tree,
                         .algo.operator.binary.tree,
                   sep="_", collapse="_");
      }
    }
    result <- .algo.setup.functions.inner[[i]](p);
    result <- force(result);
    if(!(is.null(result))) {
      break;
    }
  }

  stopifnot(!is.null(result));
  result[[.algo.representation]] <- .algo.representation.tree;
  result[[.algo.max.tree.depth]] <- depth;
  return(result);
}


.algo.setup.functions <- unlist(c(.algo.setup.functions.inner,
                                   aitoa.algorithm.setup.gp));

#' @title Get the Default Algorithm Setup Parsers
#' @description  Get the list of default algorithm setup parameter parsers
#' @return the list of default algorithm setup parameter parsers
#' @export aitoa.algorithm.setup.parsers
aitoa.algorithm.setup.parsers <- function() .algo.setup.functions

#' @title Parse the Parameters of an Algorithm
#' @description Check whether an algorithm directory fits to any known algorithm
#'   and return the corresponding parameters.
#' @param algoDir the algorithm directory
#' @param parsers the list of parsers to apply
#' @return the parameters of the algorithm
#' @export aitoa.algorithm.setup
aitoa.algorithm.setup <- function(algoDir, parsers=aitoa.algorithm.setup.parsers()) {
  for(f in parsers) {
    a <- f(algoDir);
    if(!is.null(a)) {
      return(a);
    }
  }
  stop(paste0("setup ", algoDir, " cannot be parsed."));
}
