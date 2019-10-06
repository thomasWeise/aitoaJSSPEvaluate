
#' @title Obtain a Data Frame with the Parameters of All Present Algorithms
#' @description Obtain a data frame which will contain the parameters of all algorithms
#' @param config the configuration
#' @param parsers the algorithm parameter parsers
#' @return a data frame with the algorithm parameters
#' @include utils.R
#' @include config.R
#' @include algorithm_parameters.R
#' @export aitoa.algorithm.parameters.frame
#' @importFrom utils read.csv write.csv
#' @include setups.R
aitoa.algorithm.parameters.frame <- function(config=aitoa.config(),
                                             parsers=aitoa.algorithm.parameters.parsers()) {
  old.options <- options(warn=2);
  file <- file.path(.dir.eval(config=config), "algorithmParameters.txt");

  if(!file.exists(file)) {
    config$logger("algorithm parameters file '", file, "' does not yet exist, so we need to create it.");

# get the algorithm ids
    algorithms <- as.character(unique(aitoa.setups.frame(config)$algo.id));
    n <- length(algorithms);
    stopifnot(n > 0L);

    config$logger("setups loaded, now computing algorithm parameters for ",
                  n, " different algorithms.");
    parameters <- lapply(algorithms, aitoa.algorithm.parameters, parsers=parsers);

    stopifnot(length(parameters) == length(algorithms),
              all(vapply(parameters, length, 0L) > 0L));

    param.names <- lapply(parameters, names);
    stopifnot(length(param.names) >= 1L);
    param.names <- unique(unname(unlist(param.names)));
    stopifnot(length(param.names) >= 2L,
              !(any(param.names == "algo.id")));
    param.names <- param.names[order(vapply(param.names,
                        function(nn) which(nn==.algo.param.seq)[[1L]], 0L))];
    param.names <- c("algo.id", param.names);

    config$logger("found the following parameters: ",
                  paste(param.names), "; now building frame");

    parameters <- lapply(seq_along(parameters),
                         function(pi) {
                           pp <- parameters[[pi]];
                           pp <- pp[param.names];
                           names(pp) <- param.names;
                           pp[[1L]] <- algorithms[[pi]];
                           pp[vapply(pp, is.null, FALSE)] <- NA;
                           pp <- force(pp);
                           return(pp);
                         });

    parameters <- matrix(unlist(parameters), nrow=n, byrow=TRUE);
    colnames(parameters) <- param.names;
    rownames(parameters) <- NULL;
    parameters <- data.frame(parameters,
                             check.names = FALSE);

    stopifnot(ncol(parameters) > 0L,
              nrow(parameters) > 0L,
              nrow(parameters) == n,
              length(parameters$algo.id) == n);

    srt <- c("algo.algorithm", "algo.name", param.names);
    srt <- unique(srt);
    l <- unname(lapply(srt, function(ss) parameters[, ss]));
    l <- do.call(order, l);

    parameters <- parameters[l,];

    stopifnot(ncol(parameters) > 0L,
              nrow(parameters) > 0L,
              nrow(parameters) == n,
              length(parameters$algo.id) == n);

    param.names <- parameters$algo.name;
    param.names <- param.names[!is.na(param.names)];
    stopifnot(length(param.names) == length(unique(param.names)));

    config$logger("done creating frame, now writing results to file '", file, "'.");

    write.csv(parameters,
              file=file,
              row.names=FALSE,
              quote=FALSE);
    rm("parameters");
    rm("param.names");
    rm("algorithms");
    stopifnot(file.exists(file),
              file.size(file) > 10L*n);
    config$logger("file '", file, "' created successfully.");
  }

  stopifnot(file.exists(file),
            file.size(file) > 10L);
  config$logger("now loading algorithm parameters frame from file '", file, "'.");
  result <- read.csv(file, check.names = FALSE);
  result <- force(result);
  stopifnot(is.data.frame(result),
            nrow(result) > 0L,
            colnames(result)[1L:3L] == c("algo.id",
                                         "algo.name",
                                         "algo.algorithm"),
            all(is.factor(result$algo.id)),
            all(is.factor(result$algo.name)),
            all(is.factor(result$algo.algorithm)));
  config$logger("done loading algorithm setups from file '", file, "'.");
  options(old.options);
  return(result);
}
