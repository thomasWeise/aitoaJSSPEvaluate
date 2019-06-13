
#' @title Obtain a Data Frame with the Features of All Present Instances
#' @description Obtain a data frame which will contain the features of all
#'   instances used in the experiment.
#' @param config the configuration
#' @return a data frame with the instance features, formatted exactly as
#'   \link{jsspInstances}
#' @include utils.R
#' @include config.R
#' @export aitoa.instance.features.frame
#' @importFrom utils read.csv write.csv
#' @include setups.R
aitoa.instance.features.frame <- function(config=aitoa.config()) {
  old.options <- options(warn=2);
  file <- file.path(.dir.eval(config=config), "instanceFeatures.txt");

  if(!file.exists(file)) {
    config$logger("instance features file '", file, "' does not yet exist, so we need to create it.");

    instances <- as.character(unique(aitoa.setups.frame(config)$instance));
    n <- length(instances);
    stopifnot(n > 0L);

    features <- config$instance.features();
    stopifnot(is.data.frame(features),
              nrow(features) >= n,
              all(is.integer(features$inst.jobs)),
              all(features$inst.jobs > 0L),
              all(is.integer(features$inst.machines)),
              all(features$inst.machines > 0L),
              all(is.integer(features$inst.opt.bound.lower)),
              all(features$inst.opt.bound.lower > 0L),
              all(is.integer(features$inst.opt.bound.upper)),
              all(features$inst.opt.bound.upper > 0L),
              all(is.integer(features$inst.solutions.num)),
              all(features$inst.solutions.num >= 0L),
              all(features$inst.opt.bound.upper >= features$inst.opt.bound.lower));

    names <- as.character(unname(unlist(features$inst.name)));
    stopifnot(all(vapply(instances, function(i) sum(names == i) == 1L, TRUE)));

    features <- features[vapply(instances, function(i) which(names == i)[[1L]], 1L), ];
    stopifnot(colnames(features) == c("inst.name",
                                      "inst.jobs",
                                      "inst.machines",
                                      "inst.opt.bound.lower",
                                      "inst.opt.bound.upper",
                                      "inst.solutions.num"),
              nrow(features) == n,
              all(is.integer(features$inst.jobs)),
              all(features$inst.jobs > 0L),
              all(is.integer(features$inst.machines)),
              all(features$inst.machines > 0L),
              all(is.integer(features$inst.opt.bound.lower)),
              all(features$inst.opt.bound.lower > 0L),
              all(is.integer(features$inst.opt.bound.upper)),
              all(features$inst.opt.bound.upper > 0L),
              all(is.integer(features$inst.solutions.num)),
              all(features$inst.solutions.num >= 0L),
              all(features$inst.opt.bound.upper >= features$inst.opt.bound.lower));

    config$logger("done selecting instances, now writing results to file '", file, "'.");

    write.csv(features,
              file=file,
              row.names=FALSE,
              quote=FALSE);
    rm("features");
    rm("names");
    stopifnot(file.exists(file),
              file.size(file) > 10L*n);
    config$logger("file '", file, "' created successfully.");
  }

  stopifnot(file.exists(file),
            file.size(file) > 10L);
  config$logger("now loading instance features from file '", file, "'.");
  result <- read.csv(file, check.names = FALSE);
  result <- force(result);
  stopifnot(is.data.frame(result),
            nrow(result) > 0L,
            colnames(result) == c("inst.name",
                                  "inst.jobs",
                                  "inst.machines",
                                  "inst.opt.bound.lower",
                                  "inst.opt.bound.upper",
                                  "inst.solutions.num"),
            all(is.integer(result$inst.jobs)),
            all(result$inst.jobs > 0L),
            all(is.integer(result$inst.machines)),
            all(result$inst.machines > 0L),
            all(is.integer(result$inst.opt.bound.lower)),
            all(result$inst.opt.bound.lower > 0L),
            all(is.integer(result$inst.opt.bound.upper)),
            all(result$inst.opt.bound.upper > 0L),
            all(is.integer(result$inst.solutions.num)),
            all(result$inst.solutions.num >= 0L),
            all(result$inst.opt.bound.lower <= result$inst.opt.bound.upper));
  config$logger("done loading instance features from file '", file, "'.");
  options(old.options);
  return(result);
}
