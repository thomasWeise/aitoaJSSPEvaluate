# Create the Data of this Package

old.options <- options(warn=2);

logger <- function(...) {
  cat(as.character(Sys.time()), ": ", paste(..., sep="", collapse=""), "\n", sep="", collapse="");
  invisible(TRUE);
}

logger("loading required libraries.");
library(usethis);
library(utils);

# setup directories
logger("setting up directories.");
dir.data.raw <- dirname(sys.frame(1)$ofile);
dir.data.raw <- normalizePath(dir.data.raw, mustWork = TRUE);
stopifnot(dir.exists(dir.data.raw));

# do the JSSP dataset
logger("making JSSP instances dataset.");
source(file.path(dir.data.raw, "make_instances.R"));

# do the related work
logger("making JSSP related work data set");
source(file.path(dir.data.raw, "make_related_work.R"));

# double check the data
logger("finished creating datasets, now storing them.");
stopifnot(exists("jsspInstances"),
          is.data.frame(jsspInstances),
          exists("jsspRelatedWorkResults"),
          is.data.frame(jsspRelatedWorkResults));

# store the data
usethis::use_data(jsspInstances,
                  jsspRelatedWorkResults,
                  compress="xz", version=3L, overwrite = TRUE);

rm("dir.data.raw");
rm("jsspInstances");
rm("jsspRelatedWorkResults");

options(old.options);
rm("old.options");

logger("all done.");
rm("logger");
