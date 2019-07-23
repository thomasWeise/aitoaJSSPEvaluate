# Create the Data of this Package

old.options <- options(warn=2);

library(usethis);
library(utils);

# setup directories
dir.data.raw <- dirname(sys.frame(1)$ofile);
dir.data.raw <- normalizePath(dir.data.raw, mustWork = TRUE);
stopifnot(dir.exists(dir.data.raw));

dir.data <- normalizePath(file.path(dir.data.raw, "..", "data"), mustWork = TRUE);
stopifnot(dir.exists(dir.data));

# do the JSSP dataset
source(file.path(dir.data.raw, "make_instances.R"));

# do the related work
source(file.path(dir.data.raw, "make_related_work.R"));

# store the data
usethis::use_data(jsspInstances,
                  jsspRelatedWorkResults,
                  compress="xz", version=3L, overwrite = TRUE)

options(old.options);
