# Do the JSSP Dataset
file.source <- normalizePath(file.path(dir.data.raw, "jsspInstances.txt"), mustWork = TRUE);
stopifnot(file.exists(file.source));

jsspInstances <- read.csv(file.source);
rm("file.source");

stopifnot(is.data.frame(jsspInstances),
          nrow(jsspInstances) == 242L,
          ncol(jsspInstances) == 6L,
          length(unique(jsspInstances$inst.name)) == 242L,
          is.integer(jsspInstances$inst.jobs),
          all(jsspInstances$inst.jobs > 0L),
          is.integer(jsspInstances$inst.machines),
          all(jsspInstances$inst.machines > 0L),
          is.integer(jsspInstances$inst.opt.bound.lower),
          all(jsspInstances$inst.opt.bound.lower > 0L),
          is.integer(jsspInstances$inst.opt.bound.upper),
          all(jsspInstances$inst.opt.bound.upper >= jsspInstances$inst.opt.bound.lower),
          is.integer(jsspInstances$inst.solutions.num),
          all(jsspInstances$inst.solutions.num >= 0L));

opt <- jsspInstances$inst.solutions.num > 0L;
stopifnot(all(jsspInstances$inst.opt.bound.upper[opt] == jsspInstances$inst.opt.bound.lower[opt]));
rm("opt");

