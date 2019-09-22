# get the setup names
#' @include algorithm_parameters_frame.R
.get.setup.names <- function(setups, config=aitoa.config()) {
  stopifnot(is.factor(setups));
  setups <- unname(unlist(setups));
  l <- length(setups);
  stopifnot(l > 0L);
  setups <- unique(setups);
  stopifnot(length(setups) >= l);

  names <- aitoa.algorithm.parameters.frame(config);
  stopifnot(is.data.frame(names),
            nrow(names) > 0L);

  name.1 <- unname(unlist(names$algo.id));
  stopifnot(length(name.1) == length(setups),
            length(unique(name.1)) == length(name.1));
  name.1.in.setups <- vapply(name.1, function(nn) which(setups==nn), 1L);
  stopifnot(all(name.1.in.setups > 0L),
            all(name.1.in.setups <= length(setups)));
  rm("name.1.in.setups");
  setups.in.name.1 <- vapply(setups, function(nn) which(name.1==nn), 1L);
  stopifnot(all(setups.in.name.1 > 0L),
            all(setups.in.name.1 <= length(name.1)));
  rm("name.1");
  names <- unname(unlist(names$algo.name[setups.in.name.1]));
  rm("setups.in.name.1");
  stopifnot(length(names) == length(setups));
  return(names);
}
