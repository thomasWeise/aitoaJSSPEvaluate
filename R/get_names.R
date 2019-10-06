#' @title Get the Names of the Given Algorithm Setups
#' @description Take the mnemonic and translate them to nice names
#' @param setups the list/vector of algorithm setup names
#' @param config the configuration
#' @return the vector of algorithm names
#' @include algorithm_parameters_frame.R
#' @export aitoa.get.algorithm.setup.names
aitoa.get.algorithm.setup.names <- function(setups, config=aitoa.config()) {
  setups <- unname(unlist(setups));
  stopifnot(is.factor(setups) || is.character(setups),
            !any(is.na(setups)));
  setups <- as.character(setups);
  stopifnot(!any(is.na(setups)),
            all(nchar(setups) > 0L));
  
  stopifnot(length(setups) > 0L);
  
  frame <- aitoa.algorithm.parameters.frame(config);
  stopifnot(is.data.frame(frame),
            nrow(frame) > 0L);

  algo.ids <- as.character(unname(unlist(frame$algo.id)));
  stopifnot(length(algo.ids) >= length(setups),
            length(unique(algo.ids)) == length(algo.ids));
  
  indexes <- vapply(setups, function(s) which(algo.ids == s), NA_integer_);
  stopifnot(is.integer(indexes),
            all(is.finite(indexes)),
            all(indexes > 0L),
            all(indexes <= length(algo.ids)));
  
  names <- as.character(unname(unlist(frame$algo.name)));
  stopifnot(length(names) == length(algo.ids));
  
  names <- names[indexes];
  stopifnot(is.character(names),
            all(nchar(names) > 0L));
  return(names);
}
