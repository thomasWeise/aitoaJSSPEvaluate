#' @title Obtain a Ranking of the End Results
#' @description Get a ranking of the end results by algorithm name/setup. For
#'   each instance, a separate ranking is created. The ranking for all instances
#'   per algorithm is summed up under instance id \code{[all]}.
#' @param config the configuration object
#' @param algo.selector an optional selector function
#' @return a data frame with the algorithm names and their
#' @export aitoa.rank.end.results
aitoa.rank.end.results <- function(config, algo.selector=function(param.row) TRUE) {
# load the results frame
  results <- aitoa.end.results.frame(config);
  stopifnot(is.data.frame(results),
            nrow(results) > 0L);

# load the algorithm names
  names   <- aitoa.algorithm.parameters.frame(config);
  stopifnot(is.data.frame(names),
            nrow(names) > 0L);
  for(i in seq_len(ncol(names))) {
    if(is.factor(names[, i])) {
      names[, i] <- as.character(unname(unlist(names[, i])));
    }
  }
  stopifnot(is.data.frame(names),
            nrow(names) > 0L);

# make the selection among the names
  selected <- vapply(seq_len(nrow(names)),
                     function(i) algo.selector(names[i, ]),
                     FALSE);
  stopifnot(all(vapply(selected, isTRUE, FALSE) |
                vapply(selected, isFALSE, FALSE)),
            sum(selected) > 0L);
  names <- names[selected, ];
  stopifnot(is.data.frame(names),
            nrow(names) > 0L);

# finalize names column types
  names <- names[order(as.character(unname(unlist(names$algo.id))),
                       as.character(unname(unlist(names$algo.name)))), ];
  stopifnot(is.data.frame(names),
            nrow(names) > 0L);
  for(i in seq_len(ncol(names))) {
    if(is.factor(names[, i])) {
      names[, i] <- as.character(unname(unlist(names[, i])));
    }
  }

  results.algo.ids <- as.character(unname(unlist(results$algo.id)));
  names.algo.ids   <- as.character(unname(unlist(names$algo.id)));
  stopifnot(identical(names.algo.ids, unique(names.algo.ids)));

  selected <- vapply(results.algo.ids, function(n) n %in% names.algo.ids, FALSE);
  stopifnot(all(vapply(selected, isTRUE, FALSE) |
                vapply(selected, isFALSE, FALSE)),
            sum(selected) > 0L);
  results <- results[selected, ];
  stopifnot(is.data.frame(results),
            nrow(results) > 0L);

  results.algo.ids <- as.character(unname(unlist(results$algo.id)));
  results.inst.id <- as.character(unname(unlist(results$inst.id)));
  results.best.f <- unname(unlist(results$best.f));
  rm("results");

# get the instances
  insts <- sort(unique(results.inst.id));

# get the unique algorithm ids
  frames <- list();

  total.ranks <- unname(unlist(numeric(length(names.algo.ids))));
  for(inst in insts) {
    frame <- names;

    sel <- (results.inst.id == inst);
    stopifnot(sum(sel) > 0L);

    algo.ids <- results.algo.ids[sel];
    best.fs <- results.best.f[sel];
    rm("sel");

    ranks <- rank(unname(unlist(best.fs)));
    stopifnot(all(is.finite(ranks)),
              all(ranks > 0L));

    rank.sum <- vapply(names.algo.ids,
                       function(id) {
      sel <- (algo.ids  == id);
      stopifnot(any(sel));
      count <- sum(sel);
      stopifnot(count > 0L);
      sum(ranks[sel]) / count
    }, NA_real_);

    total.ranks <- total.ranks + rank.sum;

    frames[[inst]] <- data.frame(inst.id=unname(unlist(rep(inst, length(names.algo.ids)))),
                                 frame,
                                 rank.sum=rank.sum);
  }

  frames[["[all]"]] <- data.frame(inst.id=unname(unlist(rep("[all]", length(names.algo.ids)))),
                                  frame,
                                  rank.sum=total.ranks);
  frames <- frames[c(length(frames), seq.int(from=1L, to=(length(frames)-1L)))];

  o <- order(total.ranks);
  frames <- lapply(frames, function(ff) ff[o, ]);

  result <- do.call(rbind, frames);
  result <- force(result);

  rownames(result) <- NULL;
  result <- force(result);

  return(result);
}
