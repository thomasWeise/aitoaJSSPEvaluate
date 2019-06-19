#' @title Print an End Result Statistics Table
#' @description  Print the end result statistics table in markdown format.
#' @param setups the list of setups which should be analyzed
#' @param name the (optional) file name
#' @return the path to the file
#' @export aitoa.end.results.statistics.table
#' @include utils.R
#' @include end_results_stats.R
#' @include instance_features.R
#' @include algorithm_parameters_frame.R
aitoa.end.results.statistics.table <- function(setups, name=NULL) {
  stopifnot(length(setups) > 0L);
  setups <- unique(as.character(setups));
  stopifnot(length(setups) > 0L,
            all(is.character(setups)),
            all(nchar(setups) > 0));

  names <- aitoa.algorithm.parameters.frame(config);
  stopifnot(is.data.frame(names), nrow(names) > 0L);
  names.name <- as.character(unname(unlist(names$algo.name)));
  stopifnot(length(names.name) == nrow(names));
  pick <- !is.na(names.name);
  stopifnot(sum(pick) > 0L);
  names.name <- names.name[pick];
  stopifnot(length(names.name) > 0L,
            all(nchar(names.name) > 0L));
  names.setup <- as.character(unname(unlist(names$algo.setup)))[pick];
  stopifnot(length(names.setup) == length(names.name),
            all(nchar(names.setup) > 0L));
  rm("names");
  rm("pick");

  found <- vapply(setups, function(setup) {
    sel <- names.setup == setup;
    stopifnot(sum(sel) == 1L);
    sel <- which(sel);
    stopifnot(length(sel) == 1L);
    return(sel[[1L]]);
  }, 1L);
  names.name <- names.name[found];
  names.setup <- names.setup[found];
  rm("found");

  stopifnot(all(names.name == setups));

  if(is.null(name)) {
    name <- paste0(paste(setups, sep="_", collapse="_"), "_endResultStatistics.md");
  } else {
    if(!endsWith(tolower(name), ".md")) {
      name <- paste0(name, ".md");
    }
  }

  path <- file.path(.dir.tables("endResultStatistics", config=config), name);
  if(!file.exists(path)) {
    config$logger("File '", path, "' does not exist, so we need to create it.");

    frame <- aitoa.end.results.statistics.frame(config);
    stopifnot(is.data.frame(frame), nrow(frame) > 0L, ncol(frame) > 0L);

    features <- aitoa.instance.features.frame(config);
    stopifnot(is.data.frame(features), nrow(features) > 0L, ncol(features) > 0L);

    frame <- frame[as.character(frame$algorithm) %in% names.setup, ];

    frame$zz <- integer(nrow(frame));
    for(i in seq_len(nrow(frame))) {
      frame$zz[i] <- which(as.character(frame$algorithm[[i]])==setups)[[1L]];
    }
    stopifnot(nrow(frame) >= 1L);

    frame <- frame[order(frame$instance, frame$zz), ];
    stopifnot(nrow(frame) >= 1L);

    problem.ids <- unique(frame$instance);
    problem.ids.len <- length(problem.ids);
    stopifnot(problem.ids.len >= config$min.instances);
    printInst <- (problem.ids.len > 1L);
    config$logger("should we print ", problem.ids.len, " instances: ", printInst);

    printAlgo <- length(unique(frame$algorithm));
    stopifnot(printAlgo == length(names.name));
    printAlgo <- (printAlgo > 1L);
    config$logger("should we print algorithm setups: ", printAlgo);

    text <- rep("", nrow(frame) + 2L); # 1L + problem.ids.len);

    compare <- (max(vapply(problem.ids, function(p) sum(frame$instance == p), 0L)) > 1L);

    if(printInst) {
      text[[1L]] <- "|$\\instance$";
      text[[2L]] <- "|:-:";
      text[[1L]] <- paste0(text[[1L]], "|$\\lowerBound{\\objf}$");
      text[[2L]] <- paste0(text[[2L]], "|--:");
    }

    if(printAlgo) {
      text[[1L]] <- paste0(text[[1L]], "|setup|best|mean|med|sd|med(t)|med(FEs)|");
      text[[2L]] <- paste0(text[[2L]], "|:--|--:|--:|--:|--:|--:|--:|");
    } else {
      text[[1L]] <- paste0(text[1L], "|best|mean|med|sd|med(t)|med(FEs)|");
      text[[2L]] <- paste0(text[2L], "|--:|--:|--:|--:|--:|--:");
    }
    j <- 2L;

    if(compare) {
      config$logger("found problems occuring multiple times");

      for(instance in problem.ids) {

        sframe <- frame[frame$instance == instance,];
        config$logger("now handling setups for id ", instance, ": ", paste(sframe$algorithm, sep=",", collapse=","));

        f.min <- unname(unlist(sframe$best.f.min));
        f.min.min <- min(f.min);
        f.mean <- unname(unlist(sframe$best.f.mean));
        f.mean.min <- min(f.mean);
        f.med <- unname(unlist(sframe$best.f.med));
        f.med.min <- min(f.med);
        t.med <- unname(unlist(sframe$last.improvement.time.med));
        t.med.min <- min(t.med);
        f.sd <- unname(unlist(sframe$best.f.sd));
        f.sd.min <- min(f.sd);
        fes.med <- unname(unlist(sframe$last.improvement.fes.med));
        fes.med.min <- min(fes.med);

        for(i in seq_len(nrow(sframe))) {
          j <- (j + 1L);

          row <- sframe[i,];
          if(printInst) {
            if(i <= 1L) {
              ii <- as.character(row$instance);
              lb <- features$inst.name == ii;
              stopifnot(sum(lb) == 1L);
              lb <- which(lb);
              stopifnot(length(lb) == 1L);
              lb <- features$inst.opt.bound.lower[[lb]];
              stopifnot(is.integer(lb), lb > 0L);
              text[[j]] <- paste0("|`", ii, "`|", lb);
            } else {
              text[[j]] <- "||";
            }
          }
          if(printAlgo) {
            a <- as.character(row$algorithm) == names.setup;
            stopifnot(sum(a) == 1L);
            a <- which(a);
            stopifnot(length(a) == 1L);
            text[[j]] <- paste0(text[[j]], "|`", names.name[[a]], "`");
          }

          text[[j]] <- paste0(text[[j]], "|");
          q <- (row$best.f.min <= f.min.min);
          if(q) {
            text[[j]] <- paste0(text[[j]], "**");
          }
          text[[j]] <- paste0(text[[j]], row$best.f.min);
          if(q) {
            text[[j]] <- paste0(text[[j]], "**");
          }

          text[[j]] <- paste0(text[[j]], "|");
          q <- (row$best.f.mean <= f.mean.min);
          if(q) {
            text[[j]] <- paste0(text[[j]], "**");
          }
          text[[j]] <- paste0(text[[j]], round(row$best.f.mean));
          if(q) {
            text[[j]] <- paste0(text[[j]], "**");
          }

          text[[j]] <- paste0(text[[j]], "|");
          q <- (row$best.f.med <= f.med.min);
          if(q) {
            text[[j]] <- paste0(text[[j]], "**");
          }
          text[[j]] <- paste0(text[[j]], row$best.f.med);
          if(q) {
            text[[j]] <- paste0(text[[j]], "**");
          }

          text[[j]] <- paste0(text[[j]], "|");
          q <- (row$best.f.sd <= f.sd.min);
          if(q) {
            text[[j]] <- paste0(text[[j]], "**");
          }
          text[[j]] <- paste0(text[[j]], round(row$best.f.sd));
          if(q) {
            text[[j]] <- paste0(text[[j]], "**");
          }

          text[[j]] <- paste0(text[[j]], "|");
          q <- (row$last.improvement.time.med <= t.med.min);
          if(q) {
            text[[j]] <- paste0(text[[j]], "**");
          }
          text[[j]] <- paste0(text[[j]], round(row$last.improvement.time.med/1000L));
          if(q) {
            text[[j]] <- paste0(text[[j]], "**");
          }

          text[[j]] <- paste0(text[[j]], "s|");

          q <- (row$last.improvement.fes.med <= fes.med.min);
          if(q) {
            text[[j]] <- paste0(text[[j]], "**");
          }
          text[[j]] <- paste0(text[[j]], round(row$last.improvement.fes.med));
          if(q) {
            text[[j]] <- paste0(text[[j]], "**");
          }
          text[[j]] <- paste0(text[[j]], "|");
        }
      }

    } else {
      config$logger("no problem occurs multiple times");

      for(i in seq_len(nrow(frame))) {
        row <- frame[i,];
        j <- j + 1L;
        if(printInst) {
          ii <- as.character(row$instance);
          lb <- features$inst.name == ii;
          stopifnot(sum(lb) == 1L);
          lb <- which(lb);
          stopifnot(length(lb) == 1L);
          lb <- features$inst.opt.bound.lower[[lb]];
          stopifnot(is.integer(lb), lb > 0L);
          text[[j]] <- paste0("|`", ii, "`|", lb);
        }
        if(printAlgo) {
          a <- as.character(row$algorithm) == names.setup;
          stopifnot(sum(a) == 1L);
          a <- which(a);
          stopifnot(length(a) == 1L);
          text[[j]] <- paste0(text[[j]], "|`", names.name[[a]], "`");
        }
        text[[j]] <- paste0(text[[j]],  "|",
                          row$best.f.min, "|",
                          round(row$best.f.mean), "|",
                          row$best.f.med, "|",
                          round(row$best.f.sd), "|",
                          round(row$last.improvement.time.med / 1000), "s|",
                          row$last.improvement.fes.med, "|");
      }
    }

    writeLines(text=text, con=path);
    config$logger("done writing file '", path, "'.");
  }

  path <- normalizePath(path, mustWork = TRUE);
  path <- force(path);
  return(path);
}
