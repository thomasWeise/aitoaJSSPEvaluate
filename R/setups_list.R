#' @title Obtain the List of Directories Matching the Setups that can be Parsed
#' @description This function obtains a list of files and directories that can
#'   be used to evaluate experiments.
#' @param config the configuration
#' @return a list with directories
#' @include utils.R
#' @include config.R
#' @export aitoa.setups.list
aitoa.setups.list <- function(config=aitoa.config()) {
  file <- file.path(.dir.eval(config=config), "dirs.txt");

  if(!file.exists(file)) {
    config$logger("setups directory file '", file, "' does not yet exist, so we need to create it.");

    base <- config$dir.results;

    setups <- unlist(list.dirs(path=base, full.names=TRUE, recursive=FALSE));
    setups <- unlist(lapply(setups, function(dir) {
      ss <- unlist(list.dirs(path=dir, full.names=TRUE, recursive=FALSE));
      if(length(ss) != config$num.instances) {
        config$logger(dir, " cannot be used because it has ",
                           length(ss), " sub-dirs, while ",
                           config$num.instances, " are required.");
        return(NULL);
      }
      ss <- ss[vapply(ss, function(sss) {
        files <- list.files(sss, pattern=".+\\.txt", recursive=FALSE, include.dirs=FALSE, no..=TRUE,
                            full.names = TRUE);

        if(length(files) != config$num.runs) {
          config$logger(dir, " cannot be used because it has ",
                        length(files), " files instead of the required ",
                        config$num.runs);
          return(FALSE);
        }

        not <- vapply(files, function(ff) (file.size(ff) <= 0L), FALSE);
        if(any(not)) {
          config$logger(dir, " cannot be used because it contains files of zero size, namely ",
                        paste(files[not], sep=", ", collapse=", "), ".");
          return(FALSE);
        }

        return(TRUE);
      }, FALSE)];
      if(length(ss) != config$num.instances) {
        config$logger(dir, " cannot be used because it has ",
                      length(ss), " useable instance sub-dirs, while ",
                      config$num.instances, " are required.");
        return(NULL);
      }
      return(ss);
    }), recursive=TRUE);

    setups <- unlist(setups);
    setups <- substr(setups, (nchar(base)+2L), nchar(setups));
    setups <- setups[gregexpr(".+/.+", setups, fixed=FALSE) > 0L];
    sort(setups);
    stopifnot(length(setups) > 0L);

    config$logger("got a list of ", length(setups), " interesting directories.");
    writeLines(setups, file);
    rm(setups);
    gc();
    config$logger("file '", file, "' created successfully.");
  }

  stopifnot(file.size(file) > 1L);
  config$logger("now loading list of setup directories from file '", file, "'.");
  result <- trimws(as.character(unname(unlist(readLines(file)))));
  stopifnot(length(result) > 0L,
            is.vector(result),
            all(nchar(result) > 0L));
  config$logger("done loading list of setup directories from file '", file, "'.");
  return(result);
}
