# shortcuts for pasting methods
.paste <- function(...) paste(..., sep="", collapse="");
.pastel <- function(...) paste(..., sep=", ", collapse=", ");

# create a logger string from a set of strings
.logger <- function(...) {
  cat(.paste(Sys.time(), ": ", .paste(...), "\n"));
  invisible(TRUE);
}