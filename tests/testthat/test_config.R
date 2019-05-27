library("aitoaEvaluate");
library("testthat");
context("aitoa.config");

test_that("Test aitoa.config", {
  dir <- tempfile();
  dir.create(dir);
  config <- aitoa.config(dir.results=dir);
  unlink(dir, force=TRUE, recursive = TRUE);
  expect_false(dir.exists(dir));

  expect_identical(names(config), c("dir.results",
                                    "dir.evaluation",
                                    "logger",
                                    "min.instances",
                                    "min.runs"));

  expect_length(config, 5L);
  expect_identical(config[[1L]], dir);
  expect_identical(config$dir.results, dir);
  expect_true(is.character(config[[2L]]));
  expect_true(is.function(config$logger));
  config$logger("this ", "is", " a", " test");
  expect_true(is.integer(config[[4L]]));
  expect_gt(config$min.instances, 0L);
  expect_true(is.integer(config[[5L]]));
  expect_gt(config$min.runs, 0L);
})
