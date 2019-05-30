library("aitoaEvaluate");
library("testthat");
context("aitoa.algo.setup");

test_that("Test aitoa.algorithm.setup", {
  r1 <- aitoa.algorithm.setup.rs("rs");
  r2 <- aitoa.algorithm.setup("rs");
  expect_identical(r1, r2);
  expect_identical(r1$algo.algorithm, "rs");
  expect_identical(r1$algo.representation, "default");
  expect_identical(r1$algo.isHybrid, FALSE);

  r1 <- aitoa.algorithm.setup.hc("hc_1swapR");
  r2 <- aitoa.algorithm.setup("hc_1swapR");
  expect_identical(r1, r2);
  expect_identical(r1$algo.algorithm, "hc");
  expect_identical(r1$algo.representation, "default");
  expect_identical(r1$algo.unaryOperator, "1swapR");
  expect_identical(r1$algo.restarts, FALSE);
  expect_identical(r1$algo.isHybrid, FALSE);

  r1 <- aitoa.algorithm.setup.hc("hc_rs_256_0d05_1swapR");
  r2 <- aitoa.algorithm.setup("hc_rs_256_0d05_1swapR");
  expect_identical(r1, r2);
  expect_identical(r1$algo.algorithm, "hc");
  expect_identical(r1$algo.representation, "default");
  expect_identical(r1$algo.unaryOperator, "1swapR");
  expect_identical(r1$algo.restartStrategy, 256L);
  expect_identical(r1$algo.restartParameter, 0.05);
  expect_identical(r1$algo.restarts, TRUE);
  expect_identical(r1$algo.isHybrid, FALSE);

  r1 <- aitoa.algorithm.setup.sa("sa_exp_20_0d0000002_1swapR");
  r2 <- aitoa.algorithm.setup("sa_exp_20_0d0000002_1swapR");
  expect_identical(r1, r2);
  expect_identical(r1$algo.algorithm, "sa");
  expect_identical(r1$algo.representation, "default");
  expect_identical(r1$algo.unaryOperator, "1swapR");
  expect_identical(r1$algo.temperatureSchedule, "exp");
  expect_identical(r1$algo.temperatureStart, 20L);
  expect_identical(r1$algo.temperatureEpsilon, 0.0000002);
  expect_identical(r1$algo.isHybrid, FALSE);

  r1 <- aitoa.algorithm.setup.ea("eap_16+16@0d3_1swapR_sequence");
  r2 <- aitoa.algorithm.setup("eap_16+16@0d3_1swapR_sequence");
  expect_identical(r1, r2);
  expect_identical(r1$algo.algorithm, "ea");
  expect_identical(r1$algo.representation, "default");
  expect_identical(r1$algo.unaryOperator, "1swapR");
  expect_identical(r1$algo.binaryOperator, "sequence");
  expect_identical(r1$algo.mu, 16L);
  expect_identical(r1$algo.lambda, 16L);
  expect_identical(r1$algo.cr, 0.3);
  expect_identical(r1$algo.fitness, "pruning");
  expect_identical(r1$algo.isHybrid, FALSE);

  r1 <- aitoa.algorithm.setup.ea("ea_ffa_16+16@0d3_1swapR_sequence");
  r2 <- aitoa.algorithm.setup("ea_ffa_16+16@0d3_1swapR_sequence");
  expect_identical(r1, r2);
  expect_identical(r1$algo.algorithm, "ea");
  expect_identical(r1$algo.representation, "default");
  expect_identical(r1$algo.unaryOperator, "1swapR");
  expect_identical(r1$algo.binaryOperator, "sequence");
  expect_identical(r1$algo.mu, 16L);
  expect_identical(r1$algo.lambda, 16L);
  expect_identical(r1$algo.cr, 0.3);
  expect_identical(r1$algo.fitness, "ffa");
  expect_identical(r1$algo.isHybrid, FALSE);

  r1 <- aitoa.algorithm.setup.ea("ea_16+16@0d3_1swapR_sequence");
  r2 <- aitoa.algorithm.setup("ea_16+16@0d3_1swapR_sequence");
  expect_identical(r1, r2);
  expect_identical(r1$algo.algorithm, "ea");
  expect_identical(r1$algo.representation, "default");
  expect_identical(r1$algo.unaryOperator, "1swapR");
  expect_identical(r1$algo.binaryOperator, "sequence");
  expect_identical(r1$algo.mu, 16L);
  expect_identical(r1$algo.lambda, 16L);
  expect_identical(r1$algo.cr, 0.3);
  expect_identical(r1$algo.fitness, "direct");
  expect_identical(r1$algo.isHybrid, FALSE);

  r1 <- aitoa.algorithm.setup.ma("ma_16+16_1swapR_sequence");
  r2 <- aitoa.algorithm.setup("ma_16+16_1swapR_sequence");
  expect_identical(r1, r2);
  expect_identical(r1$algo.algorithm, "ma");
  expect_identical(r1$algo.representation, "default");
  expect_identical(r1$algo.unaryOperator, "1swapR");
  expect_identical(r1$algo.binaryOperator, "sequence");
  expect_identical(r1$algo.mu, 16L);
  expect_identical(r1$algo.lambda, 16L);
  expect_identical(r1$algo.cr, 1L);
  expect_identical(r1$algo.fitness, "direct");
  expect_identical(r1$algo.isHybrid, TRUE);
  expect_identical(r1$algo.hybridGS, "ea");

  r1 <- aitoa.algorithm.setup.ma("map_16+16_1swapR_sequence");
  r2 <- aitoa.algorithm.setup("map_16+16_1swapR_sequence");
  expect_identical(r1, r2);
  expect_identical(r1$algo.algorithm, "ma");
  expect_identical(r1$algo.representation, "default");
  expect_identical(r1$algo.unaryOperator, "1swapR");
  expect_identical(r1$algo.binaryOperator, "sequence");
  expect_identical(r1$algo.mu, 16L);
  expect_identical(r1$algo.lambda, 16L);
  expect_identical(r1$algo.cr, 1L);
  expect_identical(r1$algo.fitness, "pruning");
  expect_identical(r1$algo.isHybrid, TRUE);
  expect_identical(r1$algo.hybridGS, "ea");

  r1 <- aitoa.algorithm.setup.ma("ma_ffa_16+26_1swapR_sequence");
  r2 <- aitoa.algorithm.setup("ma_ffa_16+26_1swapR_sequence");
  expect_identical(r1, r2);
  expect_identical(r1$algo.algorithm, "ma");
  expect_identical(r1$algo.representation, "default");
  expect_identical(r1$algo.unaryOperator, "1swapR");
  expect_identical(r1$algo.binaryOperator, "sequence");
  expect_identical(r1$algo.mu, 16L);
  expect_identical(r1$algo.lambda, 26L);
  expect_identical(r1$algo.cr, 1L);
  expect_identical(r1$algo.fitness, "ffa");
  expect_identical(r1$algo.isHybrid, TRUE);
  expect_identical(r1$algo.hybridGS, "ea");

  r1 <- aitoa.algorithm.setup.eda("eda_umda_1024+2048");
  r2 <- aitoa.algorithm.setup("eda_umda_1024+2048");
  expect_identical(r1, r2);
  expect_identical(r1$algo.algorithm, "eda");
  expect_identical(r1$algo.representation, "default");
  expect_identical(r1$algo.mu, 1024L);
  expect_identical(r1$algo.lambda, 2048L);
  expect_identical(r1$algo.model, "umda");
  expect_identical(r1$algo.fitness, "direct");
  expect_identical(r1$algo.isHybrid, FALSE);

  r1 <- aitoa.algorithm.setup.eda("eda_umda_ffa_1024+2048");
  r2 <- aitoa.algorithm.setup("eda_umda_ffa_1024+2048");
  expect_identical(r1, r2);
  expect_identical(r1$algo.algorithm, "eda");
  expect_identical(r1$algo.representation, "default");
  expect_identical(r1$algo.mu, 1024L);
  expect_identical(r1$algo.lambda, 2048L);
  expect_identical(r1$algo.fitness, "ffa");
  expect_identical(r1$algo.model, "umda");
  expect_identical(r1$algo.isHybrid, FALSE);

  r1 <- aitoa.algorithm.setup.heda("heda_umda_1024+2048_1swapR");
  r2 <- aitoa.algorithm.setup("heda_umda_1024+2048_1swapR");
  expect_identical(r1, r2);
  expect_identical(r1$algo.algorithm, "eda");
  expect_identical(r1$algo.representation, "default");
  expect_identical(r1$algo.mu, 1024L);
  expect_identical(r1$algo.lambda, 2048L);
  expect_identical(r1$algo.fitness, "direct");
  expect_identical(r1$algo.isHybrid, TRUE);
  expect_identical(r1$algo.hybridGS, "eda");
  expect_identical(r1$algo.model, "umda");
  expect_identical(r1$algo.unaryOperator, "1swapR");

  r1 <- aitoa.algorithm.setup.heda("heda_umda_ffa_1024+2048_1swapR");
  r2 <- aitoa.algorithm.setup("heda_umda_ffa_1024+2048_1swapR");
  expect_identical(r1, r2);
  expect_identical(r1$algo.algorithm, "eda");
  expect_identical(r1$algo.representation, "default");
  expect_identical(r1$algo.mu, 1024L);
  expect_identical(r1$algo.lambda, 2048L);
  expect_identical(r1$algo.fitness, "ffa");
  expect_identical(r1$algo.isHybrid, TRUE);
  expect_identical(r1$algo.hybridGS, "eda");
  expect_identical(r1$algo.model, "umda");
  expect_identical(r1$algo.unaryOperator, "1swapR");

  r1 <- aitoa.algorithm.setup.hc2("hc2f_rs_12swapR");
  r2 <- aitoa.algorithm.setup("hc2f_rs_12swapR");
  expect_identical(r1, r2);
  expect_identical(r1$algo.algorithm, "hc2");
  expect_identical(r1$algo.representation, "default");
  expect_identical(r1$algo.unaryOperator, "12swapR");
  expect_identical(r1$algo.restartStrategy, "afterEnumeration");
  expect_identical(r1$algo.restarts, TRUE);
  expect_identical(r1$algo.isHybrid, FALSE);
})

