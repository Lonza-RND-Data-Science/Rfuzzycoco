.fuzzycoco <- 
test_that("fuzzycoco", {
  pms <- params(nb_rules = 1, nb_max_var_per_rule = 3)

  ### CLASSIFICATION
  # params checks
  pms$output_vars_params$nb_sets <- NA
  expect_error(fuzzycoco("classification", pms), "bad output_vars_param")
  pms$output_vars_params$nb_sets <- 3
  expect_error(fuzzycoco("classification", pms), "bad output_vars_param")
  pms$output_vars_params$nb_sets <- 2
  pms$output_vars_params$nb_bits_sets <- 1
 
  model <- fuzzycoco("classification", pms, seed = 123, verbose = TRUE)

  expect_s3_class(model, "fuzzycoco_model")
  expect_identical(model$mode, CLASSIFICATION)
  expect_identical(model$params, pms)
  expect_equal(model$seed, 123)
  expect_true(model$verbose)

  ### REGRESSION
  model2 <- fuzzycoco("regression", pms, seed = 123, verbose = TRUE)

  expect_s3_class(model2, "fuzzycoco_model")
  expect_identical(model2$mode, REGRESSION)

  ### edge cases
  expect_error(fuzzycoco("toto", pms), "should be one of")
})
