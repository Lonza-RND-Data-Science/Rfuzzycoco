



.fuzzycoco_fit_df_rcpp <- 
test_that("fuzzycoco_fit_df_rcpp", {
  CASE <- example_mtcars()
  df <- CASE$data

  ####################### regression one variable ########################
  pms <- CASE$params

  model <- fuzzycoco("regression", pms, seed = 123)
  
  fit <- fuzzycoco_fit_df_rcpp(model, df, responses = "qsec")
  
  expect_s3_class(fit, "fuzzycoco_fit")
  expect_identical(fit$engine, "rcpp")
  expect_identical(fit$mode, REGRESSION)
  expect_equal(fit$seed, 123)


  # reproducible
  fit2 <- fuzzycoco_fit_df_rcpp(model, df, responses = "qsec")
  expect_identical(fit2, fit)

  # different seed
  model2 <- fuzzycoco("regression", pms, seed = 321)
  fit3 <- fuzzycoco_fit_df_rcpp(model2, df, responses = "qsec")
  expect_false(fit3$fit$fitness == fit$fit$fitness)

  ### edge cases
  # no threshold
  pms <- CASE$params
  pms$fitness_params$output_vars_defuzz_thresholds <- list()
  model <- fuzzycoco("regression", pms, seed = 123)

  expect_error(fuzzycoco_fit_df_rcpp(model, df, responses = "qsec"), "output_vars_defuzz_thresholds")

  # too many thresholds
  pms$fitness_params$output_vars_defuzz_thresholds <- list(1, 2)
  model <- fuzzycoco("regression", pms, seed = 123) 
  
  expect_error(fuzzycoco_fit_df_rcpp(model, df, responses = "qsec"), "output_vars_defuzz_thresholds")


  ####################### regression two variables ########################
  pms$fitness_params$output_vars_defuzz_thresholds <- NA
  model <- fuzzycoco("regression", pms, seed = 123)
  df <- CASE$data
  
  model$params$global_params$max_generations <- 10
  fit <- fuzzycoco_fit_df_rcpp(model, df[c("qsec", "hp", "wt")], responses = c("qsec", "hp"))

  expect_s3_class(fit, "fuzzycoco_fit")
  expect_equal(fit$fit$generations, 10)
  expect_identical(names(fit$fuzzy_system$variables$output), c("qsec", "hp"))

  ##################### categorical vars 
  ### in response
  CASE <- example_iris_binary_categorical()
  pms$fitness_params$output_vars_defuzz_thresholds <- NA
  model <- fuzzycoco("regression", pms, seed = 123)

  expect_error(fuzzycoco_fit_df_rcpp(model, CASE$data, responses = "Species"), "only numeric columns")
})
