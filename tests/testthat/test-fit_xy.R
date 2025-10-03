
.fit_xy.fuzzycoco_model__classification <- 
test_that("fit_xy.fuzzycoco_model__classification", {
  CASE <- example_mtcars()
  df <- CASE$data
  pms <- CASE$params
  pms$global_params$max_generations <- 20


  ####################### classification: one  variable ########################
  pms$output_vars_params$nb_sets <- 2
  pms$output_vars_params$nb_bits_sets <- 1
  model <- fuzzycoco("classification", pms, seed = 123)

  x <- df[-4]
  y <- df[4]

  expect_error(fit_xy(model, x, y), "only binary response")  
  y2 <- bin_continuous_responses_to_01(y)
  fit <- fit_xy(model, x, y2, engine = "rcpp")

  ref_model <- model
  ref_model$params$global_params$max_generations <- 20

  y2 <- y
  y2[[1]] <- process_classification_response(y[[1]], median(y[[1]]))
  df2 <- cbind(x, y2)
  ref_fit <- fuzzycoco_fit_df_rcpp(ref_model, df2, responses = "qsec")
  expect_identical(fit, ref_fit)

  # hybrid engine
  fit2 <- fit_xy(model, x, y2, engine = "hybrid")
  fit2$engine <- fit2$options <- fit$engine <- NULL
  fit2$engine <- fit$engine <- fit2$infos <- NULL
  expect_identical(fit2, fit)

  ####################### classification 2 variables and selected regressor ########################
  model <- fuzzycoco("classification", pms, seed = 123)
  x <- df["wt"]
  y <- df[c("qsec", "hp")]

  y2 <- bin_continuous_responses_to_01(y)

  fit <- fit_xy(model, x, y2, engine = "rcpp")

  ref_model <- model
  df2 <- cbind(x, y2)

  ref_fit <- fuzzycoco_fit_df_rcpp(ref_model, df2, responses = c("qsec", "hp"))
  expect_identical(fit, ref_fit)

  # hybrid engine
  fit2 <- fit_xy(model, x, y2, engine = "hybrid")
  fit2$engine <- fit$engine <- fit2$infos <- NULL
  expect_identical(fit2, fit)
})



.fit_xy.fuzzycoco_model__regression <- 
test_that("fit_xy.fuzzycoco_model__regression", {
  CASE <- example_mtcars()
  df <- CASE$data
  pms <- CASE$params
  pms$fitness_params$output_vars_defuzz_thresholds <- list(20)

  ####################### regression one variable ########################
  model <- fuzzycoco("regression", pms, seed = 123)
  
  x <- df[-4]
  y <- df[4]

  fit <- fit_xy(model, x, y, engine = "rcpp", max_generations = 20)

  ref_model <- model
  ref_model$params$global_params$max_generations <- 20
  ref_fit <- fuzzycoco_fit_df_rcpp(ref_model, df, responses = "qsec")
  expect_identical(fit, ref_fit)

  # hybrid engine
  fit2 <- fit_xy(model, x, y, engine = "hybrid", max_generations = 20)
  fit2$engine <- fit$engine <- fit2$infos <- NULL
  expect_identical(fit2, fit)

  ####################### regression 2 variables and selected regressor ########################
  pms$fitness_params$output_vars_defuzz_thresholds <- list(20, 10)
  model <- fuzzycoco("regression", pms, seed = 123)

  x <- df["wt"]
  y <- df[c("qsec", "hp")]
  fit <- fit_xy(model, x, y, engine = "rcpp", max_generations = 10)

  ref_model <- model
  ref_model$params$global_params$max_generations <- 10
  ref_fit <- fuzzycoco_fit_df_rcpp(ref_model, df[c("qsec", "hp", "wt")], responses = c("qsec", "hp"))
  expect_identical(fit, ref_fit)

  ##################### edge cases 
  model <- fuzzycoco("regression", pms, seed = 123)
  expect_error(fit_xy(model, iris, iris["Species"], engine = "rcpp"), "non numerical response")

  ##################### categorical vars 
  CASE <- IRIS_CATEGORICAL_BINARY()
  df <- CASE$data
  pms$fitness_params$output_vars_defuzz_thresholds <- 0.5
  model <- fuzzycoco("regression", pms, seed = 123)

  x <- df[-1]
  y <- df[1]
  expect_error(fit_xy(model, x, y), "only numeric columns")
})

