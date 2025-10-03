.fit.fuzzycoco_model <- 
test_that("fit.fuzzycoco_model", {
  CASE <- example_mtcars()
  df <- CASE$data
  pms <- CASE$params


  ####################### regression one variable ########################
  model <- fuzzycoco("regression", pms, seed = 123)

  fit <- fit(model, qsec ~ ., df, engine = "rcpp", seed = 456, max_generations = 20)

  ref_model <- model
  ref_model$seed <- 456
  ref_model$params$global_params$max_generations <- 20
  ref_fit <- fuzzycoco_fit_df_rcpp(ref_model, df, responses = "qsec")
  expect_identical(fit, ref_fit)

  ####################### regression 2 variables and selected regressor ########################
  model <- fuzzycoco("regression", pms, seed = 123)
  
  fit <- fit(model, cbind(qsec, hp) ~ wt, df, engine = "rcpp", max_generations = 10)

  ref_model <- model
  ref_model$params$global_params$max_generations <- 10
  ref_fit <- fuzzycoco_fit_df_rcpp(ref_model, df[c("qsec", "hp", "wt")], responses = c("qsec", "hp"))
  expect_identical(fit, ref_fit)

  # hybrid engine
  fit2 <- fit(model, cbind(qsec, hp) ~ wt, df, engine = "hybrid", max_generations = 10)
  fit2$engine <- fit$engine <- fit2$infos <- NULL
  expect_identical(fit2, fit)

  ####################### classification one variable ########################

  # fix output_vars_params
  pms$output_vars_params$nb_sets <- 2
  pms$output_vars_params$nb_bits_sets <- 1
  model <- fuzzycoco("classification", pms, seed = 123)

  df_wt <- df
  df_wt$wt <- bin_continuous_response_to_01(df_wt$wt)

  fit <- fit(model, wt ~ ., df_wt, engine = "rcpp")

  responses <- c("wt")
  x <- df[setdiff(names(df), responses)]
  y0 <- df[responses]
  y <- bin_continuous_responses_to_01(y0)
  ref_fit <- fit_xy(model, x, y, engine = "rcpp")
  expect_identical(fit, ref_fit)

  ####################### classification 2 variables and selected regressor ########################
  model <- fuzzycoco("classification", pms, seed = 123)

  dfbinned <- df
  dfbinned$qsec <- bin_continuous_response_to_01(dfbinned$qsec)
  dfbinned$hp <- bin_continuous_response_to_01(dfbinned$hp)
  
  fit <- fit(model, cbind(qsec, hp) ~ wt, dfbinned, engine = "rcpp", max_generations = 10)

  responses <- c("qsec", "hp")
  x <- df["wt"]
  y0 <- df[responses]
  y <- bin_continuous_responses_to_01(y0)
  ref_fit <- fit_xy(model, x, y, engine = "rcpp", max_generations = 10)
  expect_identical(fit, ref_fit)

  ##################### categorical vars 
  CASE <- IRIS_CATEGORICAL_BINARY()

  pms$fitness_params$output_vars_defuzz_thresholds <- 0.5
  model <- fuzzycoco("regression", pms, seed = 123)

  expect_error(fit(model, Petal.Length ~ ., CASE$data), "only numeric columns")
})
