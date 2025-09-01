.predict.fuzzycoco_fit <- 
test_that("predict.fuzzycoco_fit", {
  CASE <- MTCARS_MULTI_OUTPUT()
  df <- CASE$data
  pms <- CASE$params
  # N.B: no threshold set
  pms$fitness_params$output_vars_defuzz_thresholds <- list()

  ####################### classification one variable ########################
  pms$output_vars_params$nb_sets <- 2
  pms$output_vars_params$nb_bits_sets <- 1

  model <- fuzzycoco("classification", pms, seed = 123)
  x <- df[-4]
  y0 <- df[4]

  y <- bin_continuous_responses_to_01(y0)

  fit <- fit_xy(model, x, y, engine = "rcpp")

  y2num <- predict(fit, x, bin = FALSE)
  expect_equal(rmse(y2num[[1]], y[[1]]), fit$fit$metrics$rmse)
  expect_equal(rae(y2num[[1]], y[[1]]), fit$fit$metrics$rae)

  y2 <- predict(fit, x)
  expect_identical(y2, bin_continuous_responses_to_01(y2num, 0.5))

  met <- classification_metrics(y2[[1]], y[[1]])
  expect_equal(met$sensitivity, fit$fit$metrics$sensitivity)
  expect_equal(met$specificity, fit$fit$metrics$specificity)
  expect_equal(met$accuracy, fit$fit$metrics$accuracy)
  mse(y2[[1]], y0[[1]])

  ####################### classification 2 variables and selected regressor ########################
  model <- fuzzycoco("classification", pms, seed = 123)
  x <- df["wt"]
  y0 <- df[c("qsec", "hp")]

  y <- bin_continuous_responses_to_01(y0)
  fit <- fit_xy(model, x, y, engine = "rcpp")

  y2 <- predict(fit, x, bin = FALSE)

  mse_df <- apply_column_metric_to_dfs(mse)
  rmse_df <- apply_column_metric_to_dfs(rmse)
  expect_equal(mse_df(y2, y), fit$fit$metrics$mse)
  expect_equal(rmse_df(y2, y), fit$fit$metrics$rmse)

  y2bin <- predict(fit, x)

  met0 <- fit$fit$metrics
  sensitivity <- function(y, y0) classification_metrics(y, y0)$sensitivity
  specificity <- function(y, y0) classification_metrics(y, y0)$specificity
  sensitivity_df <- apply_column_metric_to_dfs(sensitivity)
  specificity_df <- apply_column_metric_to_dfs(specificity)

  expect_equal(sensitivity_df(y2bin, y), met0$sensitivity)
  expect_equal(specificity_df(y2bin, y), met0$specificity)

  ref_model <- model
  df2 <- cbind(x, y)

  ref_model$params$fitness_params$output_vars_defuzz_thresholds <- as.list(rep(0.5, 2))
  ref_fit <- fuzzycoco_fit_df_rcpp(ref_model, df2, responses = c("qsec", "hp"))
  expect_identical(fit, ref_fit)

  ####################### regression one variable ########################
  pms$fitness_params$output_vars_defuzz_thresholds <- 20
  model <- fuzzycoco("regression", pms, seed = 123)
  x <- df[-4]
  y <- df[4]
  fit <- fit_xy(model, x, y, engine = "rcpp", max_generations = 20)

  y2 <- predict(fit, x)

  expect_equal(rmse(y2[[1]], y[[1]]), fit$fit$metrics$rmse)
  expect_equal(mse(y2[[1]], y[[1]]), fit$fit$metrics$mse)

  ####################### regression 2 variables and selected regressor ########################
  pms$fitness_params$output_vars_defuzz_thresholds <- list(20, 150)
  model <- fuzzycoco("regression", pms, seed = 123)
  x <- df["wt"]; y <- df[c("qsec", "hp")]
  fit <- fit_xy(model, x, y, engine = "rcpp", max_generations = 50)

  y2 <- predict(fit, x)

  expect_equal(dim(y2), dim(y))
  expect_equal(mse_df(y2, y), fit$fit$metrics$mse)
  expect_equal(rmse_df(y2, y), fit$fit$metrics$rmse)
})
