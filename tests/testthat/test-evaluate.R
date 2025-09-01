.evaluate.fuzzycoco_fit <- 
test_that("evaluate.fuzzycoco_fit", {
  CASE <- MTCARS_MULTI_OUTPUT()
  df <- CASE$data
  pms <- CASE$params

  ####################### regression one variable: hp ########################
  pms$fitness_params$output_vars_defuzz_thresholds <- 100
  model <- fuzzycoco("regression", pms, seed = 123)
  x <- df[-2]
  y <- df[2]
  fit <- fit_xy(model, x, y, engine = "rcpp", max_generations = 30)

  res <- evaluate(fit, cbind(x, y))

  ref <- fit$fit
  ref$generations <- NULL
  expect_equal(res, ref)

  # a data frame not in order
  res2 <- evaluate(fit, cbind(y, x))
  expect_identical(res2, res)


  ####################### regression 2 variables and 2 regressors ########################
  pms$fitness_params$output_vars_defuzz_thresholds <- list(20, 150)
  model <- fuzzycoco("regression", pms, seed = 123)
  responses <- c("qsec", "hp")
  x <- df[setdiff(names(df), responses)]; y <- df[responses]
  fit <- fit_xy(model, x, y, engine = "rcpp", max_generations = 50)

  res <- evaluate(fit, cbind(x, y))

  y2 <- predict(fit, x)

  ref <- fit$fit
  ref$generations <- NULL
  expect_equal(res, ref)

  ####################### classification one variable: wt ########################
  pms$output_vars_params$nb_sets <- 2
  pms$output_vars_params$nb_bits_sets <- 1
  model <- fuzzycoco("classification", pms, seed = 123)

  response <- "wt"
  x <- df[setdiff(names(df), response)]
  y0 <- df[response]
  y <- bin_continuous_responses_to_01(y0)
  fit <- fit_xy(model, x, y, engine = "rcpp", max_generations = 30)

  res <- evaluate(fit, cbind(y, x))

  ref <- fit$fit
  ref$generations <- NULL
  expect_equal(res, ref)

  ####################### classification 3 variables and 1 regressors ########################
  model <- fuzzycoco("classification", pms, seed = 123)
  responses <- c("qsec", "wt", "hp")
  x <- df[setdiff(names(df), responses)]
  y0 <- df[responses]
  y <- bin_continuous_responses_to_01(y0)
  fit <- fit_xy(model, x, y, engine = "rcpp", max_generations = 50)

  res <- evaluate(fit, cbind(x, y))

  ref <- fit$fit
  ref$generations <- NULL
  expect_equal(res, ref)
})
