.fs_rules_to_df <- 
test_that("fs_rules_to_df", {
  CASE <- MTCARS_MULTI_OUTPUT()

  pms <- CASE$params
  pms$fitness_params$output_vars_defuzz_thresholds <- list(0, 0)
  model <- fuzzycoco("regression", pms, seed = 123)
  fit <- fit(model, cbind(qsec, hp) ~ wt, CASE$data, max_generations = 10)

  ### rules_desc_to_df
  rules_df <- fs_rules_to_df(fit$fuzzy_system)

  expect_identical(rules_df, 
    data.frame(wt = c(1, 1, 0), qsec = c(2, 0, 1), hp = c(0, 1, 1), row.names = c("rule 1", "rule 2", "default rule")) )
  
  used_df <- fs_used_vars_to_df(fit$fuzzy_system)
  expect_identical(used_df, data.frame(wt = TRUE, qsec = TRUE, hp = TRUE))

  fit_df <- fit_to_df(fit, metric = "rmse")
  expect_equal(fit_df, data.frame(wt = TRUE, qsec = TRUE, hp = TRUE, fitness = 3.814824e-11, 
    generations = 10L, seed = 123, rmse = 34.6095926))

  #######

  pms$global_params$nb_rules <- 5
  pms$global_params$nb_max_var_per_rule <- 2
  model <- fuzzycoco("regression", pms, seed = 123)
  fit <- fit(model, cbind(qsec, hp) ~ ., CASE$data, seed = 345, max_generations = 33)

  rules_df <- fs_rules_to_df(fit$fuzzy_system)
  expect_identical(rules_df, 
    data.frame(
      mpg = c(0, 1, 1, 2, 0, 0), 
      wt = c(2, 0, 0, 0, 2, 0), 
      qsec = c(0, 1, 0, 0, 0, 2), 
      hp = c(2, 0, 3, 1, 3, 1), 
      row.names = c("rule 1", "rule 2", "rule 3", "rule 4", "rule 5", "default rule")
    )
  )

  used_df <- fs_used_vars_to_df(fit$fuzzy_system)
  expect_identical(used_df, data.frame(mpg = TRUE, wt = TRUE, qsec = TRUE, hp = TRUE))

  
  ### 
  pms$global_params$nb_rules <- 1
  pms$global_params$nb_max_var_per_rule <- 1
  pms$fitness_params$output_vars_defuzz_thresholds <- 0
  model <- fuzzycoco("regression", pms, seed = 123)
  fit <- fit(model, qsec ~ ., CASE$data, seed = 345, max_generations = 5)

  used_df <- fs_used_vars_to_df(fit$fuzzy_system)
  expect_identical(used_df, data.frame(mpg = FALSE, hp = FALSE, wt = TRUE, qsec = TRUE))

  fit_df <- fit_to_df(fit, metric = "rmse")
  expect_equal(fit_df, data.frame(mpg = FALSE, hp = FALSE, wt = TRUE, qsec = TRUE, 
    fitness = 0.292716383967136, generations = 5L, seed = 345, rmse = 1.77242459612869))

})
