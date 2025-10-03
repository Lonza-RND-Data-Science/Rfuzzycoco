.resolve_params <- 
test_that("resolve_params", {

  params <- params(1, 2)
  
  ### output_vars_defuzz_thresholds for regression
  ## NA
  # one response
  params$fitness_params$output_vars_defuzz_thresholds <- NA
  y <- mtcars['qsec']
  expect_error(check_params(params, 1), 'bad param "output_vars_defuzz_thresholds"')

  params2 <- resolve_params(params, y, TRUE)

  expect_no_error(check_params(params2, 1))
  expect_equal(params2$fitness_params$output_vars_defuzz_thresholds[[1]], median(y[[1]]))

  params2 <- resolve_params(params, y, FALSE)

  expect_no_error(check_params(params2, 1))
  expect_equal(params2$fitness_params$output_vars_defuzz_thresholds[[1]], 0.5)

  # two responses
  params$fitness_params$output_vars_defuzz_thresholds <- NA
  y <- mtcars[c('qsec', 'wt')]
  expect_error(check_params(params, 1), 'bad param "output_vars_defuzz_thresholds"')

  params2 <- resolve_params(params, y, TRUE)

  expect_no_error(check_params(params2, 2))
  expect_equal(params2$fitness_params$output_vars_defuzz_thresholds[[1]], median(y[[1]]))
  expect_equal(params2$fitness_params$output_vars_defuzz_thresholds[[2]], median(y[[2]]))

  params2 <- resolve_params(params, y, FALSE)

  expect_no_error(check_params(params2, 2))
  expect_equal(params2$fitness_params$output_vars_defuzz_thresholds[[1]], 0.5)
  expect_equal(params2$fitness_params$output_vars_defuzz_thresholds[[2]], 0.5)

  # set
  params$fitness_params$output_vars_defuzz_thresholds <- -1.111

  params2 <- resolve_params(params, y, TRUE)
  expect_identical(params2, params)

  params2 <- resolve_params(params, y, FALSE)
  expect_identical(params2, params)
})



.params <- 
test_that("params", {
  lst <- params(100, 33)

  expect_true(is.list(lst))
  expect_equal(lst$global_params$nb_rules, 100)
  expect_equal(lst$global_params$nb_max_var_per_rule, 33)
})

