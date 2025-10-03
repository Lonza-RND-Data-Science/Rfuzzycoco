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




.complete_params_with_defaults <- 
test_that("complete_params_with_defaults", {
  lst <- complete_params_with_defaults(NULL)
  expect_identical(lst, params(NA, NA))
  
  ###
  lst <- list(
    extra = 2,
    global_params = list(nb_rules = 3, extra2 = "toto"),
    input_vars_params = list(nb_bits_sets = 0)
  )
  lst2 <- complete_params_with_defaults(lst)
  expected <- params(nb_rules = 3, nb_max_var_per_rule = NA, ivars.nb_bits_sets = 0)
  expected$extra <- 2
  expected$global_params$extra2 <- "toto"  

  expect_mapequal(lst2, expected)
})

.remove_comments <- 
test_that("remove_comments", {
  CONTENT_REF <- r"({
    "Temperature": {
     "Cold": 10,
     "Warm": 20.1,
      "Hot":  30
    }
  })"

  CONTENT_COMMENTED = r"(
{
# comment1 starting at col 1
    "Temperature": {
      # comment2
     "Cold": 10, # inline comment 3
     "Warm": 20.1,
       # "toto": "titi" commented line
      "Hot":  30
    }
       # comment4
  }# inline comment 5 )"

  lines <- strsplit(CONTENT_COMMENTED, "\n")[[1]]
  res <- remove_comments(lines)
  line <- paste0(res, collapse = "\n")
  expect_equal(line, CONTENT_REF)
})


.load_params_json <- 
test_that("load_params_json", {
  res <- load_params_json(IRIS36_PARAMS_PATH)
  
  expected <- params(nb_rules = 3, nb_max_var_per_rule = 3, 
    ivars.nb_sets = 2, ivars.nb_bits_vars = 14, ivars.nb_bits_sets = 1, ivars.nb_bits_pos = 10,
    ovars.nb_sets = 2, ovars.nb_bits_vars = 1, ovars.nb_bits_sets = 1, ovars.nb_bits_pos = 1,
    rules.pop_size = 100, rules.elite_size = 3, rules.cx_prob = 0.8, rules.mut_flip_genome = 0.8, rules.mut_flip_bit = 0.1,
    mfs.pop_size = 100, mfs.elite_size = 3, mfs.cx_prob = 0.8, mfs.mut_flip_genome = 0.8, mfs.mut_flip_bit = 0.1,
    output_vars_defuzz_thresholds = 0.5, metricsw.rmse = 0.2, metricsw.distanceThreshold = 0.2)

  expect_mapequal(res, expected)
})

.params <- 
test_that("params", {
  lst <- params(100, 33)

  expect_true(is.list(lst))
  expect_equal(lst$global_params$nb_rules, 100)
  expect_equal(lst$global_params$nb_max_var_per_rule, 33)
})

