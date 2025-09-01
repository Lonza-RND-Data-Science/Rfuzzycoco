

.stop_engine_if_stalling <- 
test_that("stop_engine_if_stalling", {
  CASE <- MTCARS_MULTI_OUTPUT()
  df <- CASE$data
  pms <- CASE$params
  pms$fitness_params$output_vars_defuzz_thresholds <- 20
  model <- fuzzycoco("regression", pms, seed = 123)
  responses <- c("qsec")
  x <- df[setdiff(names(df), responses)]; y <- df[responses]

  fit <- fit(model, qsec ~ ., CASE$data, engine = "hybrid", until = stop_engine_if_stalling(5))
  expect_true(fit$fit$generations <= 20)

  ### with stop_engine_on_first_of: max_generations
  fit <- fit(model, qsec ~ ., CASE$data, engine = "hybrid", 
    until = stop_engine_on_first_of(max_generations = 20, other_func = stop_engine_if_stalling(30)))
  expect_true(fit$fit$generations == 20)

  # with max_generations and max_fitness
  fit <- fit(model, qsec ~ ., CASE$data, engine = "hybrid", 
    until = stop_engine_on_first_of(max_generations = 20, max_fitness = 0.4, other_func = stop_engine_if_stalling(30)))
  expect_true(fit$fit$generations <= 15)

  # stopped by stalling
  fit <- fit(model, qsec ~ ., CASE$data, engine = "hybrid", 
    until = stop_engine_on_first_of(max_generations = 1000, max_fitness = 0.9, other_func = stop_engine_if_stalling(5)))
  expect_true(fit$fit$generations <= 20)
})

.fuzzycoco_fit_df_hybrid <- 
test_that("fuzzycoco_fit_df_hybrid", {
  CASE <- MTCARS_MULTI_OUTPUT()
  df <- CASE$data

  ####################### regression one variable ########################
  pms <- CASE$params
  pms$fitness_params$output_vars_defuzz_thresholds <- list(20)
  model <- fuzzycoco("regression", pms, seed = 123)

  responses <- c("qsec")
  x <- df[setdiff(names(df), responses)]; y <- df[responses]

  fit <- fuzzycoco_fit_df_hybrid(model, x, y)

  expect_identical(fit$engine, FUZZY_COCO_HYBRID_ENGINE)
  expect_equal(fit$infos$iterations, model$params$global_params$max_generations)
  ref_fit <- fuzzycoco_fit_df_rcpp(model, cbind(x, y), responses = "qsec")
  ref_fit$engine <- fit$engine <- fit$infos<- NULL
  expect_identical(fit, ref_fit)

  ####################### regression two variables ########################
  pms$fitness_params$output_vars_defuzz_thresholds <- list(20, 10)
  model <- fuzzycoco("regression", pms, seed = 123)
  df <- CASE$data
  model$params$global_params$max_generations <- 10
  responses <- c("qsec", "hp")
  x <- df[setdiff(names(df), responses)]; y <- df[responses]

  fit <- fuzzycoco_fit_df_hybrid(model, x, y)

  ref_fit <- fuzzycoco_fit_df_rcpp(model, df, responses = responses)
  expect_equal(fit$infos$iterations, model$params$global_params$max_generations)
  ref_fit$engine <- fit$engine  <- fit$infos <- NULL
  expect_identical(fit, ref_fit)

  ####################### classification: one  variable ########################
  pms$output_vars_params$nb_sets <- 2
  pms$output_vars_params$nb_bits_sets <- 1
  model <- fuzzycoco("classification", pms, seed = 123)
  responses <- c("qsec")
  x <- df[setdiff(names(df), responses)]; y <- df[responses]
  y2 <- bin_continuous_responses_to_01(y)

  expect_error(fuzzycoco_fit_df_hybrid(model, x, y2), "output_vars_defuzz_thresholds")
  pms$fitness_params$output_vars_defuzz_thresholds <- 0.5
  model <- fuzzycoco("classification", pms, seed = 123)

  fit <- fuzzycoco_fit_df_hybrid(model, x, y2)

  expect_equal(fit$infos$iterations, model$params$global_params$max_generations)
  ref_fit <- fuzzycoco_fit_df_rcpp(model, cbind(x, y2), responses = responses)
  ref_fit$engine <- fit$engine <- fit$infos <- NULL
  expect_identical(fit, ref_fit)
})



.stop_engine_on_first_of <- 
test_that("stop_engine_on_first_of", {
  CASE <- MTCARS_MULTI_OUTPUT()
  engine <- new_hybrid_engine(CASE$data, 2, CASE$params, 123)

  ### no args- -> error
  expect_error(stop_engine_on_first_of(), "you must give at least one arg")

  ### generations
  until <- stop_engine_on_first_of(max_generations = 3)

  engine <- new_hybrid_engine(CASE$data, 2, CASE$params, 123)
  expect_false(until(engine))
  start_engine(engine)
  expect_false(until(engine))
  for (i in 1:2) {
    compute_next_generation(engine)
    expect_false(until(engine))
  }
  compute_next_generation(engine)
  expect_true(until(engine))

  ### fitness
  until <- stop_engine_on_first_of(max_fitness = 0.4)
  engine <- new_hybrid_engine(CASE$data, 2, CASE$params, 123)
  expect_false(until(engine))
  start_engine(engine)
  expect_false(until(engine))
  while(!until(engine)) {
    compute_next_generation(engine)
  }
  expect_gt(get_current_generation_fitness(engine), 0.4)

  ### other_func
  other <- function(engine) {
    describe_best_system(engine)$fit$metrics$accuracy >= 0.6
  }
  engine <- new_hybrid_engine(CASE$data, 2, CASE$params, 123)
  start_engine(engine)
  expect_false(until(engine))
  while(!until(engine)) {
    compute_next_generation(engine)
  }
  expect_true(describe_best_system(engine)$fit$metrics$accuracy >= 0.6)

  ### mix
  until <- stop_engine_on_first_of(max_generations = 100, max_fitness = 1, other_func = other)
  engine <- new_hybrid_engine(CASE$data, 2, CASE$params, 123)
  start_engine(engine)
  while(!until(engine)) {
    compute_next_generation(engine)
  }
  expect_false(get_current_generation_nb(engine) >= 100)
  expect_false(get_current_generation_fitness(engine) >= 1)
  expect_true(describe_best_system(engine)$fit$metrics$accuracy >= 0.6)

})




.hybrid_engine_wrappers <- 
test_that("hybrid_engine_wrappers", {
  CASE <- MTCARS_MULTI_OUTPUT()

  ## new_hybrid_engine
  engine <- new_hybrid_engine(CASE$data, 2, CASE$params, 123)
  expect_s4_class(engine, "Rcpp_FuzzyCocoWrapper")
  
  ##
  expect_error(describe_best_system(engine), "not started")
  expect_error(describe_current_generation(engine), "not started")
  expect_error(compute_next_generation(engine), "not started")
  expect_equal(get_current_generation_nb(engine), 0)

  start_engine(engine)
  expect_error(start_engine(engine), "already started")

  expect_equal(get_current_generation_nb(engine), 0)

  desc <- describe_best_system(engine)
  expect_identical(names(desc), c("fit", "fuzzy_system", "params"))

  fitness <- compute_next_generation(engine)
  expect_true(length(fitness) == 1 && is.numeric(fitness) && fitness > 0)

  expect_equal(get_current_generation_fitness(engine), fitness)

  expect_equal(get_current_generation_nb(engine), 1)

  ## gen 2
  fitness2 <- compute_next_generation(engine)
  expect_gt(fitness2, fitness)

  expect_equal(get_current_generation_fitness(engine), fitness2)

  expect_equal(get_current_generation_nb(engine), 2)

  ## describe_current_generation
  gen <- describe_current_generation(engine)

  expect_equal(gen$fitness, fitness2)
  expect_equal(gen$generation_number, 2)
  expect_identical(names(gen$rules_population), c("individuals", "elite"))
  expect_identical(names(gen$mfs_population), c("individuals", "elite"))
})
