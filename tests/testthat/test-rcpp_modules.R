.FuzzyCocoWrapper <- 
test_that("FuzzyCocoWrapper", {
  CASE <- IRIS36()

  lst <- params(3L, 2L, rules.pop_size = 10L, mfs.pop_size = 20L, ivars.nb_bits_pos = 8L, ovars.nb_bits_pos = 3L)
  lst$fitness_params$output_vars_defuzz_thresholds <- list(0.5)

  x <- new(FuzzyCocoWrapper, CASE$data, 1, lst, 123, FALSE)

  expect_equal(x$getCurrentFitness(), 0)
  expect_equal(x$getCurrentGenerationNb(), 0)

  x$start()

  # rules <- x$describeCurrentGenerationRules()
  # mfs <- x$describeCurrentGenerationMFs()
  gen_desc <- x$describeCurrentGeneration()

  expect_identical(names(gen_desc),  c("fitness", "generation_number", "rules_population", "mfs_population"))
  expect_equal(gen_desc$fitness,  0)
  expect_equal(gen_desc$generation_number,  0)

  rules_pop = gen_desc$rules_population
  expect_identical(names(rules_pop), c("individuals", "elite"))
  expect_length(rules_pop$individuals, lst$rules_params$pop_size)
  expect_length(rules_pop$elite, lst$rules_params$elite_size)
  expect_identical(names(rules_pop$individuals[[1]]), c("rules", "default_rules"))
  
  mfs_pop <- gen_desc$mfs_population
  expect_identical(names(mfs_pop), c("individuals", "elite"))
  expect_length(mfs_pop$individuals, lst$mfs_params$pop_size)
  expect_length(mfs_pop$elite, lst$mfs_params$elite_size)
  expect_identical(names(mfs_pop$individuals[[1]]), c("input", "output"))

  expect_equal(x$getCurrentFitness(), 0)
  expect_equal(x$getCurrentGenerationNb(), 0)

  # gen <- x$getCurrentGeneration()
  # expect_equal(gen$fitness, 0)
  # expect_equal(gen$generation_number, 0)

  fitness <- NULL
  while((fitness <- x$next_gen()) < 0.95) {} 

  expect_equal(x$getCurrentFitness(), 1)
  expect_lt(x$getCurrentGenerationNb(), 100)
})


.multi_convergence <- 
test_that("multi_convergence", {
  CASE <- IRIS36()
  df <- CASE$data

  lst <- params(3L, 2L, rules.pop_size = 10L, mfs.pop_size = 20L, ivars.nb_bits_pos = 8L, ovars.nb_bits_pos = 3L)
  lst$fitness_params$output_vars_defuzz_thresholds <- 0.5

  params_sets <- lapply(1:3, \(i) { lst$global_params$nb_rules <- i; lst})

  engines <- lapply(params_sets, \(params) new(FuzzyCocoWrapper, df, 1, params, 456, FALSE))

  for (engine in engines) engine$start()

  best_fitness <- 0
  while(best_fitness < 1) {
    fitnesses <- sapply(engines, \(x) x$next_gen())
    best_fitness <- max(fitnesses)
  }

  best_engine <- which.max(fitnesses)
  expect_equal(best_engine, 2)
  expect_equal(engines[[best_engine]]$getCurrentGenerationNb(), 12)

  desc <- engines[[best_engine]]$describeBestSystem()
  expect_true(is.list(desc))
  fs <- desc$fuzzy_system
  rules <- fs$rules
  expect_length(rules, 2)
  expect_identical(names(rules[[1]]$antecedents), "Petal.Length")
  expect_identical(names(rules[[2]]$antecedents), "Sepal.Width")
})