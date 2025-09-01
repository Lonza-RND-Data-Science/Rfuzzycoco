.FuzzySystem <- 
test_that(".FuzzySystem", {

  df <- utils::type.convert(utils::read.csv2(IRIS36_CSV_PATH), as.is = TRUE)
  df[[1]] <- NULL

  lst <- params(3L, 2L, rules.pop_size = 10L, mfs.pop_size = 20L, ivars.nb_bits_pos = 8L, ovars.nb_bits_pos = 3L)
  lst$fitness_params$output_vars_defuzz_thresholds <- 0.5

  # for a binary output
  # TODO: investigate that 
  lst$output_vars_params$nb_sets <- 2
  lst$output_vars_params$nb_bits_vars <- 1
  lst$output_vars_params$nb_bits_sets <- 1
  lst$output_vars_params$nb_bits_pos <- 1

  x <- new(FuzzyCocoWrapper, df, 1, lst, 123, FALSE)
  x$start()
  while((fitness <- x$next_gen()) < 1) {} 

  res <- x$describeBestSystem()
  fs_desc <- res$fuzzy_system
  
  fs <- new(FuzzySystemWrapper, fs_desc)

  dfout <- fs$predict(df)
  # caution: it is not thresholded yet

  outcomes <- ifelse(dfout[[1]] >= lst$fitness_params$output_vars_defuzz_thresholds, 1, 0)
  expect_equal(outcomes, df$OUT)
})

