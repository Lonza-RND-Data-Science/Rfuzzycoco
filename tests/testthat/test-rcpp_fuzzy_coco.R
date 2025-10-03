.rcpp_fuzzy_coco_searchBestFuzzySystem <- 
test_that("rcpp_fuzzy_coco_searchBestFuzzySystem", {
  df <- rbind(head(iris[iris$Species == "setosa", ], 5), head(iris[iris$Species != "setosa", ], 5))

  df$Species <- as.integer(as.logical(df$Species== 'setosa'))

  lst <- params(3L, 2L, rules.pop_size = 10L, mfs.pop_size = 20L, ivars.nb_bits_pos = 8L, ovars.nb_bits_pos = 3L)
  lst$fitness_params$output_vars_defuzz_thresholds <- 0.5

  res <- rcpp_fuzzy_coco_searchBestFuzzySystem(df, 1, lst)

  expect_true(is.list(res))
  expect_identical(names(res), c("fit", "fuzzy_system", "params"))
  expect_equal(res$fit$fitness, 1)
  expect_equal(res$fit$generations, 1)

  # need to defuzzify
  pred <- rcpp_fuzzy_coco_predict(df, res$fuzzy_system)
  pred <- ifelse(pred[[1]] > 0.5, 1, 0)
  expect_equal(pred, df$Species)
})


.iris36 <- 
test_that("iris36", {
  CASE <- example_iris36()
  df <- CASE$data

  params <- resolve_params(CASE$params, df["OUT"], FALSE)

  res <- rcpp_fuzzy_coco_searchBestFuzzySystem(df, 1, params, seed = 123)

  expect_true(is.list(res))
  expect_equal(res$fit$fitness, 1)
  expect_equal(res$fit$generations, 3)

  expected_rules <- list(
    rule1 = list(
      antecedents = list(Petal.Width = list(Petal.Width.1 = 0.817888563049853)), 
      consequents = list(OUT = list(OUT.1 = 0))
    )
  )
  

  expect_equal(res$fuzzy_system$rules, expected_rules)

  ### predict
  dfout <- rcpp_fuzzy_coco_predict(df, res$fuzzy_system, verbose = FALSE)

  expect_true(is.data.frame(dfout))
  expect_equal(dfout[[1]], df$OUT)

  ### eval
  ## eval with same dataset
  res2 <- rcpp_fuzzy_coco_eval(df, res$fuzzy_system, params, verbose = FALSE)
  # n.B: all positives are true
  expect_equal(res2$metrics$true_positives, 18)
  expect_equal(res2$metrics$true_negatives, 18)

  expected <- res$fit
  expected$generations <- NULL
  expect_equal(res2, expected)

  ## eval with other dataset
  df2 <- df
  df2$OUT <- 1

  res2 <- rcpp_fuzzy_coco_eval(df2, res$fuzzy_system, params, verbose = FALSE)
  
  expect_lt(res2$fitness, 0.35)
  ## N.B: all predicted negatives are actually positives according to df2 --> false negatives
  expect_equal(res2$metrics$true_positives, 18)
  expect_equal(res2$metrics$false_negatives, 18)
  
  ### invert
  df2 <- df
  df2$OUT <- 1 - df$OUT

  res2 <- rcpp_fuzzy_coco_eval(df2, res$fuzzy_system, params, verbose = FALSE)
  
  expect_lt(res2$fitness, 0.1)
  ## N.B: all predicted negatives are actually positives according to df2 --> false negatives
  expect_equal(res2$metrics$false_positives, 18)
  expect_equal(res2$metrics$false_negatives, 18)

})



.multi_ouput <- 
test_that("multi_ouput", {
  CASE <- example_mtcars()
  df <- CASE$data

  params <- resolve_params(CASE$params, df[c("wt", "qsec")], TRUE)

  res <- rcpp_fuzzy_coco_searchBestFuzzySystem(df, 2, params, seed = 123)

  rules <- res$fuzzy_system$rules
  expect_setequal(names(rules[[1]]$consequents), "qsec")
  expect_setequal(names(rules[[2]]$consequents), c("wt", "qsec"))
  expect_setequal(names(res$fuzzy_system$default_rules), c("wt", "qsec"))

  dfout <- rcpp_fuzzy_coco_predict(df, res$fuzzy_system, verbose = FALSE)
  expect_equal(nrow(dfout), nrow(df))
  expect_equal(ncol(dfout), 2)
})



.iris36_params2 <- 
test_that("iris36_params2", {
  CASE <- example_iris36()

  pms <- params(nb_rules = 1, nb_max_var_per_rule = 2, rules.pop_size = 20, mfs.pop_size = 20, 
    ivars.nb_sets = 2, ivars.nb_bits_vars = 3,  ivars.nb_bits_sets = 2, ivars.nb_bits_pos = 8, 
    ovars.nb_sets = 2, ovars.nb_bits_vars = 1, ovars.nb_bits_sets = 1, ovars.nb_bits_pos = 1, 
    metricsw.sensitivity = 1, metricsw.specificity = 1,
    output_vars_defuzz_thresholds = 0.5)

  fit <- rcpp_fuzzy_coco_searchBestFuzzySystem(CASE$data, 1, pms, 123)

  pred <- rcpp_fuzzy_coco_predict(CASE$data, fit$fuzzy_system)
  pred <- ifelse(pred[[1]] > 0.5, 1, 0)
  expect_equal(pred, CASE$data$OUT)
})