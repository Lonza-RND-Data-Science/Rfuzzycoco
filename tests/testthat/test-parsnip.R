
.unregister_fuzzy_coco_parsnip <- 
test_that("unregister_fuzzy_coco_parsnip", {
  unregister_fuzzy_coco_parsnip()
  expect_false(is_fuzzy_coco_parsnip_registered())
 
  register_fuzzy_coco_parsnip()
  expect_true(is_fuzzy_coco_parsnip_registered())

  unregister_fuzzy_coco_parsnip()
  expect_false(is_fuzzy_coco_parsnip_registered())
})


.parsnip_classification_fit <- 
test_that("parsnip_classification_fit", {

  df <- utils::type.convert(utils::read.csv2(IRIS36_CSV_PATH), as.is = TRUE)
  df[[1]] <- NULL
  df$OUT <- as.factor(df$OUT)
  levels(df$OUT) <- c("LOW", "HIGH")

  pms <- params(nb_rules = 1, nb_max_var_per_rule = 2, rules.pop_size = 20, mfs.pop_size = 20, 
    ivars.nb_sets = 2, ivars.nb_bits_vars = 3,  ivars.nb_bits_sets = 2, ivars.nb_bits_pos = 8, 
    ovars.nb_sets = 2, ovars.nb_bits_vars = 1, ovars.nb_bits_sets = 1, ovars.nb_bits_pos = 1, 
    metricsw.sensitivity = 1, metricsw.specificity = 1,
    output_vars_defuzz_thresholds = 0.5)

  spec <- fuzzy_coco_parsnip("classification",  params = pms, seed = 123)
  # try again to test double-registration
  expect_no_error(fuzzy_coco_parsnip("classification", params = pms))

  ### fit
  fit <- parsnip::fit(spec, OUT ~ ., data = df)

  expect_s3_class(fit,  "model_fit")  
  model <- fuzzycoco("classification",  params = pms, seed = 123)
  ref_fit <- fit(model, OUT ~ ., df)
  expect_identical(fit$fit, ref_fit)

  fit2 <- parsnip::fit(spec, OUT ~ ., data = df)
  expect_identical(fit2$fit, fit$fit)

  # rcpp engine
  fit2 <- spec |> parsnip::set_engine("rcpp") |>  parsnip::fit(OUT ~ ., data = df)
  expect_identical(fit2$fit$engine, "rcpp")
  expect_null(fit2$fit$infos)
  fit2$fit$engine <- fit$fit$engine <- NULL
  fit2$fit$infos <- fit$fit$infos <- NULL
  expect_identical(fit2$fit, fit$fit)

  ### pred - probs
  pred_probs <- predict(fit, df, type = "prob")

  expect_s3_class(pred_probs, "tbl_df")
  expect_identical(names(pred_probs), c(".pred_LOW", ".pred_HIGH"))
  ref_pred_probs <- predict(ref_fit, df, bin = FALSE)
  expect_equal(pred_probs[[1]], 1 - ref_pred_probs[[1]])
  expect_equal(pred_probs[[2]], ref_pred_probs[[1]])

  ### pred - classes
  pred_classes <- predict(fit, df)

  expect_identical(names(pred_classes), c(".pred_class"))
  expect_true(is.factor(pred_classes[[1]]))
  expect_identical(levels(pred_classes[[1]]), c("LOW", "HIGH"))
  ref_pred_classes <- predict(ref_fit, df)
  expect_equal(as.character(pred_classes[[1]]), c("LOW", "HIGH")[ref_pred_classes[[1]] + 1])
})


.parsnip_regression_fit <- 
test_that("parsnip_regression_fit", {
  CASE <- MTCARS_MULTI_OUTPUT()
  df <- CASE$data
  pms <- CASE$params

  ### regression 1 var

  pms$fitness_params$output_vars_defuzz_thresholds <- median(df$qsec)

  spec <- fuzzy_coco_parsnip("regression",  params = pms, seed = 123)
  # try again to test double-registration
  expect_no_error(fuzzy_coco_parsnip("regression", params = pms))

  ### fit
  fit <- parsnip::fit(spec, qsec ~ ., data = df)

  expect_s3_class(fit,  "model_fit") 
  model <- fuzzycoco("regression",  params = pms, seed = 123)
  ref_fit <- fit(model, qsec ~ ., df)
  expect_identical(fit$fit, ref_fit)

  ## passing args to the fit function
  spec_until <- spec |> parsnip::set_engine("hybrid", until = stop_engine_if_stalling(5))
  fit2 <- parsnip::fit(spec_until, qsec ~ ., data = df)
  expect_lt(fit2$fit$fit$generations, fit$fit$fit$generations)

  spec2 <- spec |> parsnip::set_engine("hybrid", max_generations = 2)
  fit2 <- parsnip::fit(spec2, qsec ~ ., data = df)
  expect_equal(fit2$fit$fit$generations, 2)

  spec2 <- spec |> parsnip::set_engine("hybrid", max_fitness = 0)
  fit2 <- parsnip::fit(spec2, qsec ~ ., data = df)
  expect_equal(fit2$fit$fit$generations, 0)

  ## rcpp engine
  fit2 <- spec |> parsnip::set_engine("rcpp") |>  parsnip::fit(qsec ~ ., data = df)
  expect_identical(fit2$fit$engine, "rcpp")
  expect_null(fit2$fit$infos)
  fit2$fit$engine <- fit$fit$engine <- NULL
  fit2$fit$infos <- fit$fit$infos <- NULL
  expect_identical(fit2$fit, fit$fit)

  ### pred 
  pred <- predict(fit, df)

  expect_identical(names(pred), c(".pred_qsec"))
  expect_true(is.numeric(pred[[1]]))
  ref_pred <- predict(ref_fit, df)
  expect_identical(pred[[1]], ref_pred[[1]])


  ################  regression 2 vars #################
  pms$fitness_params$output_vars_defuzz_thresholds <- lapply(df[c("qsec", "wt")], median)
  spec <- fuzzy_coco_parsnip("regression",  params = pms, seed = 123)

  fit <- parsnip::fit(spec, cbind(qsec, wt) ~ ., data = df)

  model <- fuzzycoco("regression",  params = pms, seed = 123)
  ref_fit <- fit(model, cbind(qsec, wt) ~ ., df)
  expect_identical(fit$fit, ref_fit)

  ### pred 
  pred <- predict(fit, df)

  expect_identical(names(pred), c(".pred_qsec", ".pred_wt"))
  ref_pred <- predict(ref_fit, df)
  expect_identical(pred[[1]], ref_pred[[1]])
  expect_identical(pred[[2]], ref_pred[[2]])
})