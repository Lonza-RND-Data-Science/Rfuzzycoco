
.fuzzy_coco_systematic_fit <- 
test_that("fuzzy_coco_systematic_fit", {
  CASE <- example_mtcars()
  df <- CASE$data
  pms <- CASE$params
  pms$fitness_params$output_vars_defuzz_thresholds <- 0
  pms$global_params$nb_rules <- 1
  pms$global_params$nb_max_var_per_rule <- 2
  pms$output_vars_params$nb_sets <- 2

  x <- df[c("mpg", "hp", "wt")]
  y <- df["qsec"]

  fitter <- function(metrics) 2^-metrics$rms

  res <- fuzzy_coco_systematic_fit(x, y, pms, fitter)

  expect_length(res, 1)
  res1 <- res[[1]]
  ref <- evaluate_fuzzy_system(res1$fs, cbind(x, y), pms)
  expect_equal(fitter(ref$metrics), res1$metric)
})

.make_half_rule_grid <- 
test_that("make_half_rule_grid", {
  vars <- paste0("var", 1:3)

  .nb_rules <- function(nb_vars, nb_max, nb_sets) {
    res <- 0
    for (i in seq_len(nb_max))
      res <- res + choose(nb_vars, i)*nb_sets^i
    res
  }
  ### nb_max_var_per_rule=2 
  grid <- make_half_rule_grid(vars, 2, 3)

  expect_equal(ncol(grid), 2)
  expect_equal(nrow(grid), .nb_rules(3, 2, 3))
  expect_equal(anyDuplicated(grid), 0)

  ## nb_var_per_rule=3
  grid <- make_half_rule_grid(vars, 3, 3)
  
  expect_equal(anyDuplicated(grid), 0)
  expect_equal(ncol(grid), 3)
  expect_equal(nrow(grid), .nb_rules(3, 3, 3))
})


.make_sets_grid <- 
test_that("make_sets_grid", {
  vars <- paste0("var", 1:3)

  ### nb_var_per_rule=3 
  grid <- make_sets_grid(vars, 3, c(3, 3, 3))
  expect_equal(ncol(grid), 3)
  expect_equal(nrow(grid), choose(length(vars), 3)*3^3)

  ## nb_var_per_rule=2
  grid <- make_sets_grid(vars, 2, c(3, 3, 3))
  
  expect_equal(ncol(grid), 2)
  expect_equal(nrow(grid), choose(length(vars), 2)*3^2)
  
  ### 
  grid <- make_sets_grid(vars, 3, c(3, 1, 2))

  expect_equal(ncol(grid), 3)
  expect_equal(nrow(grid), choose(length(vars), 3)*3*1*2)
})


.grid_row_to_rules <- 
test_that("grid_row_to_rules", {
  row <- c("in1.1", "in2.2", "out1.2", "out2.2", "out1.1", "out2.1")

  rules <- grid_row_to_rules(row, 2, 2)
  
  exp_rules <- list(rules = list(
      list(
        antecedents = list(
          in1 = list(in1.1 = 0), 
          in2 = list(in2.2 = 0)), 
        consequents = list(
          out1 = list(out1.2 = 0), 
          out2 = list(out2.2 = 0))
        )
      ), 
      default_rules = list(
        out1 = "out1.1", 
        out2 = "out2.1"
      )
    )

  expect_identical(rules, exp_rules)

  ##########
  row <- c("in1.1", "in2.2", "out1.2", "out2.2", "in1.1", "in2.2", "out1.2", "out2.2", "out1.1", "out2.1")

  rules <- grid_row_to_rules(row, 2, 2)
  exp_rules2 <- exp_rules
  exp_rules2$rules[[2]] <- exp_rules2$rules[[1]]
  expect_identical(rules, exp_rules2)

  ### with NAs
  row <- c("in1.1", NA, "out1.2", "out2.2", "out1.1", "out2.1")

  rules <- grid_row_to_rules(row, 2, 2)

  exp_rules2 <- exp_rules
  exp_rules2$rules[[1]]$antecedents[[2]] <- NULL
  expect_identical(rules, exp_rules2)
})


.make_rules_grid <- 
test_that("make_rules_grid", {
  grid <- make_rules_grid(1, c("i1", "i2"), c("o1", "o2"), 2, 3, 2)
  
  .nb_rules <- function(nb_vars, nb_max, nb_sets) {
    res <- 0
    for (i in seq_len(nb_max))
      res <- res + choose(nb_vars, i)*nb_sets^i
    res
  }
  expect_equal(ncol(grid), 2 + 2 + 2)
  expect_equal(nrow(grid), .nb_rules(2, 2, 3) * .nb_rules(2, 2, 2) * .nb_rules(2, 2, 2))
  expect_equal(anyDuplicated(grid), 0)

  ### 
  grid <- make_rules_grid(2, c("i1", "i2", "i3"), "o1", 2, 3, 3)

  expect_equal(ncol(grid), 2*(2 + 1) + 1)
  expect_equal(nrow(grid), (.nb_rules(3, 2, 3) * .nb_rules(1, 1, 3))^2 * .nb_rules(1, 1, 3))
  expect_equal(anyDuplicated(grid), 0)
})



.compute_optimal_quantile_fuzzy_set_positions <- 
test_that("compute_optimal_quantile_fuzzy_set_positions", {
  pos <- compute_optimal_quantile_fuzzy_set_positions(mtcars, 3)

  expect_true(is.list(pos))
  expect_identical(names(pos), names(mtcars))
  expect_equal(pos$carb, compute_optimal_quantile_fuzzy_set_positions_vec(mtcars$carb, 3))
})


.compute_optimal_quantile_fuzzy_set_positions_vec <- 
test_that("compute_optimal_quantile_fuzzy_set_positions_vec", {
  set.seed(123)
  x <- runif(100000)

  expect_equal(compute_optimal_quantile_fuzzy_set_positions_vec(x, 2), 1:2 / 3, tolerance = 0.01)
  expect_equal(compute_optimal_quantile_fuzzy_set_positions_vec(x, 3), 1:3 / 4, tolerance = 0.01)

  expect_equal(compute_optimal_quantile_fuzzy_set_positions_vec(0, 2), c(0, 0))
  expect_equal(compute_optimal_quantile_fuzzy_set_positions_vec(rep(0, 1000), 2), c(0, 0))
  expect_equal(compute_optimal_quantile_fuzzy_set_positions_vec(rep(c(0, 1), 1000), 2), c(0, 1))
})


