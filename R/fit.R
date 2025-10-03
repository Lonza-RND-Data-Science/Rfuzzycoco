### we try to implement an interface close to tidymodels/parsnip but independent 
### from those packages, in order to make the optional parsnip integration easier to implement



#' fit the FuzzyCoco model using the formula interface
#' 
#' N.B: `fix_xy()` is the workhorse, `fit()` is a simple formula-based layer
#' 
#' @inherit fit_xy.fuzzycoco_model
#' @inheritDotParams fit_xy.fuzzycoco_model 
#' @inheritParams shared_params
#' @return a named list 
#' @export
#' @examples
#' pms <- params(
#'  nb_rules = 2, nb_max_var_per_rule = 3,        # structural parameters
#'  rules.pop_size = 100, mfs.pop_size = 100,     # coevolution population sizes
#'  ivars.nb_sets = 3, , ivars.nb_bits_pos = 8,   # input vars: 3 fuzzy sets, and 8 bits to discretize the values 
#'  ovars.nb_sets = 3, ovars.nb_bits_pos = 8,     # output vars: 3 fuzzy sets, and 8 bits to discretize the values 
#'  metricsw.sensitivity = 0, metricsw.specificity = 0, metricsw.rmse = 1, # we just use RMSE (root mean square error)
#'  output_vars_defuzz_thresholds = 17            # threshold for the qsec output variable
#')
#' model <- fuzzycoco("regression", pms, seed = 123)
#' 
#' fit <- fit(model, qsec ~ ., mtcars[c("mpg", "hp", "wt", "qsec")], engine = "rcpp", seed = 456, max_generations = 20)
#' print(names(fit))
#' 
fit.fuzzycoco_model <- function(object, formula, data, engine = FUZZY_COCO_HYBRID_ENGINE, 
  max_generations = object$params$global_params$max_generations, 
  max_fitness = object$params$global_params$max_fitness, 
  seed = object$seed, verbose = object$verbose, ...) 
{
  model <- object
  dfs <- preprocess_data_with_formula(formula, data)

  x <- dfs[[1]]
  y <- dfs[[2]]

  fit_xy.fuzzycoco_model(model, x,y, 
    engine = engine, 
    max_generations = max_generations, 
    max_fitness = max_fitness,
    seed = seed,
    verbose = verbose, 
    ...)
}


new_fuzzycoco_fit <- function(res, mode = NULL, engine = NULL, seed = NULL, infos = NULL) {
  res$engine <- engine
  res$seed <- seed
  res$mode <- mode
  res$infos <- infos
  class(res) <- "fuzzycoco_fit"
  res
}
