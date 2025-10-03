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
#' model <- fuzzycoco("regression", example_mtcars()$params, seed = 123)
#' df <- mtcars[c("mpg", "hp", "wt", "qsec")]
#' fit <- fit(model, qsec ~ ., df, seed = 456, max_generations = 10, progress = FALSE)
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
