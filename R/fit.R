### we try to implement an interface close to tidymodels/parsnip but independent 
### from those packages, in order to make the optional parsnip integration easier to implement


# N.B: fix_xy() is the workhorse, fit() is a simple formula-based layer
#' @export
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
