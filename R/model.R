CLASSIFICATION <- "classification"
REGRESSION <- "regression"

#'creates a model for the Fuzzy Coco algorithm
#' 
#' @inheritParams shared_params
#' 
#' @return a *fuzzycoco_model*  object (named list)
#' @export
#' @examples
#' model <- fuzzycoco("regression", params(nb_rules = 1, nb_max_var_per_rule = 3), seed = 123, verbose = TRUE)
fuzzycoco <- function(mode = c("classification", "regression"), params, seed = sample.int(10^5, 1), verbose = FALSE) {
  mode <- match.arg(mode)

  if (mode == CLASSIFICATION) {
    with(params$output_vars_params, 
      stop_unless(!is.na(nb_sets) && nb_sets == 2 && !is.na(nb_bits_sets) && nb_bits_sets == 1, 
        "bad output_vars_params for classification, should be: nb_sets=2  nb_bits_sets=1")
    )
  }

  model <- list(mode = mode, params = params, seed = seed, verbose = verbose)
  class(model) <- "fuzzycoco_model"
  model
}

