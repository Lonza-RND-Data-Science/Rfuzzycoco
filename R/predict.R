#' @export
predict.fuzzycoco_fit <- function(object, x, verbose = FALSE, bin = TRUE, ...) 
{
  fit <- object
  y <- predict_fuzzy_system(fit$fuzzy_system, x, verbose = verbose)
  if (fit$mode == CLASSIFICATION) {
    if (bin) y <- bin_continuous_responses_to_01(y, fit$params$fitness_params$output_vars_defuzz_thresholds)
  }
  
  y
}

predict_fuzzy_system <- function(fuzzy_system, x, verbose = FALSE) {
  rcpp_fuzzy_coco_predict(x, fuzzy_system, verbose = verbose)
}
