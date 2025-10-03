#' predict the outcome on some input data using a fitted model
#' 
#' N.B: just a S3 method method wrapping the [predict_fuzzy_system()] function
#' @param object the fuzzycoco_fit object containing the fuzzy system to predict on
#' @param bin   whether to transform the output data into a binary response. Only applies to classification models.
#' @param ...   not used. Only for S3 generic consistency
#' @inherit predict_fuzzy_system
#' @export
#' @examples
#' model <- fuzzycoco("regression", example_mtcars()$params, seed = 123)
#' x <- mtcars[c("mpg", "hp", "wt")]
#' y <- mtcars["qsec"]
#' fit <- fit_xy(model, x, y, progress = FALSE)
#' 
#' y2 <- predict(fit, x)
predict.fuzzycoco_fit <- function(object, x, verbose = FALSE, bin = TRUE, ...) 
{
  fit <- object
  y <- predict_fuzzy_system(fit$fuzzy_system, x, verbose = verbose)
  if (fit$mode == CLASSIFICATION) {
    if (bin) y <- bin_continuous_responses_to_01(y, fit$params$fitness_params$output_vars_defuzz_thresholds)
  }
  
  y
}

#' predict the outcome of a fuzzy system on some input data
#' 
#' @param fs        the fuzzy system to predict on (as a named list)
#' @param x         the input data to use with the fuzzy system to predict the output
#' @inheritParams shared_params
#' 
#' @return the predicted output data as a data frame
#' @export
#' @examples
#' model <- fuzzycoco("regression", example_mtcars()$params, seed = 123)
#' x <- mtcars[c("mpg", "hp", "wt")]
#' y <- mtcars["qsec"]
#' fit <- fit_xy(model, x, y, progress = FALSE)
#' 
#' y2 <- predict_fuzzy_system(fit$fuzzy_system,x)
predict_fuzzy_system <- function(fs, x, verbose = FALSE) {
  rcpp_fuzzy_coco_predict(x, fs, verbose = verbose)
}
