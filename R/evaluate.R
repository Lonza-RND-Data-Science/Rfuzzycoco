#' evaluate the fuzzy system from a fit on some given data
#' 
#' N.B: just a S3 method method wrapping the [evaluate_fuzzy_system()] function
#' @param x the fuzzycoco_fit object containing the fuzzy system to evaluate
#' @param ...   not used. Only for S3 generic consistency
#' @inherit evaluate_fuzzy_system
#' @export
#' @examples
#' model <- fuzzycoco("regression", example_mtcars()$params, seed = 123)
#' df <- mtcars[c("mpg", "hp", "wt", "qsec")]
#' fit <- fit(model, qsec ~ ., df, engine = "rcpp", seed = 456, max_generations = 20)
#' 
#' res <- evaluate(fit, df)
#' print(res$fitness)
evaluate.fuzzycoco_fit <- function(x, data, verbose = FALSE, ...) 
{
  evaluate_fuzzy_system(x$fuzzy_system, data, x$params, verbose = verbose)
}

# TODO: we probably do NOT need the params here
# TODO: not tested directly
#' evaluate the fuzzy system from a fit on some given data
#' @param fs        the fuzzy system to evaluate (as a named list)
#' @param data      the data to evaluate the fuzzy system on
#' @param params    the fuzzycoco parameters. probably not needed...
#' @inheritParams shared_params
#' 
#' @return the evaluation as a named list
#' @export
#' @examples
#' model <- fuzzycoco("regression", example_mtcars()$params, seed = 123)
#' x <- mtcars[c("mpg", "hp", "wt")]
#' y <- mtcars["qsec"]
#' fit <- fit_xy(model, x, y, progress = FALSE)
#' 
#' res <- evaluate_fuzzy_system(fit$fuzzy_system, cbind(x, y), fit$params)
#' print(res$metrics$rmse)
evaluate_fuzzy_system <- function(fs, data, params, verbose = FALSE) 
{
  nb_vars <- length(fs$variables$input) + length(fs$variables$output)
  stop_unless(ncol(data) == nb_vars, 
    "bad number of columns in data: %i != %i", ncol(data), nb_vars)

  # reorder the data
  all_vars <- c(names(fs$variables$input), names(fs$variables$output))
  stop_unless(all(all_vars %in% names(data)), "data must contain all fuzzy system variables")
  df <- data[all_vars]

  rcpp_fuzzy_coco_eval(df, fs, params, verbose = verbose)
}
