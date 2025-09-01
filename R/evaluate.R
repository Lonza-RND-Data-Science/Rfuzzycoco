#' @export
evaluate.fuzzycoco_fit <- function(x, data, verbose = FALSE, ...) 
{
  evaluate_fuzzy_system(x$fuzzy_system, data, x$params, verbose = verbose)
}

#' @export
evaluate_fuzzy_system <- function(fs, x, params, verbose = FALSE) 
{
  nb_vars <- length(fs$variables$input) + length(fs$variables$output)
  stop_unless(ncol(x) == nb_vars, 
    "bad number of columns in data: %i != %i", ncol(x), nb_vars)

  # reorder the data
  all_vars <- c(names(fs$variables$input), names(fs$variables$output))
  stop_unless(all(all_vars %in% names(x)), "x must contain all fuzzy system variables")
  df <- x[all_vars]

  rcpp_fuzzy_coco_eval(df, fs, params, verbose = verbose)
}
