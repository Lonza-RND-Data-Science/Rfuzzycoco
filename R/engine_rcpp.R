# fit a regression model using the data.frame interface using the rcpp engine
fuzzycoco_fit_df_rcpp <- function(model, data, responses = tail(names(data), 1), verbose = model$verbose) {
  stop_unless(is.data.frame(data), "bad arg 'data': not a data.frame") 
  # check all column types: currently only numeric
  bad_cols <- which(!sapply(data, is.numeric))
  stop_if(length(bad_cols), "bad column(s) '%s', only numeric columns are allowed", names(bad_cols))

  stop_unless(length(responses) > 0, "bad arg 'responses': empty")
  stop_unless(all(responses %in% names(data)), "bad arg 'responses': must be data col names")

  regressors <- setdiff(names(data), responses)
  stop_unless(length(regressors) > 0, "error, no regressors left")

  ordered_vars <- c(regressors, responses)
  df <- data[ordered_vars]

  y <- data[responses]
  params <- resolve_params(model$params, y, model$mode == REGRESSION)

  nb_out_vars <- length(responses)
  check_params(params, nb_out_vars)
  res <- rcpp_fuzzy_coco_searchBestFuzzySystem(df, nb_out_vars, params, model$seed, verbose)
  
  new_fuzzycoco_fit(res, mode = model$mode, engine = FUZZY_COCO_RCPP_ENGINE, seed = model$seed)
}
