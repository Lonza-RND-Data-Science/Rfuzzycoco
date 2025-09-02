# N.B: classification is strictly for binary reponses (POSITIVE/NEGATIVE)
# variables will be converted to 0/1, and if not set a threshold==0.5

#' fit the FuzzyCoco model using the dataframe interface
#' 
#' @param object  the *fuzzycoco_model* object to fit
#' @inheritDotParams fuzzycoco_fit_df_hybrid
#' @inheritParams shared_params
#' @return a named list
#' @export
fit_xy.fuzzycoco_model <- function(object, x, y, engine = FUZZY_COCO_HYBRID_ENGINE, 
  max_generations = object$params$global_params$max_generations, 
  max_fitness = object$params$global_params$max_fitness, 
  seed = object$seed, verbose = object$verbose, ...) 
{
  model <- object
  is_regression <- model$mode == REGRESSION
  
  if (is_regression) {
    # check that the response variables are numeric
    not_nums <- which(!sapply(y, is.numeric))
    stop_if(length(not_nums), "error, non numerical response(s): %s", names(not_nums))
  } else {
    # check output var params for classification: --> only 2 output fuzzy sets

    y <- transform_binary_responses_to_01(y)
    # now set the thresholds for binary 01 vars
    thresholds <- as.list(rep(0.5, length(y)))
    model$params$fitness_params$output_vars_defuzz_thresholds <- thresholds
  }

  model$params$global_params$max_generations <- max_generations
  model$params$global_params$max_fitness <- max_fitness
  model$seed <- seed

  df <- cbind(x, y)
  responses <- names(y)

  fit <- switch(engine, 
    rcpp = fuzzycoco_fit_df_rcpp(model, df, responses = responses, verbose = verbose),
    hybrid = fuzzycoco_fit_df_hybrid(model, x, y, verbose = verbose, ...),
    stop_if(TRUE, "bad engine %s", engine)
  )

  fit
}




