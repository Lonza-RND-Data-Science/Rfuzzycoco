### parsnip integration of fuzzy_coco_parsnip
# Note that this is optional: if not used, the tidymodels ecosystem do not need to be installed
# since the related packages are in "Suggests:"

FUZZY_COCO_MODEL <- "fuzzy_coco"

#' parsnip model function
#' 
#' @inheritParams shared_params
#' @export
fuzzy_coco_parsnip <- function(mode = "unknown", params, 
  engine = FUZZY_COCO_HYBRID_ENGINE, seed = sample.int(10^5, 1), verbose = FALSE) 
{
  register_fuzzy_coco_parsnip()
  internal_model <- fuzzycoco(mode = mode, params = params, seed = seed, verbose = verbose)

  # Capture the arguments in quosures
  args <- list(internal_model = internal_model)
  
  # Save some empty slots for future parts of the specification
  parsnip::new_model_spec(
    FUZZY_COCO_MODEL,
    args = args,
    eng_args = NULL,
    mode = mode,
    method = NULL,
    engine = engine
  )
}

#' this is an utility function used to implement the parsnip interface
#' 
#' It should not be exported, it only is because of parsnip internal implementation
#' @param object,internal_model,fit,df,pred,type,... no comment
#' @inheritParams shared_params
#' @export
fuzzy_coco_parsnip_wrapper <- function(formula, data, object = NULL, internal_model = NULL, engine = NULL, 
  fit = NULL, df = NULL, pred = FALSE, type = NULL, ...) {

  if (!pred) {
    return(fit(internal_model, formula, data, engine = engine, ...))
  }

  if (type == "numeric") {
    return(predict(fit, df[names(fit$fuzzy_system$variables$input)]))
  }

  # pred classification
  y <- predict(fit, df[names(fit$fuzzy_system$variables$input)], bin = (type != "prob"))
  lvl <- object$lvl %||% c("0", "1")
  
  if (type != "prob") return(lvl[y[[1]] + 1])

  probs <- data.frame(1 - y[[1]], y[[1]])
  names(probs) <- paste0(".pred_", lvl)

  probs
}


# N.B: this is very fragile and dependent on parsnip internals
is_fuzzy_coco_parsnip_registered <- function() {
  length(get0(FUZZY_COCO_MODEL, envir = parsnip::get_model_env(), inherits = FALSE)) > 0
}

# N.B: this is very fragile and dependent on parsnip internals
unregister_fuzzy_coco_parsnip <- function() {
  env <- parsnip::get_model_env()
  pattern <- paste0("^", FUZZY_COCO_MODEL)
  keys <- grep(pattern, ls(env), value = TRUE)
  rm(list = keys, envir = env)
  env$models <- setdiff(env$models, FUZZY_COCO_MODEL)
}

register_parsnip_models <- function() {
  parsnip::set_new_model(FUZZY_COCO_MODEL)
  parsnip::set_model_mode(FUZZY_COCO_MODEL, "classification")
  parsnip::set_model_mode(FUZZY_COCO_MODEL, "regression")

  for (engine in FUZZY_COCO_ENGINES) {
    parsnip::set_model_engine(FUZZY_COCO_MODEL, mode = "classification", eng = engine)
    parsnip::set_model_engine(FUZZY_COCO_MODEL, mode = "regression", eng = engine)
  }
}

register_parsnip_fits <- function() {
  for (engine in FUZZY_COCO_ENGINES) {
    parsnip::set_fit(
      model = FUZZY_COCO_MODEL,
      eng = engine,
      mode = "classification",
      value = list(
        interface = "formula",
        protect = c("formula", "data"), 
        func = c(pkg = "Rfuzzycoco", fun = "fuzzy_coco_parsnip_wrapper"),
        defaults = list(engine = engine)
      )
    )

    parsnip::set_fit(
      model = FUZZY_COCO_MODEL,
      eng = engine,
      mode = "regression",
      value = list(
        interface = "formula",
        protect = c("formula", "data"), 
        func = c(pkg = "Rfuzzycoco", fun = "fuzzy_coco_parsnip_wrapper"),
        defaults = list(engine = engine)
      )
    )

    parsnip::set_model_arg(FUZZY_COCO_MODEL, engine,
      parsnip = "internal_model", original = "internal_model", 
      func = c(fun = "identity"), has_submodel = FALSE)

    parsnip::set_encoding(
      model = FUZZY_COCO_MODEL,
      eng = engine,
      mode = "classification",
      options = list(
        predictor_indicators = "one_hot", 
        compute_intercept = FALSE,
        remove_intercept = TRUE,
        allow_sparse_x = FALSE
      )
    )

    parsnip::set_encoding(
      model = FUZZY_COCO_MODEL,
      eng = engine,
      mode = "regression",
      options = list(
        predictor_indicators = "one_hot", 
        compute_intercept = FALSE,
        remove_intercept = TRUE,
        allow_sparse_x = FALSE
      )
    )
  }
}


register_parsnip_preds <- function() {
  # fix R CMD check warning: no visible binding for global variable xxx
  new_data <- object <- NULL

  for (engine in FUZZY_COCO_ENGINES) {
    parsnip::set_pred(
      model = FUZZY_COCO_MODEL,
      eng = engine,
      mode = "classification",
      type = "prob",
      value = list(
        pre = NULL,
        post = NULL,
        func = c(pkg = "Rfuzzycoco", fun = "fuzzy_coco_parsnip_wrapper"),
        args =
          list(
            fit = rlang::expr(object$fit),
            df = rlang::expr(new_data),
            object = rlang::expr(object),
            pred = TRUE,
            type = "prob"
          )
      )
    )

    parsnip::set_pred(
      model = FUZZY_COCO_MODEL,
      eng = engine,
      mode = "classification",
      type = "class",
      value = list(
        pre = NULL,
        post = NULL,
        func = c(pkg = "Rfuzzycoco", fun = "fuzzy_coco_parsnip_wrapper"),
        args =
          list(
            fit = rlang::expr(object$fit),
            df = rlang::expr(new_data),
            object = rlang::expr(object),
            pred = TRUE,
            type = "class"
          )
      )
    )

  }

    parsnip::set_pred(
      model = FUZZY_COCO_MODEL,
      eng = engine,
      mode = "regression",
      type = "numeric",
      value = list(
        pre = NULL,
        post = NULL,
        func = c(pkg = "Rfuzzycoco", fun = "fuzzy_coco_parsnip_wrapper"),
        args =
          list(
            fit = rlang::expr(object$fit),
            df = rlang::expr(new_data),
            object = rlang::expr(object),
            pred = TRUE,
            type = "numeric"
          )
      )
    )
}

register_fuzzy_coco_parsnip <- function() {
  if (is_fuzzy_coco_parsnip_registered()) return()

  register_parsnip_models()
  register_parsnip_fits()
  register_parsnip_preds()
}