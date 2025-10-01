


# just a wrapper on FuzzyCocoWrapper
new_hybrid_engine <- function(data, nb_out_vars, params, seed, verbose = FALSE) {
  new(FuzzyCocoWrapper, data, nb_out_vars, params, seed, verbose)
}

start_engine <- function(engine) { engine$start() }

# computes the next fuzzycoco generation from the current one
# @return the objective value for this new generation
# return the objective value for the 
compute_next_generation <- function(engine) {
  engine$next_gen() 
}

# describes the best fuzzy system evaluated so far
# @return the best fuzzy system as a named list
describe_best_system <- function(engine) {
  engine$describeBestSystem() 
}

# describes the current generation
# @return the current generation as a named list
describe_current_generation <- function(engine) {
  engine$describeCurrentGeneration() 
}

# get the current generation number
# @return the number as an integer
get_current_generation_nb <- function(engine) {
  engine$getCurrentGenerationNb()
}

# get the current generation fitness
# @return the fitness as a numeric
get_current_generation_fitness <- function(engine) {
  engine$getCurrentFitness()
}

#' lowest-level implementation of the fitting of a fuzzy coco model using the **hybrid engine**
#' 
#' @return a named list as a `fuzzy_coco` fit object
#' @inheritParams shared_params
#' @export
#' @examples
#'  x <- mtcars[c("mpg", "hp", "wt")]
#'  y <- mtcars["qsec"]
#'  pms <- params(
#'    nb_rules = 2, nb_max_var_per_rule = 3, rules.pop_size = 20, mfs.pop_size = 20, 
#'    ivars.nb_sets = 3, ivars.nb_bits_vars = 3,  ivars.nb_bits_sets = 2, ivars.nb_bits_pos = 8, 
#'    ovars.nb_sets = 3, ovars.nb_bits_vars = 1, ovars.nb_bits_sets = 2, ovars.nb_bits_pos = 8, 
#'    metricsw.sensitivity = 0, metricsw.specificity = 0, metricsw.rmse = 1,
#'    output_vars_defuzz_thresholds = 17
#'  )
#'  model <- fuzzycoco("regression", pms)
#' 
#'  fit <- fuzzycoco_fit_df_hybrid(model, x, y)
fuzzycoco_fit_df_hybrid <- function(model, x, y, 
  until = stop_engine_on_first_of(
    max_generations = model$params$global_params$max_generations, 
    max_fitness = model$params$global_params$max_fitness
  ), verbose = model$verbose, progress = TRUE) 
{
  stop_unless(length(model$params$fitness_params$output_vars_defuzz_thresholds) == ncol(y),
    "bad params$fitness_params$output_vars_defuzz_thresholds, must be defined for all output vars")

  progressr <- progress && requireNamespace("progressr")
  max_gen <- until() %||% model$params$global_params$max_generations
  p <- NULL
  if (progressr) p <- progressr::progressor(max_gen)

  data <- cbind(x, y)

  engine <- new_hybrid_engine(data, ncol(y), model$params, model$seed, verbose = verbose)
  start_engine(engine)
  # if (progress) pb$tick(0)
  last_fitness <- last_percent <- it <- 0

  while(!until(engine)) {
    compute_next_generation(engine)
    it <- it + 1
    percent <- it * 100 / max_gen
    if (progressr &&  percent > last_percent) {
      fitness <- get_current_generation_fitness(engine)
      if (fitness > last_fitness) last_fitness <- fitness
      last_percent <- percent
      p(message = sprintf("fitness = %f at iteration %i", fitness, it))
    }
  }

  res <- engine$describeBestSystem()
  
  infos <- list(iterations = it)
  new_fuzzycoco_fit(res, mode = model$mode, engine = FUZZY_COCO_HYBRID_ENGINE, seed = model$seed, infos = infos)
}

#' an utility function to easily generate the commonly used `until` parameter, as used by [fuzzycoco_fit_df_hybrid()]
#' 
#' @param other_func    if not NULL, a function engine -->logical that should return TRUE to stop the evolution 
#'  (cf [stop_engine_if_stalling()]) 
#' @inheritParams shared_params
#' @return a function engine --> logical that stops (i.e/ returns TRUE) when the number of generations or the fitness
#'  are reached, or when the `other_func` if provided returns TRUE
#' @export
#' @examples
#' until <- stop_engine_on_first_of(max_generations = 100)
#' until <- stop_engine_on_first_of(max_generations = 100, max_fitness = 0.8)
#' until <- stop_engine_on_first_of(max_fitness = 0.9, other_func = stop_engine_if_stalling(5))
stop_engine_on_first_of <- function(max_generations = NULL, max_fitness = NULL, other_func = NULL) {
  stop_if(!length(max_generations) && !length(max_fitness) && !length(other_func), 
     "you must give at least one arg")
  stop_if(length(max_fitness) && !is.numeric(max_fitness), "max_fitness must be numeric")
  stop_if(length(other_func) && !is.function(other_func), "other_func is not a function")

  function(engine) {
    if (missing(engine)) return(max_generations)
    (length(max_generations) && get_current_generation_nb(engine) >= max_generations) ||
    (length(max_fitness) && get_current_generation_fitness(engine) >= max_fitness) ||
    (length(other_func) && other_func(engine))
  }
}

#' an utility function to easily generate a stop function that stops when the convergence is stalling
#' 
#' @param nb_iterations    number of iterations of the stalling: stops if the fitness has not increased during that
#'  number of iterations.
#' @inheritParams shared_params
#' @return a function engine --> logical that stops (i.e/ returns TRUE) if the convergence is stalling
#' @export
#' @examples
#' until <- stop_engine_on_first_of(max_generations = 1000, other_func = stop_engine_if_stalling(5))
stop_engine_if_stalling <- function(nb_iterations) {
  best_fitness <- -1
  best_nb <- -1

  function(engine) {
    if (missing(engine)) return(NULL)

    fitness <- get_current_generation_fitness(engine)
    iterations <- get_current_generation_nb(engine)
    if (fitness > best_fitness) {
      ### ! critical: we update the closure variable, from parent
      best_fitness <<- fitness
      best_nb <<- iterations
      # if (verbose) cat("best_fitness", best_fitness, "at iteration",  iterations, "\n")

    } else {
      if ( (iterations - best_nb) >= nb_iterations) return(TRUE)
    }
    FALSE
  }
}

