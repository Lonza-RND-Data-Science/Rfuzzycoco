


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

#' lowest-level implementation of the fitting of a fuzzy coco model using the **hynrid engine**
#' 
#' @return a named list as a `fuzzy_coco` fit object
#' @inheritParams shared_params
#' @export
fuzzycoco_fit_df_hybrid <- function(model, x, y, 
  until = stop_engine_on_first_of(
    max_generations = model$params$global_params$max_generations, 
    max_fitness = model$params$global_params$max_fitness
  ), verbose = model$verbose) 
{
  stop_unless(length(model$params$fitness_params$output_vars_defuzz_thresholds) == ncol(y),
    "bad params$fitness_params$output_vars_defuzz_thresholds, must be defined for all output vars")

  progressr <- requireNamespace("progressr")
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

stop_engine_if_stalling <- function(nb_iterations, verbose = FALSE) {
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
      if (verbose) cat("best_fitness", best_fitness, "at iteration",  iterations, "\n")

    } else {
      if ( (iterations - best_nb) >= nb_iterations) return(TRUE)
    }
    FALSE
  }
}

