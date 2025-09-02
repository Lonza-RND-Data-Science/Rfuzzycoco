

remove_comments <- function(lines) {
  # inline comments
  lines <- sub("\\s*#[^\"].*", "", lines)
  # fully commented lines
  lines <- sub("^\\s*#.*", "", lines)
  lines[nzchar(lines)]
}

load_params_json <- function(path) {
  lines <- remove_comments(readLines(path))
  lst <- jsonlite::parse_json(lines)
  lst <- complete_params_with_defaults(lst)
}

complete_params_with_defaults <- function(lst) {
  defaults <- params(NA, NA)
  lst <- lst %||% list()
  utils::modifyList(defaults, lst)
}

#' utility to build the Fuzzy Coco parameters data structure
#' @param nb_rules              (mandatory) the number of rules in the fuzzy system
#' @param nb_max_var_per_rule   (mandatory) The maximum number of antecedents (input variables) to use in each rule.
#' @param max_generations,max_fitness,nb_cooperators,influence_rules_initial_population,influence_evolving_ratio,ivars.nb_sets,ivars.nb_bits_vars,ivars.nb_bits_sets,ivars.nb_bits_pos,ovars.nb_sets,ovars.nb_bits_vars,ovars.nb_bits_sets,ovars.nb_bits_pos,rules.pop_size,rules.elite_size,rules.cx_prob,rules.mut_flip_genome,rules.mut_flip_bit,mfs.pop_size,mfs.elite_size,mfs.cx_prob,mfs.mut_flip_genome,mfs.mut_flip_bit,output_vars_defuzz_thresholds,metricsw.sensitivity,metricsw.specificity,metricsw.accuracy,metricsw.ppv,metricsw.rmse,metricsw.rrse,metricsw.rae,metricsw.mse,metricsw.distanceThreshold,metricsw.distanceMinThreshold,metricsw.nb_vars,metricsw.overLearn,metricsw.true_positives,metricsw.false_positives,metricsw.true_negatives,metricsw.false_negatives,features_weights
#'  cf [fuzzycoco doc](https://github.com/Lonza-RND-Data-Science/fuzzycoco/blob/main/PARAMS.md)
#' 
#' @return a nested named list
#' @export
#' @examples 
#' pms <- params(
#'  nb_rules = 2, nb_max_var_per_rule = 3, rules.pop_size = 20, mfs.pop_size = 20, 
#'  ivars.nb_sets = 3, ivars.nb_bits_vars = 3,  ivars.nb_bits_sets = 2, ivars.nb_bits_pos = 8, 
#'  ovars.nb_sets = 3, ovars.nb_bits_vars = 1, ovars.nb_bits_sets = 2, ovars.nb_bits_pos = 8, 
#'  metricsw.sensitivity = 0, metricsw.specificity = 0, metricsw.rmse = 1,
#'  output_vars_defuzz_thresholds = list(3, 17)
#')
params <- function(nb_rules, nb_max_var_per_rule, 
  max_generations = 100,
  max_fitness = 1,
  nb_cooperators = 2,
  influence_rules_initial_population = FALSE,
  influence_evolving_ratio = 0.8,

  ivars.nb_sets = 3,
  ivars.nb_bits_vars = NA_integer_,    
  ivars.nb_bits_sets = NA_integer_,
  ivars.nb_bits_pos = NA_integer_,

  ovars.nb_sets = 3,
  ovars.nb_bits_vars = NA_integer_,    
  ovars.nb_bits_sets = NA_integer_,
  ovars.nb_bits_pos = NA_integer_,

  rules.pop_size = 100,
  rules.elite_size = 5,
  rules.cx_prob = 0.5,
  rules.mut_flip_genome = 0.5,
  rules.mut_flip_bit = 0.025,

  mfs.pop_size = 100,
  mfs.elite_size = 5,
  mfs.cx_prob = 0.5,
  mfs.mut_flip_genome = 0.5,
  mfs.mut_flip_bit = 0.025,

  output_vars_defuzz_thresholds = list(),

  metricsw.sensitivity = 1.0,
  metricsw.specificity = 0.8,
  metricsw.accuracy = 0.0,
  metricsw.ppv = 0.0,
  metricsw.rmse = 0.0,
  metricsw.rrse = 0.0,
  metricsw.rae = 0.0,
  metricsw.mse = 0.0,
  metricsw.distanceThreshold = 0.0,
  metricsw.distanceMinThreshold = 0.0,
  metricsw.nb_vars = 0.0,
  metricsw.overLearn = 0.0,
  metricsw.true_positives = 0.0,
  metricsw.false_positives = 0.0,
  metricsw.true_negatives = 0.0,
  metricsw.false_negatives = 0.0,

  features_weights = list()
) 
{
  list(
    global_params = list(
      nb_rules = nb_rules,
      nb_max_var_per_rule = nb_max_var_per_rule,
      max_generations = max_generations,
      max_fitness = max_fitness,
      nb_cooperators = nb_cooperators,
      influence_rules_initial_population = influence_rules_initial_population,
      influence_evolving_ratio = influence_evolving_ratio 
    ),

    input_vars_params = list(
      nb_sets = ivars.nb_sets,
      nb_bits_vars = ivars.nb_bits_vars,    
      nb_bits_sets = ivars.nb_bits_sets,
      nb_bits_pos = ivars.nb_bits_pos
    ),

    output_vars_params = list(
      nb_sets = ovars.nb_sets,
      nb_bits_vars = ovars.nb_bits_vars,    
      nb_bits_sets = ovars.nb_bits_sets,
      nb_bits_pos = ovars.nb_bits_pos
    ),

    rules_params = list(
      pop_size = rules.pop_size,
      elite_size = rules.elite_size,
      cx_prob = rules.cx_prob,
      mut_flip_genome = rules.mut_flip_genome,
      mut_flip_bit = rules.mut_flip_bit
    ),

    mfs_params = list(
      pop_size = mfs.pop_size,
      elite_size = mfs.elite_size,
      cx_prob = mfs.cx_prob,
      mut_flip_genome = mfs.mut_flip_genome,
      mut_flip_bit = mfs.mut_flip_bit
    ),

    fitness_params = list(
      output_vars_defuzz_thresholds = output_vars_defuzz_thresholds,
      metrics_weights = list(
        sensitivity = metricsw.sensitivity,
        specificity = metricsw.specificity,
        accuracy = metricsw.accuracy,
        ppv = metricsw.ppv,
        rmse = metricsw.rmse,
        rrse = metricsw.rrse,
        rae = metricsw.rae,
        mse = metricsw.mse,
        distanceThreshold = metricsw.distanceThreshold,
        distanceMinThreshold = metricsw.distanceMinThreshold,
        nb_vars = metricsw.nb_vars,
        overLearn = metricsw.overLearn,
        true_positives = metricsw.true_positives,
        false_positives = metricsw.false_positives,
        true_negatives = metricsw.true_negatives,
        false_negatives = metricsw.false_negatives
      ),
      features_weights = features_weights
    )
  )
}