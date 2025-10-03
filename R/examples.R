#' model parameters and data for the mtcars regression example
#' 
#' @return the example as a named list with:
#'  -  params: the model parameters
#'  - data: the data to fit as a data frame
#' @export
#' @examples 
#'  model <- fuzzycoco("regression", example_mtcars()$params)
#'  fit <- fit(model, qsec ~ ., example_mtcars()$data, max_generations = 20, progress = FALSE)
example_mtcars <- function() {
 pms <- params(
    nb_rules = 2, nb_max_var_per_rule = 3, rules.pop_size = 20, mfs.pop_size = 20, 
    ivars.nb_sets = 3, ivars.nb_bits_vars = 3,  ivars.nb_bits_sets = 2, ivars.nb_bits_pos = 8, 
    ovars.nb_sets = 3, ovars.nb_bits_vars = 1, ovars.nb_bits_sets = 2, ovars.nb_bits_pos = 8, 
    metricsw.sensitivity = 0, metricsw.specificity = 0, metricsw.rmse = 1
  )

  list(
      data = mtcars[c("mpg", "hp", "wt", "qsec")], 
      params = pms
  )
}

