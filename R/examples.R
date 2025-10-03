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

#' model parameters and data for the IRIS36 classification example
#' 
#' a small (36 rows) dataset extracted from iris with a binary 0/1 outcome `OUT` respnse variable 
#'
#' @return the example as a named list with:
#'  -  params: the model parameters
#'  - data: the data to fit as a data frame
#' @export
#' @examples 
#'  model <- fuzzycoco("classification", example_iris36()$params)
#'  fit <- fit(model, qsec ~ ., example_iris36()$data, max_generations = 20, progress = FALSE)
example_iris36 <- function() {
 pms <- params(
    nb_rules = 3, nb_max_var_per_rule = 3, 
    ivars.nb_sets = 2, ivars.nb_bits_vars = 14,  ivars.nb_bits_sets = 1, ivars.nb_bits_pos = 10, 
    ovars.nb_sets = 2, ovars.nb_bits_vars = 1, ovars.nb_bits_sets = 1, ovars.nb_bits_pos = 1, 
    rules.pop_size = 100, rules.elite_size = 3, rules.cx_prob = 0.8, rules.mut_flip_genome = 0.8, rules.mut_flip_bit = 0.1,
    mfs.pop_size = 100, mfs.elite_size = 3, mfs.cx_prob = 0.8, mfs.mut_flip_genome = 0.8, mfs.mut_flip_bit =  0.1,
    metricsw.sensitivity = 1, metricsw.specificity = 0.8, metricsw.rmse = 0.2, metricsw.distanceThreshold = 0.2
  )

  zeroes <- c(2:5, 7, 11, 14, 16, 19, 26, 30, 32:36, 40, 47)
  ones <- c(51, 53:54, 57, 59, 61:62, 72, 74, 76:77, 80:83, 87, 90, 99)

  df <- iris[c(zeroes, ones + 50), ]
  df$Species <- NULL
  df$OUT <- c(rep(0, length(zeroes)), rep(1, length(zeroes)))
  list(
      data =df, 
      params = pms
  )
}


#' model parameters and data for the IRIS36 classification example
#' 
#' an example dataset based on iris with a binary categorical (non-factor) response
#'
#' @return the example as a named list with:
#'  -  params: the model parameters
#'  - data: the data to fit as a data frame
#' @export
#' @examples 
#'  model <- fuzzycoco("classification", example_iris_binary_categorical()$params)
#'  fit <- fit(model, qsec ~ ., example_iris_binary_categorical()$data, max_generations = 20, progress = FALSE)
example_iris_binary_categorical <- function() {
  df <- rbind(head(iris), tail(iris))
  df$Species <- as.character(df$Species)

  lst <- params(
    nb_rules = 2, nb_max_var_per_rule = 3, rules.pop_size = 20, mfs.pop_size = 20, 
    ivars.nb_sets = 3, ivars.nb_bits_vars = 3,  ivars.nb_bits_sets = 2, ivars.nb_bits_pos = 8, 
    ovars.nb_sets = 2, ovars.nb_bits_vars = 1, ovars.nb_bits_sets = 1, ovars.nb_bits_pos = 1, 
    metricsw.sensitivity = 0, metricsw.specificity = 0, metricsw.rmse = 1,
    output_vars_defuzz_thresholds = 0.5
  )

  list(
      data = df, 
      params = lst
  )
}
