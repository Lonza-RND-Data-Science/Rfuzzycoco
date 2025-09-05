TESTDATA <- 'testdata'

IRIS36_PARAMS_PATH <- file.path(TESTDATA, "iris36/params.json")
IRIS36_CSV_PATH <- file.path(TESTDATA, "iris36/iris36.csv")

# an example dataset from the fuzzycoco c++ lib
# it contains 36 rows from iris, plus a custom binary outcome OUT 
# it also contains some exoected 
IRIS36 <- function() {
  df <- utils::type.convert(utils::read.csv2(IRIS36_CSV_PATH), as.is = TRUE)
  df[[1]] <- NULL

  pms <- load_params_json(IRIS36_PARAMS_PATH)

  rules <- list(seed123 = 
    list(rule1 = list(
      antecedents = list(Petal.Length = list(Petal.Length.2 = 4.36393)), 
      consequents = list(OUT = list(OUT.2 = 1)))
    )
  )

  list(data = df, params = pms, rules = rules)
}

# an example dataset based on mtcars
# to test a regression on multiple output variables

MTCARS_MULTI_OUTPUT <- function() {
  lst <- params(
    nb_rules = 2, nb_max_var_per_rule = 3, rules.pop_size = 20, mfs.pop_size = 20, 
    ivars.nb_sets = 3, ivars.nb_bits_vars = 3,  ivars.nb_bits_sets = 2, ivars.nb_bits_pos = 8, 
    ovars.nb_sets = 3, ovars.nb_bits_vars = 1, ovars.nb_bits_sets = 2, ovars.nb_bits_pos = 8, 
    metricsw.sensitivity = 0, metricsw.specificity = 0, metricsw.rmse = 1,
    output_vars_defuzz_thresholds = list(3, 17)
  )

  list(
      data = mtcars[c("mpg", "hp", "wt", "qsec")], 
      params = lst
  )
}

# an example dataset based on iris with a binary categorical (non-factor) response
IRIS_CATEGORICAL_BINARY <- function() {
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