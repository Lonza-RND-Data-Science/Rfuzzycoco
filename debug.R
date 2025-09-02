library(devtools)
load_all(export_all = FALSE)

x <- mtcars[c("mpg", "hp", "wt")]
y <- mtcars["qsec"]
pms <- params(
  nb_rules = 2, nb_max_var_per_rule = 3, rules.pop_size = 20, mfs.pop_size = 20, 
  ivars.nb_sets = 3, ivars.nb_bits_vars = 3,  ivars.nb_bits_sets = 2, ivars.nb_bits_pos = 8, 
  ovars.nb_sets = 3, ovars.nb_bits_vars = 1, ovars.nb_bits_sets = 2, ovars.nb_bits_pos = 8, 
  metricsw.sensitivity = 0, metricsw.specificity = 0, metricsw.rmse = 1,
  output_vars_defuzz_thresholds = 17
)
model <- fuzzycoco("regression", pms)
fit <- fuzzycoco_fit_df_hybrid(model, x, y)


