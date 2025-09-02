# systematic search and related tools

fuzzy_coco_systematic_fit <- function(x, y, params, fitter) {
  invars <- names(x)
  outvars <- names(y)
  fs <- list(
    variables = list(input = list(), output = list()),
    rules = list(), 
    default_rules = list()
  )

  .pos_to_fset <- function(var, pos) {
    lst <- as.list(pos[[var]])
    names(lst) <- paste0(var, ".", seq_along(pos[[var]]))
    lst
  }

  ipos <- compute_optimal_quantile_fuzzy_set_positions(x, params$input_vars_params$nb_sets)
  fs$variables$input <- lapply(invars, .pos_to_fset, ipos)
  names(fs$variables$input) <- invars

  opos <- compute_optimal_quantile_fuzzy_set_positions(y, params$output_vars_params$nb_sets)
  fs$variables$output <- lapply(outvars, .pos_to_fset, opos)
  names(fs$variables$output) <- outvars

  nb_rules <- params$global_params$nb_rules

  nb_max_var_per_rule <- params$global_params$nb_max_var_per_rule

  grid <- make_rules_grid(nb_rules, invars, outvars, nb_max_var_per_rule, 
    params$input_vars_params$nb_sets, params$output_vars_params$nb_sets)

  progressr <- requireNamespace("progressr")
  p <- NULL
  if (progressr) p <- progressr::progressor(nrow(grid))

  nb_out_vars <- length(outvars)
  df <- cbind(x, y)
  .build_fs <- function(i) {
    rules <- grid_row_to_rules(grid[i, ], nb_max_var_per_rule, nb_out_vars)
    fs$rules <- rules$rules
    fs$default_rules <- rules$default_rules
    fs
  }
  .process_row <- function(i) {
    if (progressr) p()
    res <- evaluate_fuzzy_system(.build_fs(i), df, params)
    fitter(res$metrics)
  }

  res <- sapply(seq_len(nrow(grid)), .process_row)

  best_value <- max(res)
  best_idx <- which(res == best_value)
  
  .build_res <- function(idx) {
    list(metric = best_value, fs = .build_fs(idx))
  }
  
  lapply(best_idx, .build_res)
}


grid_row_to_rules <- function(row, nb_max_var_per_rule, nb_out_vars) {
  rule_size <- nb_max_var_per_rule + nb_out_vars
  defrule_size <- nb_out_vars
  nb_rules <- (length(row) - defrule_size) / rule_size

  .build_rule <- function(i) {
    offset <- rule_size * (i - 1)
    # N.B: input vars with set=NA must be discarded
    isets <- row[offset + seq_len(nb_max_var_per_rule)]
    
    new_rule(
      isets[!is.na(isets)], 
      row[offset + nb_max_var_per_rule + seq_len(nb_out_vars)], 
      0
    )
  }
  rules <- lapply(seq_len(nb_rules), .build_rule)
  defrule <- new_default_rule(row[rule_size * nb_rules + seq_len(nb_out_vars)])

  list(rules = rules, default_rules = defrule)
}

# make a grid for all possible combination for a rule with at most nb_max_var_per_rule vars
make_half_rule_grid <- function(vars, nb_max_var_per_rule, nb_sets) {
  sets_per_var <- rep(nb_sets, length(vars))

  .make_grid <- function(nb) {
    grid <- make_sets_grid(vars, nb, sets_per_var)
    # complete it with NAs
    if (nb < nb_max_var_per_rule) {
      args <- list(grid, rep(list(NA), nb_max_var_per_rule - nb))
      na_df <- do.call(data.frame, rep(list(NA), nb_max_var_per_rule - nb))
      grid <- cbind(grid, na_df)
    }
    # N.B: need names for rbind()
    names(grid) <- seq_along(grid)
    grid
  }
  grids <- lapply(seq_len(nb_max_var_per_rule), .make_grid)
  grid <- do.call(rbind, grids)
  
  grid
}


make_rules_grid <- function(nb_rules, invars, outvars, nb_max_var_per_rule, in_nb_sets, out_nb_sets) {
  grid_in <- make_half_rule_grid(invars, nb_max_var_per_rule, in_nb_sets)
  grid_out <- make_half_rule_grid(outvars, length(outvars), out_nb_sets)

  grid <- merge(grid_in, grid_out, by = NULL)

  if (nb_rules > 1) {
    grid_tmp <- grid
    for (i in seq_len(nb_rules - 1)) {
      grid <- merge(grid, grid_tmp, by = NULL)
    }
  }
  

  default_rules_grid <- grid_out
  grid <- merge(grid, default_rules_grid, by = NULL)

  names(grid) <- NULL

  grid
}


make_sets_grid <- function(vars, nb_var_per_rule, nb_sets_per_var) {
  stop_unless(length(vars) == length(nb_sets_per_var), "nb_sets_per_var length must match vars")

  # generate the fuzzy set names for each var
  sets <- lapply(seq_along(vars), \(i) c(paste0(vars[i], ".", seq_len(nb_sets_per_var[i]))))
  names(sets) <- vars

  vars_combn <- utils::combn(vars, nb_var_per_rule, simplify = FALSE)
  .process_selected_vars <- function(selected_vars) {
    df <- do.call(expand.grid, c(sets[selected_vars], stringsAsFactors = FALSE))
    names(df) <- paste0("set", seq_along(df))

    df
  }
  dfs <- lapply(vars_combn, .process_selected_vars)
  grid <- do.call(rbind, dfs)
  names(grid) <- NULL

  grid
}


compute_optimal_quantile_fuzzy_set_positions_vec <- function(x, nb_sets) {
  y <- stats::quantile(x, probs = seq(0, 1, 1 / (nb_sets + 1)))
  unname(y[c(-1, -length(y))])
}

compute_optimal_quantile_fuzzy_set_positions <- function(df, nb_sets) {
  lapply(df, compute_optimal_quantile_fuzzy_set_positions_vec, nb_sets)
}