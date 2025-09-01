

fs_rules_to_df <- function(fuzzy_system_desc) {
  all_vars <- with(fuzzy_system_desc$variables, c(names(input), names(output)))
 .rule_to_mat <- function(rule_desc) {
    mat <- matrix(0, ncol = length(all_vars), nrow = 1)
    colnames(mat) <- all_vars
    ants <- rule_desc$antecedents
    for (var in names(ants)) {
      mat[, var] <- as.integer(sub('^.+\\.', '', names(ants[[var]])))
    }
    cons <- rule_desc$consequents
    for (var in names(cons)) {
      mat[, var] <- as.integer(sub('^.+\\.', '', names(cons[[var]])))
    }
    mat
  }
  rows <- lapply(fuzzy_system_desc$rules, .rule_to_mat)
  ns <- paste0("rule ", seq_along(rows))
  # default rule
  mat <- matrix(0, ncol = length(all_vars), nrow = 1)
  colnames(mat) <- all_vars
  def <- fuzzy_system_desc$default_rules
  for (var in names(def)) {
    mat[, var] <- as.integer(sub('^.+\\.', '', def[[var]]))
  }

  rows <- c(rows, list(mat))
  ns <- c(ns, "default rule")

  mat <- do.call(rbind, rows)
  rownames(mat) <- ns

  as.data.frame(mat)
}

fs_used_vars_to_df <- function(fuzzy_system_desc) {
  df <- fs_rules_to_df(fuzzy_system_desc)
  used <- colSums(df) > 0

  as.data.frame(t(used))
}

fit_to_df <- function(fit, metric = NULL) {
  df <- fs_used_vars_to_df(fit$fuzzy_system)
  df$fitness <- fit$fit$fitness
  df$generations <- fit$fit$generations
  df$seed <- fit$seed

  if (length(metric)) {
    df[[metric]] <- fit$fit$metrics[[metric]]
  }

  df
}