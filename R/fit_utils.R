
#' format the fuzzy rules as a data frame
#' 
#' @inheritParams shared_params
#' @return a data frame, one row per rule, including the default rule, in columns the input and ouput variables.
#'  The values are the corresponding fuzzy set number.
#' @family fit_utils 
#' @export
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

#' extract the usage of the variables by a fuzzy system
#' 
#' @inheritParams shared_params
#' @return a one-row data frame, in columns the input and ouput variables, with TRUE iff the variable is used.
#' @family fit_utils 
#' @export
fs_used_vars_to_df <- function(fuzzy_system_desc) {
  df <- fs_rules_to_df(fuzzy_system_desc)
  used <- colSums(df) > 0

  as.data.frame(t(used))
}

#' a one-row overview of a fuzzy system with the usage of variables, the fitness, number of generations and optionally 
#'  a metric
#' 
#' @param fit   a fit object, as returned by [fit.
#' @param metric    an optional metric name to report (e.g. `rmse`)
#' @return a one-row data frame
#' @family fit_utils 
#' @export
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