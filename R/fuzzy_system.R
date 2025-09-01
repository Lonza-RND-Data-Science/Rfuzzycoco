


fs_get_rules <- function(fs) { fs$rules }
fs_set_rules <- function(fs, rules) { 
  fs$rules <- rules
  fs  
}

fs_remove_rule <- function(fs, i) {
  fs_set_rules(fs, fs_get_rules(fs)[-i])
}

fs_add_rule <- function(fs, rule) {
  rules <- fs_get_rules(fs)
  new_rule <- list(rule)
  names(new_rule) <- paste0("rule", length(rules) + 1)
  rules <- c(fs_get_rules(fs), new_rule)
  
  fs_set_rules(fs, rules)
}

new_rule <- function(antecedents, consequents, pos = NA_real_) {
  rule <- list()
  .process_term <- function(antecedent) {
    set <- parse_fuzzyset(antecedent)
    res <- list()
    value <- list()
    value[[antecedent]] <- pos
    res[[set$name]] <- value
    
    res
  }
  names(antecedents) <- names(consequents) <- NULL

  rule$antecedents <- sapply(antecedents, .process_term, USE.NAMES = FALSE)
  rule$consequents <- sapply(consequents, .process_term, USE.NAMES = FALSE)
  
  rule
}

new_default_rule <- function(out_sets) {
  rule <- list()
  .process_term <- function(term) {
    set <- parse_fuzzyset(term)
    res <- list()
    res[[set$name]] <- term
    
    res
  }
  names(out_sets) <- NULL
  def <- sapply(out_sets, .process_term, USE.NAMES = FALSE)

  def
}

discard_rule_values <- function(rule, value = NA_real_) {
  for (i in seq_along(rule$antecedents)) {
    rule$antecedents[[i]][[1]] <- value
  }
  for (i in seq_along(rule$consequents)) {
    rule$consequents[[i]][[1]] <- value
  }

  rule
}


parse_fuzzyset <- function(name) {
  tokens <- strsplit(name, "\\.")[[1]]
  var_name <- paste0(tokens[-length(tokens)], collapse = ".")
  list(name = var_name, set = as.integer(tokens[length(tokens)]))
}

fs_replace_positions <- function(fs, pos) {
  invars <- names(fs$variables$input)
  outvars <- names(fs$variables$output)
  for (var in names(pos)) {
    if (var %in% invars) fs$variables$input[[var]] <- pos[[var]]
    if (var %in% outvars) fs$variables$output[[var]] <- pos[[var]]
  }
  
  fs
}