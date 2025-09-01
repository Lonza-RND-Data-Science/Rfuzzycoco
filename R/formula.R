preprocess_data_with_formula <- function(fml, data) {
  df <- stats::model.frame(fml, data)

  # detect multiple responses
  dfout <- if (is.matrix(df[[1]])) as.data.frame(df[[1]]) else df[1]
  dfin <- df[, -1, drop = FALSE]

  list(dfin = dfin, dfout = dfout)
}
