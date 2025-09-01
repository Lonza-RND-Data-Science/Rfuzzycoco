

# creata a function that applies a metric based on comparing two columns on all corresponding columns of 2 dfs 
apply_column_metric_to_dfs <- function(metric, aggregate = mean) {
  function(df, df0) {
    stop_unless(ncol(df) == ncol(df0), "error, must have the same number of columns")
    .process_col <- function(col) metric(df[[col]], df0[[col]])
    res_by_col <- sapply(seq_along(df), .process_col)
    aggregate(res_by_col)
  }
}

mse <- function(y, y0) sum( (y - y0)^2 ) / length(y)
rmse <- function(y, y0) sqrt(mse(y, y0))

mean_error <- function(y, y0) (y + y0) / 2
rae <- function(y, y0) sum(abs( (y - y0) / mean_error(y, y0))) / length(y0)
rrse <- function(y, y0) sqrt(sum( ( (y - y0) / mean_error(y, y0) )^2) / length(y0))

classification_metrics <- function(y, y0) {
  TP <- sum(y0 > 0 & y > 0 )
  TN <- sum(y0 <= 0 & y <= 0 )
  FP <- sum(y0 <= 0 & y > 0 )
  FN <- sum(y0 > 0 & y <= 0 )
  
  sensitivity <- TP / (TP + FN)
  specificity <- TN / (TN + FP)
  accuracy <- (TP + TN) / length(y0)
  
  list(TP = TP, TN = TN, FP = FP, FN = FN, 
    sensitivity = sensitivity, specificity = specificity, accuracy = accuracy)
}

classification_metrics_df <- function(y, y0) {
  stop_unless(ncol(y) == ncol(y0), "error, must have the same number of columns")

  .process_col <- function(col) as.integer(classification_metrics(y[[col]], y0[[col]]))
  mat <- sapply(seq_along(y), .process_col)
  mat <- t(mat[1:4, ])
  met <- as.list(colSums(mat))
  names(met) <- c("TP", "TN", "FP", "FN")

  with(met, {
    sensitivity <- TP / (TP + FN)
    specificity <- TN / (TN + FP)
    accuracy <- (TP + TN) / length(y0)
    list(TP = TP, TN = TN, FP = FP, FN = FN, 
      sensitivity = sensitivity, specificity = specificity, accuracy = accuracy)
  })
}
