.apply_column_metric_to_dfs <- 
test_that("apply_column_metric_to_dfs", {
  ones <- rep(1L, 100)
  zeroes <- rep(0L, 100)
  mix <- rep(0:1, 50)
  df1 <- data.frame(mix1 = mix, mix2 = mix, ones = ones)
  df2 <- data.frame(ones = ones, zeroes = zeroes, ones = ones)

  mse_df <- apply_column_metric_to_dfs(mse)

  expect_equal(mse_df(df1, df2), mean(c(mse(df1[[1]], df2[[1]]), mse(df1[[2]], df2[[2]]), mse(df1[[3]], df2[[3]]))))
})


.classification_metrics_df <- 
test_that("classification_metrics_df", {
  ones <- rep(1L, 100)
  zeroes <- rep(0L, 100)
  mix <- rep(0:1, 50)
  df1 <- data.frame(mix1 = mix, mix2 = mix)
  df2 <- data.frame(ones = ones, zeroes = zeroes)

  x <- classification_metrics_df(df1, df2)

  expect_identical(x, 
    list(TP = 50, TN = 50, FP = 50, FN = 50, sensitivity = 0.5, specificity = 0.5, accuracy = 50))
})

.classification_metrics <- 
test_that("classification_metrics", {
  ones <- rep(1L, 100)
  zeroes <- rep(0L, 100)
  mix <- rep(0:1, 50)

  x <- classification_metrics(zeroes, zeroes)
  expect_identical(x,
    list(TP = 0L, TN = 100L, FP = 0L, FN = 0L, sensitivity = NaN, specificity = 1, accuracy = 1))
  
  x <- classification_metrics(zeroes, ones)
  expect_identical(x,
    list(TP = 0L, TN = 0L, FP = 0L, FN = 100L, sensitivity = 0, specificity = NaN, accuracy = 0))

  x <- classification_metrics(ones, zeroes)
  expect_identical(x,
    list(TP = 0L, TN = 0L, FP = 100L, FN = 0L, sensitivity = NaN, specificity = 0, accuracy = 0))

  x <- classification_metrics(ones, ones)
  expect_identical(x,
    list(TP = 100L, TN = 0L, FP = 0L, FN = 0L, sensitivity = 1, specificity = NaN, accuracy = 1))

  x <- classification_metrics(mix, mix)
  expect_identical(x,
    list(TP = 50L, TN = 50L, FP = 0L, FN = 0L, sensitivity = 1, specificity = 1, accuracy = 1))

  x <- classification_metrics(mix, ones)
  expect_identical(x,
    list(TP = 50L, TN = 0L, FP = 0L, FN = 50L, sensitivity = 0.5, specificity = NaN, accuracy = 0.5))

  x <- classification_metrics(ones, mix)
  expect_identical(x,
    list(TP = 50L, TN = 0L, FP = 50L, FN = 0L, sensitivity = 1, specificity = 0, accuracy = 0.5))
})
