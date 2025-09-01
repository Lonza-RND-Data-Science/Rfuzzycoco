test_that("df_convert", {
  df <- head(mtcars)
  df2 <- df_convert(df)
  expect_identical(df2, df)
  
  ### with factors --> converted to double for now
  df <- head(iris)
  df2 <- df_convert(df)
  expect_true(is.double(df2$Species))

  df2$Species <- factor(levels(df$Species)[df2$Species], levels(df$Species))
  # rownames are always character
  expect_identical(rownames(df2), as.character(rownames(df)))
  rownames(df2) <- as.integer(rownames(df2))
  expect_identical(df2, df)


  ### regression: only one column
  df <- data.frame(OUT = rep(0, 10))
  df2 <- df_convert(df)

  ### with non-numeric: not supported
  df <- data.frame(a = "toto")
  expect_error(df_convert(df), "only numeric columns are allowed")
})
