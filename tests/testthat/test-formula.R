.preprocess_data_with_formula <- 
test_that("preprocess_data_with_formula", {
  CASE <- MTCARS_MULTI_OUTPUT()

  ### one response already at the end of the df
  lst <- preprocess_data_with_formula(qsec ~ ., CASE$data)

  df <- cbind(lst[[1]], lst[[2]])

  expect_identical(names(df), c("mpg", "hp", "wt", "qsec"))
  expect_identical(names(lst$dfout), "qsec")

  ### one response already NOT at the end of the df
  lst <- preprocess_data_with_formula(mpg ~ ., CASE$data)
    
  df <- cbind(lst[[1]], lst[[2]])
  expect_identical(names(df), c("hp", "wt", "qsec", "mpg"))
  expect_identical(names(lst$dfout), "mpg")

  ### two reponses
  lst <- preprocess_data_with_formula(cbind(mpg, wt) ~ ., CASE$data)

  df <- cbind(lst[[1]], lst[[2]])
  expect_identical(names(df), c("hp", "qsec", "mpg", "wt"))
  expect_identical(names(lst$dfout), c("mpg", "wt"))

  ### one response, some regressors
  lst <- preprocess_data_with_formula(qsec ~ hp + mpg, CASE$data)

  df <- cbind(lst[[1]], lst[[2]])
  expect_identical(names(df), c("hp", "mpg", "qsec"))
  expect_identical(names(lst$dfout), "qsec")

  #### 2 responses, 1 regressor
  lst <- preprocess_data_with_formula(cbind(mpg, wt) ~ qsec, CASE$data)

  df <- cbind(lst[[1]], lst[[2]])
  expect_identical(names(df), c("qsec", "mpg", "wt"))
  expect_identical(names(lst$dfout), c("mpg", "wt"))
})
