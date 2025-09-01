.IRIS_CATEGORICAL_BINARY <- 
test_that("IRIS_CATEGORICAL_BINARY", {
  case <- IRIS_CATEGORICAL_BINARY()

  df <- case$data
  expect_equal(nrow(df), 12)
  expect_identical(names(df),  c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width", "Species"))
  expect_true(is.list(case$params))
})


.IRIS36 <- 
test_that("IRIS36", {
  case <- IRIS36()
  df <- case$data
  expect_equal(nrow(df), 36)
  expect_identical(names(df),  c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width", "OUT"))

  expect_true(is.list(case$params))

  expect_true(is.list(case$rules$seed123))
})

.MTCARS_MULTI_OUTPUT <- 
test_that("MTCARS_MULTI_OUTPUT", {

  case <- MTCARS_MULTI_OUTPUT()
  df <- case$data
  expect_equal(nrow(df), nrow(mtcars))
  expect_identical(names(df),  c("mpg", "hp", "wt", "qsec"))

  expect_true(is.list(case$params))
})


