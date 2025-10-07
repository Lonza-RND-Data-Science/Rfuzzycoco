.example_iris_binary_categorical <- 
test_that("example_iris_binary_categorical", {
  case <- example_iris_binary_categorical()

  df <- case$data
  expect_equal(nrow(df), 12)
  expect_identical(names(df),  c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width", "Species"))
  expect_true(is.list(case$params))
})


.example_mtcars <- 
test_that("example_mtcars", {

  case <- example_mtcars()
  df <- case$data
  expect_equal(nrow(df), nrow(mtcars))
  expect_identical(names(df),  c("mpg", "hp", "wt", "qsec"))

  expect_true(is.list(case$params))
})


.example_iris36 <- 
test_that("example_iris36", {

  case <- example_iris36()

  df <- case$data
  expect_equal(nrow(df), 36)
  expect_identical(names(df), c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width", "OUT"))
  expect_equal(sum(df$OUT), 18)

  params <- resolve_params(case$params, df["OUT"], FALSE)
  expect_no_error(check_params(params, 1))
})
