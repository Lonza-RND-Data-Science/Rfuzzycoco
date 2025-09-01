.rng <- 
test_that("rng", {
  x <- rng(123)
  expect_equal(x$random_int(1, 1), 1)
  expect_equal(x$randomReal(0, 0), 0)
})
