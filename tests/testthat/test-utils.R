

.stop_if <- 
test_that("stop_if", {
  expect_error(stop_if(TRUE, "foo"), "foo")
  expect_no_error(stop_if(FALSE, stop()))

  expect_error(stop_if(TRUE, "%s", LETTERS[1:3]), "A,B,C")
  expect_error(stop_if(TRUE, "%s", "argh"), "argh")
  expect_error(stop_if(TRUE, "%s", NULL), "")

  ## error message handling
  expect_error(stop_if(TRUE, stop("foo")), "could not build error message")

  expect_error(stop_if(TRUE, 1:3), "must be a scalar")

  ## error on condition
  expect_error(stop_if(NA, "foo"), "condiction must not be missing")
  expect_error(stop_unless(LETTERS[1:3], "foo"), "condition must be a scalar")
})