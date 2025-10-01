test_that("lst_convert", {
  # simple scalar stuff
  lst <- list(int = 1L, num = pi, str = "toto", bool = TRUE)
  lst2 <- lst_convert(lst)
  expect_identical(lst2, lst)

  # NULL
  expect_error(lst_convert(list(null = NULL)), "NULL not supported")

  # unamed list
  lst <- list(1L, pi, "toto", TRUE)
  lst2 <- lst_convert(lst)
  expect_identical(lst2, lst)

  # partially named list
  lst <- list(1L, num = pi, str = "toto", TRUE)
  lst2 <- lst_convert(lst)
  expect_identical(lst2, lst)

  # vector are not supported (only scalars)
  expect_error(lst_convert(list(intvec = 1:10)), "not supported")
  expect_error(lst_convert(list(intvec = as.numeric(1:10))), "not supported")
  expect_error(lst_convert(list(intvec = as.character(1:10))), "not supported")
  expect_error(lst_convert(list(intvec = as.logical(1:10))), "not supported")

  # nested lists...
  lst <- list(int = 0L, sublist = list(num = pi, bool = FALSE), str = "toto")
  lst2 <- lst_convert(lst)
  expect_identical(lst2, lst)

  lst <- list(int = 0L, sublist = list(num = pi, bool = FALSE, subsublist = list(1, "deux")), str = "toto")
  lst2 <- lst_convert(lst)
  expect_identical(lst2, lst)

  ### NAs: N.B: NamedList has no NA support for booleans and strings
  expect_error(lst_convert(list(na_bool = NA)), "not supported")
  expect_error(lst_convert(list(na_str = NA_character_)), "not supported")

  expect_identical(lst_convert(list(na_int = NA_integer_)), list(na_int = NA_integer_))
  expect_identical(lst_convert(list(na_real = NA_real_)), list(na_real = NA_real_))

  #### unsupported types
  expect_error(lst_convert(list(x = raw())), "Unsupported type")
})
