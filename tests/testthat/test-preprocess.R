

.preprocess_y_classification <- 
test_that("preprocess_y_classification", {
  CASE <- MTCARS_MULTI_OUTPUT()
  df <- CASE$data

  ####################### one numerical variable ########################
  x <- df[-4]
  y <- df[4]

  expect_error(preprocess_y_classification(y), "only binary response")
  # --> we must use a threshold
  thresholds <- median(y[[1]])

  data <- preprocess_y_classification(y, thresholds = thresholds)

  y2 <- y
  y2[[1]] <- process_classification_response(y[[1]], thresholds)
  expect_identical(data, list(y = y2, thresholds = list(0.5)))


  ####################### regression 2 variables and selected regressor ########################
  x <- df["wt"]
  y <- df[c("qsec", "hp")]
  thresholds <- lapply(y, median)

  data <- preprocess_y_classification(y, thresholds = thresholds)

  y2 <- process_classification_response_df(y, thresholds)
  expect_identical(data, list(y = y2, thresholds = as.list(rep(0.5, 2))))
})


.process_classification_response <- 
test_that("process_classification_response", {
  
  ### continuous var to bin
  y <- 1:100
  y[c(10, 20, 30, 56)] <- NA
  ref <- c(rep(0L, 49), rep(1L, 51))

  y2 <- process_classification_response(y, 50)

  expect_equal(is.na(y2), is.na(y))
  expect_identical(y2[!is.na(y2)], ref[!is.na(y2)]) 

  ### continuous var no threshold
  expect_error(process_classification_response(y), "only binary response")

  ## character
  y <- rep(c("HI", "LO", NA), 10)
  y2 <- process_classification_response(y)
  expect_identical(y2, rep(c(0, 1, NA), 10))

})

.bin_continuous_responses_to_01 <- 
test_that("bin_continuous_responses_to_01", {

  ref <- c(rep(0L, 49), rep(1L, 51))
  y <- data.frame(y1 = 1:100, y2 = ref)

  y2 <- bin_continuous_responses_to_01(y, c(50, 0.5))

  expect_identical(y2, data.frame(y1 = ref, y2 = ref))

  ### factors
  y[[2]] <- factor(y[[2]])
  levels(y[[2]]) <- LETTERS[1:2]

  expect_error(bin_continuous_responses_to_01(y), "need numeric data")
})

.bin_continuous_response_to_01 <- 
test_that("bin_continuous_response_to_01", {

  ref <- c(rep(0L, 49), rep(1L, 51))
  expect_identical(bin_continuous_response_to_01(1:100, 50), ref)

  expect_identical(bin_continuous_response_to_01(ref, 0.5), ref)

  ### same with NAs
  y <- 1:100
  y[c(10, 20, 30, 56)] <- NA

  y2 <- bin_continuous_response_to_01(y, 50)
  
  # NAs are preserved
  expect_equal(is.na(y2), is.na(y))
  # otherwise it agrees
  expect_identical(y2[!is.na(y2)], ref[!is.na(y2)]) 

  ### all 0
  expect_identical(bin_continuous_response_to_01(1:100, 1000), rep(0L, 100))
  ### all 1
  expect_identical(bin_continuous_response_to_01(1:100, 0), rep(1L, 100))
})


.transform_binary_responses_to_01 <- 
test_that("transform_binary_responses_to_01", {

  yf <- factor(rep(c("HI", "LO", NA), 10))
  levels(yf) <- c("LO", "HI")

  y <- data.frame(
    ints_01_no_nas = rep(c(0L, 1L), 15),
    ints_01 = rep(c(0L, 1L, NA), 10),
    ints = rep(c(50L, -2L, NA), 10),
    numeric = rep(c(pi, -sqrt(2), NA), 10),
    character = rep(c("HI", "LO", NA), 10),
    factor = yf
  )

  y2 <- transform_binary_responses_to_01(y)
  ref <- data.frame(
    ints_01_no_nas = as.numeric(y$ints_01_no_nas),
    ints_01 = as.numeric(y$ints_01),
    ints = rep(c(1, 0, NA), 10),
    numeric = rep(c(1, 0, NA), 10),
    character = rep(c(0, 1, NA), 10),
    factor = rep(c(0, 1, NA), 10)
  )

  expect_identical(y2, ref)

  ### > 2 levels --> error
  y <- data.frame(bad = as.numeric(1:3))
  expect_error(transform_binary_responses_to_01(y), "only binary response") 
})



.transform_binary_response_to_01 <- 
test_that("transform_binary_response_to_01", {

  ### ======= already binary =========== --> cf transform_binary_responses_to_01 test
  
  ### > 2 levels --> error
  expect_error(transform_binary_response_to_01(as.numeric(1:3)), "only binary response")
  expect_error(transform_binary_response_to_01(as.integer(1:3)), "only binary response")
  expect_error(transform_binary_response_to_01(LETTERS), "only binary response")
  expect_error(transform_binary_response_to_01(as.factor(LETTERS)), "only binary response")  

  ### < 2 levels --> same
  expect_error(transform_binary_response_to_01(as.numeric(1)), "only binary response")
  expect_error(transform_binary_response_to_01(as.integer(1)), "only binary response")
  expect_error(transform_binary_response_to_01(LETTERS[1]), "only binary response")
  expect_error(transform_binary_response_to_01(as.factor(LETTERS[1])), "only binary response")  
  expect_error(transform_binary_response_to_01(rep(NA, 10L)), "only binary response")  
})



