preprocess_y_classification <- function(y, thresholds = NULL)
{
  # check and transform the binary responses (we accept NAs)
  y2 <- process_classification_response_df(y, thresholds)
  # now set the thresholds for binary 01 vars
  thresholds <- as.list(rep(0.5, length(y2)))

  list(y = y2, thresholds = thresholds)
}

process_classification_response_df <- function(y, thresholds) {
  thresholds <- thresholds %||% list()
  length(thresholds) <- length(y)
  for (col in seq_along(y)) {
    y[[col]] <- process_classification_response(y[[col]], thresholds[[col]])
  }

  y
}

# accept either a binary variable, or a numerical one with a threshold
# in the latter, the variable is transformed into a binary var using the threshold
# N.B: if we bin using a threshold into [0,1], we need to change the threshold to
# 0.5
process_classification_response <- function(vec, threshold = NULL) {
  if (length(threshold) && is.numeric(vec)) {
    solid_values <- na.omit(unique(vec))
    if (length(solid_values) > 2) { 
      return(bin_continuous_response_to_01(vec, threshold))
    }
  }

  transform_binary_response_to_01(vec)
}

bin_continuous_responses_to_01 <- function(y, thresholds = lapply(y, median)) {
  stop_unless(is.data.frame(y), "only for data frames")
  stop_unless(length(thresholds) == ncol(y), "bad arg 'thresholds' length: must match y columns number")
  for (col in seq_along(y)) {
    y[[col]] <- bin_continuous_response_to_01(y[[col]], thresholds[[col]])
  }
  y
}

bin_continuous_response_to_01 <- function(vec, threshold = median(vec)) {
  ifelse(vec < threshold, 0L, 1L)
}

transform_binary_responses_to_01 <- function(y) {
  stop_unless(is.data.frame(y), "only for data frames")
  for (col in seq_along(y)) {
    y[[col]] <- transform_binary_response_to_01(y[[col]])
  }
  y
}

# transform a classification response variable as a binary integer variable
transform_binary_response_to_01 <- function(vec) {
  stop_if(length(dim(vec)), "only for vectors")

  values <- unique(vec)
  solid_values <- na.omit(values)
  stop_if(length(solid_values) != 2, "only binary response")

  # recode?
  vecfact <- as.factor(vec)
  levels(vecfact) <- c(0, 1)
  vec2 <- as.numeric(as.character(vecfact))
  
  vec2
}