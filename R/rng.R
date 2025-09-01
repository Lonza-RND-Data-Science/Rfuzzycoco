# wraps the RandomGenerator class as exposed by Rcpp in R

rng <- function(seed) {
  new(RandomGenerator, 345)
}
