.onLoad <- function(libname, pkgname) {
  # Rcpp::loadModule(module = "RcppCoevGeneration", TRUE)
  Rcpp::loadModule(module = "RcppFuzzyCocoWrapper", TRUE)
}