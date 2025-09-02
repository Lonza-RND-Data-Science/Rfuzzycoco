.onLoad <- function(libname, pkgname) {
  Rcpp::loadModule(module = "RcppCoevGeneration", TRUE)
  Rcpp::loadModule(module = "RcppFuzzyCocoWrapper", TRUE)
  Rcpp::loadModule(module = "RcppFuzzySystem", TRUE)
}