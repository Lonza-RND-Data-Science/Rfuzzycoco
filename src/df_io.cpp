// convert to/from FuzzyCoco::Dataframe to R dataframes
// cf https://gallery.rcpp.org/articles/custom-templated-wrap-and-as-for-seamingless-interfaces/

// -------------- Stage 1: Forward Declarations with `RcppCommon.h`
#include <RcppCommon.h>
#include "rcpp_fuzzy_coco.h"



// convert a R data frame to Fuzzycoco then to Rcpp then back to R
// [[Rcpp::export]]
Rcpp::DataFrame df_convert(Rcpp::DataFrame df) {
  fuzzy_coco::DataFrame fdf = Rcpp::as<fuzzy_coco::DataFrame>(df);
  // Rcpp::Rcout << fdf;
  return fdf;
}

