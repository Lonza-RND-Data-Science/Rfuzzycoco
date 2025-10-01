#include "rcpp_fuzzy_coco.h"

// convert a R list to Fuzzycoco NamedList then to Rcpp then back to R
// [[Rcpp::export]]
Rcpp::List lst_convert(Rcpp::List lst) {

  fuzzy_coco::NamedList flst = Rcpp::as<fuzzy_coco::NamedList>(lst);
  // Rcpp::Rcerr << flst;
  return Rcpp::wrap(flst);
}

