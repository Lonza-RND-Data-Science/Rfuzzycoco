


#include "rcpp_fuzzy_coco.h"
#include "fuzzy_coco.h"
#include "logging_logger.h"
#include <Rcpp.h>
using namespace Rcpp;

//' FuzzyCoco::eval()
//' @return a data.frame
//' @export
// [[Rcpp::export]]
List rcpp_fuzzy_coco_eval(Rcpp::DataFrame df, List fuzzy_system_desc, List params, bool verbose = false) {
  logging::logger().activate(verbose);

  fuzzy_coco::DataFrame _df = as<fuzzy_coco::DataFrame>(df);
  fuzzy_coco::NamedList _fs_desc = as<fuzzy_coco::NamedList>(fuzzy_system_desc);

  FuzzySystem _fs = FuzzySystem::load(_fs_desc);

  NamedList params_desc = as<NamedList>(params);
  FuzzyCocoParams _params(params_desc);

  fuzzy_coco::NamedList res = FuzzyCoco::eval(_df, _fs, _params);

  return Rcpp::wrap(res);
}

//' FuzzyCoco::loadAndPredict()
//' @return a data.frame
//' @export
// [[Rcpp::export]]
Rcpp::DataFrame rcpp_fuzzy_coco_predict(Rcpp::DataFrame df, List fuzzy_system_desc, bool verbose = false) {
  logging::logger().activate(verbose);

  fuzzy_coco::DataFrame _df = as<fuzzy_coco::DataFrame>(df);
  fuzzy_coco::NamedList _fs = as<fuzzy_coco::NamedList>(fuzzy_system_desc);

  FuzzySystem fs = FuzzySystem::load(_fs);

  fuzzy_coco::DataFrame res = fs.smartPredict(_df);
  // Rcerr << res;

  return Rcpp::wrap(res);
}


//' FuzzyCoco::searchBestFuzzySystem()
//' @return a list
//' @export
// [[Rcpp::export]]
List rcpp_fuzzy_coco_searchBestFuzzySystem(Rcpp::DataFrame df, int nb_out_vars, List params, int seed = 123, bool verbose = false) {
  fuzzy_coco::DataFrame _df = as<fuzzy_coco::DataFrame>(df);

  // Rcerr << _df;
  logging::logger().activate(verbose);
  NamedList params_desc = as<NamedList>(params);
  // Rcerr << "params_desc = "<< params_desc;
  FuzzyCocoParams _params(params_desc);
  _params.evaluate_missing(_df.nbcols() - nb_out_vars, nb_out_vars);


  if (_params.has_missing()) throw std::invalid_argument("some parameters are missing");
  
  auto res = FuzzyCoco::searchBestFuzzySystem(_df, nb_out_vars, _params, seed);

  return Rcpp::wrap(res);
}
