// expose CoevGeneration struct to R
#include "rcpp_fuzzy_coco.h"
#include <Rcpp.h>
using namespace Rcpp;

#include "random_generator.h"
#include "coevolution_engine.h"
#include "fuzzy_coco.h"
#include "logging_logger.h"
#include "fuzzy_coco_params.h"

using namespace fuzzy_coco;
using namespace fuzzy_coco::logging;
class FuzzySystemWrapper {
public:
  FuzzySystemWrapper(List fuzzy_system_desc) : 
    _fs(FuzzySystem::load(as<fuzzy_coco::NamedList>(fuzzy_system_desc)))
  {}

  Rcpp::DataFrame predict(Rcpp::DataFrame df) {
    fuzzy_coco::DataFrame fc_df_in = as<fuzzy_coco::DataFrame>(df);
    fuzzy_coco::DataFrame fc_df_out = _fs.smartPredict(fc_df_in);
    return Rcpp::wrap(fc_df_out);
  }


private:
  fuzzy_coco::FuzzySystem _fs;
};



// Expose (some of) the CoevGeneration struct
RCPP_MODULE(RcppFuzzySystem){
  Rcpp::class_<FuzzySystemWrapper>("FuzzySystemWrapper")
  .constructor<List>()
  .method("predict", &FuzzySystemWrapper::predict)
  ;
}

