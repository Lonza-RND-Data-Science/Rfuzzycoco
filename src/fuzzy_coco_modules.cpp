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
class FuzzyCocoWrapper {
public:
  // FuzzyCocoWrapper(bool verbose = false) {
  //    logger().activate(verbose);
  //    logger() << "FuzzyCocoWrapper(bool verbose = false)\n";
  // }
  FuzzyCocoWrapper(const Rcpp::DataFrame& df, int nb_out_vars, List params, int seed, bool verbose = false)
    : _rng(seed)
  {
    logger().activate(verbose);
    logger() << "FuzzyCocoWrapper() \n";

    fuzzy_coco::DataFrame _df = as<fuzzy_coco::DataFrame>(df);

    FuzzyCoco::split_dataset(_df, nb_out_vars, _dfin, _dfout);

    NamedList params_desc = as<NamedList>(params);
    _params_ptr = make_unique<FuzzyCocoParams>(params_desc);
    _params_ptr->evaluate_missing(_dfin.nbcols(), _dfout.nbcols());
    if (_params_ptr->has_missing()) throw std::invalid_argument("some parameters are missing");

    _coco_ptr = make_unique<FuzzyCoco>(_dfin, _dfout, *_params_ptr, _rng);
  }

  CoevGeneration* getCurrentGeneration() { return new CoevGeneration(_current_gen); }
  double getCurrentFitness() const { return _current_gen.fitness; }
  int getCurrentGenerationNb() const { return _current_gen.generation_number; }

  NamedList describeCurrentGeneration() {
    if (!_started) throw std::runtime_error("not started!");
    NamedList desc;
    desc.add("fitness", _current_gen.fitness);
    desc.add("generation_number", _current_gen.generation_number);
    desc.add("rules_population", describeCurrentGenerationRules());
    desc.add("mfs_population", describeCurrentGenerationMFs());
    return desc;
  }

  NamedList describeCurrentGenerationRules() {
    if (!_started) throw std::runtime_error("not started!");
    NamedList desc;
    desc.add("individuals", describeRules(_current_gen.left_gen.individuals));
    desc.add("elite", describeRules(_current_gen.left_gen.elite));
    return desc;
  }

  NamedList describeCurrentGenerationMFs() {
    if (!_started) throw std::runtime_error("not started!");

    NamedList desc;
    desc.add("individuals", describeMFs(_current_gen.right_gen.individuals));
    desc.add("elite", describeMFs(_current_gen.right_gen.elite));
    return desc;
  }

  void start() {
    if (_started) throw std::runtime_error("already started!");

    _current_gen = getFuzzyCoco().start(
      getRng(), 
      getParams().global_params.influence_rules_initial_population, 
      getParams().global_params.influence_evolving_ratio);

    _started = true;
  }

  double next_gen() {
    if (!_started) throw std::runtime_error("not started!");
    _current_gen = getFuzzyCoco().getEngine().next(_current_gen);
    return _current_gen.fitness;
  }

  NamedList describeBestSystem() {
    if (!_started) throw std::runtime_error("not started!");
    getFuzzyCoco().selectBestFuzzySystem();
    return getFuzzyCoco().describe(getCurrentGenerationNb());
  }

  void print() {
    Rcerr << _dfin << _dfout;
    if (_params_ptr) Rcerr << *_params_ptr;
    if (_coco_ptr) Rcerr << *_coco_ptr;
  }

protected:

  NamedList describeMFs(const Genomes& mfs_genomes) {
    if (!_started) throw std::runtime_error("not started!");
    auto& codec = getFuzzyCoco().getEngine().getFuzzyCocoCodec();
    auto& fs = getFuzzyCoco().getFuzzySystem();
    NamedList desc;
    for (size_t i = 0; i < mfs_genomes.size(); i++) {
      const auto& mfs_gen = mfs_genomes[i];
      codec.setMFsGenome(fs, mfs_gen);
      desc.add(string("genome ") + to_string(i+1), fs.getDB().describe());
    }

    return desc;
  }

  NamedList describeRules(const Genomes& rules_genomes) {
    if (!_started) throw std::runtime_error("not started!");
    auto& codec = getFuzzyCoco().getEngine().getFuzzyCocoCodec();
    auto& fs = getFuzzyCoco().getFuzzySystem();
    NamedList desc;
    for (size_t i = 0; i < rules_genomes.size(); i++) {
      const auto& rule_gen = rules_genomes[i];
      codec.setRulesGenome(fs, rule_gen);
      NamedList gen;
      gen.add("rules",  FuzzyRule::describeRules(fs.getRules(), false));
      gen.add("default_rules", FuzzyDefaultRule::describeDefaultRules(fs.fetchDefaultRules()));
      desc.add(string("genome ") + to_string(i+1), gen);
    }

    return desc;
  }

  FuzzyCoco& getFuzzyCoco() { return *_coco_ptr; }
  RandomGenerator& getRng() { return _rng; }
  FuzzyCocoParams& getParams() { return *_params_ptr;}

private:
  fuzzy_coco::DataFrame _dfin, _dfout;
  RandomGenerator _rng;
  unique_ptr<FuzzyCocoParams> _params_ptr;
  unique_ptr<FuzzyCoco> _coco_ptr;
  CoevGeneration _current_gen;
  bool _started = false;
};



// // Expose (some of) the CoevGeneration struct
// RCPP_MODULE(RcppCoevGeneration){
//   Rcpp::class_<CoevGeneration>("CoevGeneration")
//   // .constructor<std::string, int, bool>()
//   .field("fitness", &CoevGeneration::fitness)
//   .field("generation_number", &CoevGeneration::generation_number)
//   // .method("GetName", &Student::GetName)
//   ;
// }

// Expose the FuzzyCocoWrapper class
RCPP_MODULE(RcppFuzzyCocoWrapper){
  Rcpp::class_<FuzzyCocoWrapper>("FuzzyCocoWrapper")
    // .constructor<bool>()
    .constructor<Rcpp::DataFrame, int, List, int, bool>()
    .method("start", &FuzzyCocoWrapper::start)
    .method("next_gen", &FuzzyCocoWrapper::next_gen)
    // .method("getCurrentGeneration", &FuzzyCocoWrapper::getCurrentGeneration)
    .method("getCurrentFitness", &FuzzyCocoWrapper::getCurrentFitness)
    .method("getCurrentGenerationNb", &FuzzyCocoWrapper::getCurrentGenerationNb)
    // .method("describeCurrentGenerationRules", &FuzzyCocoWrapper::describeCurrentGenerationRules)
    // .method("describeCurrentGenerationMFs", &FuzzyCocoWrapper::describeCurrentGenerationMFs)
    .method("describeCurrentGeneration", &FuzzyCocoWrapper::describeCurrentGeneration)
    .method("describeBestSystem", &FuzzyCocoWrapper::describeBestSystem)

    // .method("set_df", &FuzzyCocoWrapper::set_df)
    // .method("set_params", &FuzzyCocoWrapper::set_params)
    // .method("init_coco", &FuzzyCocoWrapper::init_coco)
    .method("print", &FuzzyCocoWrapper::print)
    // .method("set_df", static_cast<void (FuzzyCocoWrapper::*)(Rcpp::DataFrame)>(&FuzzyCocoWrapper::set_df))
  ;
}


