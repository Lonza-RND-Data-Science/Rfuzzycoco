
#ifndef MISC_IO_H
#define MISC_IO_H

#include <Rcpp.h>

namespace Rcpp {

  using std::string;

  inline std::vector<string> from_char_vector(const CharacterVector& v) {
    std::vector<string> v2(v.size());
    std::copy(v.cbegin(), v.cend(), v2.begin());
    return v2;
  };

  inline CharacterVector to_char_vector(const std::vector<string>& v) {
    CharacterVector v2(v.size());
    std::copy(v.cbegin(), v.cend(), v2.begin());
    return v2;
  };

  inline std::vector<double> from_double_vector(const NumericVector& v) {
    std::vector<double> v2(v.size());
    std::copy(v.cbegin(), v.cend(), v2.begin());
    return v2;
  };

  inline NumericVector to_double_vector(const std::vector<double>& v) {
    NumericVector v2(v.size());
    std::copy(v.cbegin(), v.cend(), v2.begin());
    return v2;
  };

}

#endif// MISC_IO_H