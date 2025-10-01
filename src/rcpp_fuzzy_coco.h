#ifndef RCPP_FUZZY_COCO_HPP
#define RCPP_FUZZY_COCO_HPP

#include <RcppCommon.h>

#include "dataframe.h"
#include "named_list.h"



// Provide Forward Declarations
namespace Rcpp {
  namespace traits{
      // Support for as<T>
      template <>  class Exporter< fuzzy_coco::DataFrame >;
       template <> class Exporter< fuzzy_coco::NamedList >;
  }
}

// -------------- Stage 2: Including Rcpp.h
// ------ Place <Rcpp.h> AFTER the Forward Declaration!!!!
#include <Rcpp.h>
#include "misc_io.h"

// AUTOMATIC CONVERSION FROM NamedList to Rcpp::List
// Define template specializations for as<> and wrap
namespace Rcpp {

  using namespace fuzzy_coco;

  inline Rcpp::List wrap_list(const fuzzy_coco::NamedList& lst) {
    const int nb = lst.size();
    Rcpp::List rcpp_lst(nb);
    for (int i = 0; i < nb; i++) {
      const auto& elt = lst[i];
      if (elt.is_scalar()) {
        const auto& scalar = elt.scalar_check();
        if (scalar.is_bool()) {
          rcpp_lst(i) = (bool)scalar.get_bool();
        } else if (scalar.is_int()) {
          int v = scalar.get_int();
          if (v == MISSING_DATA_INT) v = NA_INTEGER;
          rcpp_lst(i) = v;
        } else if (scalar.is_double()) {
          double  d = scalar.get_double();
          if (d == MISSING_DATA_DOUBLE) d = NA_REAL;
          rcpp_lst(i) = d;
        } else if (scalar.is_string()) {
          rcpp_lst(i) = scalar.get_string();
        } 
        // this can't happen since there's is no way to add a null scalar to a NamedList
        // else if (scalar.is_null()) {
        //   rcpp_lst(i) = R_NilValue;
        // } else {
        //   throw(Rcpp::exception("unknown scalar type: should not happen"));
        // }
      } else { // list
         rcpp_lst(i) = wrap_list(elt);
      }
    }

    const auto& lst_names = lst.names();
    if (!all_of(lst_names.begin(), lst_names.end(), [](auto x) { return x.empty(); })) {
      rcpp_lst.names() = lst.names();
    }
    
    return rcpp_lst;
  }

  // AUTOMATIC CONVERSION FROM Rcpp::List to NamedList
  // N.B: vector elements are not supported!
  // Defined wrap case
  template <>
  inline SEXP wrap(const fuzzy_coco::NamedList& lst){
    return wrap_list(lst);
  };


  // Defined wrap case
  template <>
  inline SEXP wrap(const fuzzy_coco::DataFrame& df){
    List cols;
    int nb = df.nbcols();
    for (int j = 0; j < nb; j++) {
      // NumericVector col = to_double_vector(df[j]);
      cols[df.colnames()[j]] = to_double_vector(df[j]);
    }

    // Create a new dataframe
    Rcpp::DataFrame DF(cols);
    if (df.rownames().size() > 0)
      DF.attr("row.names") = to_char_vector(df.rownames()); 

    return DF;
  };

  namespace traits{

    // Defined as< > case
    template <> class Exporter< fuzzy_coco::NamedList > {
    private:
      Rcpp::List _rcpp_lst;

    public:
      Exporter(SEXP x) : _rcpp_lst(x)  { }
      // convert a  Rcpp::List to a NamedList
      fuzzy_coco::NamedList get() {
          fuzzy_coco::NamedList lst;
          const int nb = _rcpp_lst.size();
          std::vector<string> lst_names(nb);
          if (_rcpp_lst.hasAttribute("names")) {
            Rcpp::CharacterVector ns = _rcpp_lst.names();
            // Rcerr <<  "ns.size()="<< ns.size() << std::endl;
            lst_names = from_char_vector(ns);
          } else {
            // for (int i = 0; i < nb; i++)
            //   lst_names[i] = std::to_string(i + 1);
          }
          const auto& error = std::invalid_argument("vectors (size > 1) are not supported");

          for (int i = 0; i < nb; i++) {
            SEXP elem = _rcpp_lst[i];
            switch (TYPEOF(elem)) {
              case INTSXP:
              {
                IntegerVector iv = as<IntegerVector>(elem);
                if (iv.size() > 1) throw error;
                int value = IntegerVector::is_na(iv(0)) ? MISSING_DATA_INT : iv(0);
                lst.add(lst_names[i], value);
                break;
              }
              case REALSXP:
              {
                NumericVector nv = as<NumericVector>(elem);
                if (nv.size() > 1) throw error;
                double value = NumericVector::is_na(nv(0)) ? MISSING_DATA_DOUBLE : nv(0);
                lst.add(lst_names[i], value);
                break;
              }
              case STRSXP: 
              {
                CharacterVector cv = as<CharacterVector>(elem);
                if (cv.size() > 1) throw error;
                std::string str(cv(0));
                if (CharacterVector::is_na(cv(0))) throw std::invalid_argument("NA_character_ not supported by NamedList");

                lst.add(lst_names[i], str);
                break;
              }
              case LGLSXP:
              {
                LogicalVector lv = as<LogicalVector>(elem);
                if (lv.size() > 1) throw error;
                int b = lv(0);
                if (LogicalVector::is_na(b)) throw std::invalid_argument("NA (logical) not supported by NamedList");
                lst.add(lst_names[i],(bool)b);
                break;
              }
              case VECSXP:
              {
                List sublst = as<List>(elem);
                lst.add(lst_names[i], as<NamedList>(sublst));
                break;
              }
              case NILSXP:
                 throw std::invalid_argument("NULL not supported yet for NamedList");
                break;

              default:
                throw std::invalid_argument("Unsupported type at index " + std::to_string(i));
            }

          }



          return lst;
      }
    };

       // Defined as< > case
    template <> class Exporter< fuzzy_coco::DataFrame > {
    private:
      Rcpp::DataFrame _rcpp_df;

    public:
      Exporter(SEXP x) : _rcpp_df(x)
      {
          // if (TYPEOF(x) != RTYPE)
          //     throw std::invalid_argument("Wrong R type for mapped 1D array");
      }
      fuzzy_coco::DataFrame get() {
          fuzzy_coco::DataFrame df(_rcpp_df.nrow(), _rcpp_df.ncol());
          Rcpp::CharacterVector ns = _rcpp_df.names();
          std::vector<string> colnames = from_char_vector(ns);
          df.colnames(colnames);
          
          Rcpp::CharacterVector rns = _rcpp_df.attr("row.names");
          std::vector<string> rownames = from_char_vector(rns);
          df.rownames(rownames);

          for (int j = 0; j < _rcpp_df.ncol(); j++) {
            switch( TYPEOF(_rcpp_df[j]) ) {
              case REALSXP: {
                const NumericVector& col = _rcpp_df[j];
                for (int i = 0; i < col.size(); i++) df.set(i, j, col[i]);
                break;
              }
              case INTSXP:  {
                  const IntegerVector& col = _rcpp_df[j];
                  for (int i = 0; i < col.size(); i++) df.set(i, j, col[i]);
                  break;
                }
              default: {
                  stop("incompatible dataframe colum: only numeric columns are allowed");
              }
            }
          }

          return df;
      }
    };


  }
}


#endif
