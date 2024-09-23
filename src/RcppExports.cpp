// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// convex_hull_cpp
NumericMatrix convex_hull_cpp(NumericMatrix points);
RcppExport SEXP _baselinedetection_convex_hull_cpp(SEXP pointsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type points(pointsSEXP);
    rcpp_result_gen = Rcpp::wrap(convex_hull_cpp(points));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_baselinedetection_convex_hull_cpp", (DL_FUNC) &_baselinedetection_convex_hull_cpp, 1},
    {NULL, NULL, 0}
};

RcppExport void R_init_baselinedetection(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}