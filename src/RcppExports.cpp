// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// setDebug
void setDebug(bool debug_lvl);
RcppExport SEXP _STASNet_setDebug(SEXP debug_lvlSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< bool >::type debug_lvl(debug_lvlSEXP);
    setDebug(debug_lvl);
    return R_NilValue;
END_RCPP
}

RcppExport SEXP _rcpp_module_boot_ModelStructureEx();
RcppExport SEXP _rcpp_module_boot_ModelEx();
RcppExport SEXP _rcpp_module_boot_ExperimentalDesignEx();
RcppExport SEXP _rcpp_module_boot_DataEx();

static const R_CallMethodDef CallEntries[] = {
    {"_STASNet_setDebug", (DL_FUNC) &_STASNet_setDebug, 1},
    {"_rcpp_module_boot_ModelStructureEx", (DL_FUNC) &_rcpp_module_boot_ModelStructureEx, 0},
    {"_rcpp_module_boot_ModelEx", (DL_FUNC) &_rcpp_module_boot_ModelEx, 0},
    {"_rcpp_module_boot_ExperimentalDesignEx", (DL_FUNC) &_rcpp_module_boot_ExperimentalDesignEx, 0},
    {"_rcpp_module_boot_DataEx", (DL_FUNC) &_rcpp_module_boot_DataEx, 0},
    {NULL, NULL, 0}
};

RcppExport void R_init_STASNet(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
