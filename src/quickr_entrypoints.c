#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>


extern void dct_q(
  const double* const y__,
  const int* const kk__,
  const int* const jj__,
  const int* const forward__,
  double* const coefs__,
  const R_len_t y__len_);

SEXP dct_q_(SEXP _args) {
  // y
  _args = CDR(_args);
  SEXP y = CAR(_args);
  if (TYPEOF(y) != REALSXP) {
    Rf_error("typeof(y) must be 'double', not '%s'", R_typeToChar(y));
  }
  const double* const y__ = REAL(y);
  const R_xlen_t y__len_ = Rf_xlength(y);

  // kk
  _args = CDR(_args);
  SEXP kk = CAR(_args);
  if (TYPEOF(kk) != INTSXP) {
    Rf_error("typeof(kk) must be 'integer', not '%s'", R_typeToChar(kk));
  }
  const int* const kk__ = INTEGER(kk);
  const R_xlen_t kk__len_ = Rf_xlength(kk);

  // jj
  _args = CDR(_args);
  SEXP jj = CAR(_args);
  if (TYPEOF(jj) != INTSXP) {
    Rf_error("typeof(jj) must be 'integer', not '%s'", R_typeToChar(jj));
  }
  const int* const jj__ = INTEGER(jj);
  const R_xlen_t jj__len_ = Rf_xlength(jj);

  // forward
  _args = CDR(_args);
  SEXP forward = CAR(_args);
  if (TYPEOF(forward) != LGLSXP) {
    Rf_error("typeof(forward) must be 'logical', not '%s'", R_typeToChar(forward));
  }
  const int* const forward__ = LOGICAL(forward);
  const R_xlen_t forward__len_ = Rf_xlength(forward);

  if (kk__len_ != 1)
    Rf_error("length(kk) must be 1, not %0.f",
              (double)kk__len_);
  if (jj__len_ != 1)
    Rf_error("length(jj) must be 1, not %0.f",
              (double)jj__len_);
  if (forward__len_ != 1)
    Rf_error("length(forward) must be 1, not %0.f",
              (double)forward__len_);
  const R_xlen_t coefs__len_ = Rf_asInteger(kk);
  SEXP coefs = PROTECT(Rf_allocVector(REALSXP, coefs__len_));
  double* coefs__ = REAL(coefs);

  dct_q(
    y__,
    kk__,
    jj__,
    forward__,
    coefs__,
    y__len_);

  UNPROTECT(1);
  return coefs;
}

static const R_ExternalMethodDef QuickrEntries[] = {
  {"dct_q_", (DL_FUNC) &dct_q_, -1}
};

#include <R_ext/Rdynload.h>

void R_init_tidynorm_quick_functions(DllInfo *dll) {
  R_registerRoutines(dll, NULL, NULL, NULL, QuickrEntries);
}
