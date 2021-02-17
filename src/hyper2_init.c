/* RH:  created by the following R session:

> getwd()
[1] "/home/rhankin/rstudio/hyper2"
> library(tools)
 tools::package_native_routine_registration_skeleton("." , character_only=FALSE)

but also edited below (see end)

*/


#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .Call calls */

extern SEXP _hyper2_accessor(SEXP, SEXP, SEXP);
extern SEXP _hyper2_addL(SEXP, SEXP, SEXP, SEXP);
extern SEXP _hyper2_assigner(SEXP, SEXP, SEXP, SEXP);
extern SEXP _hyper2_differentiate(SEXP, SEXP, SEXP, SEXP);
extern SEXP _hyper2_differentiate_n(SEXP, SEXP, SEXP, SEXP);
extern SEXP _hyper2_equality(SEXP, SEXP, SEXP, SEXP);
extern SEXP _hyper2_evaluate(SEXP, SEXP, SEXP);
extern SEXP _hyper2_hessian_lowlevel(SEXP, SEXP, SEXP, SEXP);
extern SEXP _hyper2_identityL(SEXP, SEXP);
extern SEXP _hyper2_overwrite(SEXP, SEXP, SEXP, SEXP);

/*

Following lines commented out because they are duplicated in
RcppExports.cpp (which is created by compileAttributes())


static const R_CallMethodDef CallEntries[] = {
    {"_hyper2_accessor",         (DL_FUNC) &_hyper2_accessor,         3},
    {"_hyper2_addL",             (DL_FUNC) &_hyper2_addL,             4},
    {"_hyper2_assigner",         (DL_FUNC) &_hyper2_assigner,         4},
    {"_hyper2_differentiate",    (DL_FUNC) &_hyper2_differentiate,    4},
    {"_hyper2_differentiate_n",  (DL_FUNC) &_hyper2_differentiate_n,  4},
    {"_hyper2_equality",         (DL_FUNC) &_hyper2_equality,         4},
    {"_hyper2_evaluate",         (DL_FUNC) &_hyper2_evaluate,         3},
    {"_hyper2_hessian_lowlevel", (DL_FUNC) &_hyper2_hessian_lowlevel, 4},
    {"_hyper2_identityL",        (DL_FUNC) &_hyper2_identityL,        2},
    {"_hyper2_overwrite",        (DL_FUNC) &_hyper2_overwrite,        4},
    {NULL, NULL, 0}
};

void R_init_hyper2(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
*/
