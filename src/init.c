#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP do_mchoice_match(SEXP, SEXP, SEXP);
extern SEXP do_nstr(SEXP, SEXP);

/* .Fortran calls */
extern void F77_NAME(cidxcn)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void F77_NAME(cidxcp)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void F77_NAME(hoeffd)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void F77_NAME(jacklins)(void *, void *, void *, void *, void *);
extern void F77_NAME(largrec)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void F77_NAME(maxempr)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void F77_NAME(rcorr)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
/* extern void F77_NAME(wcidxy)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *); */
extern void F77_NAME(wclosepw)(void *, void *, void *, void *, void *, void *, void *, void *);
extern void F77_NAME(wclosest)(void *, void *, void *, void *, void *);
extern void F77_NAME(hlqest)(void *, void *, void *, void *);
extern void F77_NAME(cutgn)(void *, void *, void *, void *);

static const R_CallMethodDef CallEntries[] = {
    {"do_mchoice_match", (DL_FUNC) &do_mchoice_match, 3},
    {"do_nstr",          (DL_FUNC) &do_nstr,          2},
    {NULL, NULL, 0}
};

static const R_FortranMethodDef FortranEntries[] = {
    {"cidxcn",   (DL_FUNC) &F77_NAME(cidxcn),   11},
    {"cidxcp",   (DL_FUNC) &F77_NAME(cidxcp),   17},
    {"hoeffd",   (DL_FUNC) &F77_NAME(hoeffd),   12},
    {"jacklins", (DL_FUNC) &F77_NAME(jacklins),  5},
    {"largrec",  (DL_FUNC) &F77_NAME(largrec),  11},
    {"maxempr",  (DL_FUNC) &F77_NAME(maxempr),  10},
    {"rcorr",    (DL_FUNC) &F77_NAME(rcorr),    12},
/*    {"wcidxy",   (DL_FUNC) &F77_NAME(wcidxy),   11}, */
    {"wclosepw", (DL_FUNC) &F77_NAME(wclosepw),  8},
    {"wclosest", (DL_FUNC) &F77_NAME(wclosest),  5},
    {"hlqest",   (DL_FUNC) &F77_NAME(hlqest),    4},
    {"cutgn",    (DL_FUNC) &F77_NAME(cutgn),    4},
    {NULL, NULL, 0}
};

void R_init_Hmisc(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, FortranEntries, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
