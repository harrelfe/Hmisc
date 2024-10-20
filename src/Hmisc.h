#ifndef _HMISC_H_
#define _HMISC_H_

#include <string.h>
#include <errno.h>
#include <R.h>
#include <Rdefines.h>
#include "R_ext/Error.h"

#ifdef __cplusplus
#define EXTERNC extern "C"
#else
#define EXTERNC
#endif

#undef EXTERNC

#ifdef _SPLUS_
#  define STRING_ELT(x,i) (CHARACTER_POINTER(x)[i])
#  define TO_CHAR(x) (x)
#  define translateChar(x) (x)
#  define IS_NA_LGL(x) (is_na(&x, LGL))
#  define SET_NA_LGL(x) (na_set(&x, LGL))
   typedef s_object *SEXP ;
   typedef char *STR_ELT;
#else
#  define TO_CHAR(x) (CHAR(x))
#  define STR_ELT SEXP   
#  define IS_NA_LGL(x) (x == NA_LOGICAL)
#  define SET_NA_LGL(x) (x = NA_LOGICAL)
#endif

#define MAXELTSIZE 8192

typedef struct 
{
     char *data;
     size_t bufsize;
     size_t defaultSize;
} Hmisc_StringBuffer;

char *Hmisc_AllocStringBuffer(size_t blen, Hmisc_StringBuffer *buf);

void Hmisc_FreeStringBuffer(Hmisc_StringBuffer *buf);

#endif
