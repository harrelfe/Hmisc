#include "Hmisc.h"

static Hmisc_StringBuffer cbuff = {NULL, 0, MAXELTSIZE};


SEXP do_nstr(SEXP s, SEXP n)
{
   SEXP ans;                    /* the returned character string */
   int i, j;                    /* the length of the string and number of reps */
   int s_counter = 0, n_counter = 0;
   int longest, s_length, n_length;
   
   S_EVALUATOR

   n_length = length(n);
   s_length = length(s);
   
   longest = n_length < s_length ? s_length : n_length;
   
   if(n_length == 1 && INTEGER(n)[0] == 1)
      return s;
   
   PROTECT(ans = allocVector(STRSXP, longest));

   for(i=0; i < longest; i++) 
   {
      int n_reps = INTEGER(n)[n_counter];
      
      if(n_reps < 1)
      {
         SET_STRING_ELT(ans, i, mkChar(""));
      }
      else if(n_reps == 1)
      {
         SET_STRING_ELT(ans, i, duplicate(STRING_ELT(s, s_counter)));
      }
      else
      {
         char *cbuf, *buf;
         const char *seg;
         size_t seg_len;

         seg = CHAR(STRING_ELT(s, s_counter));
         seg_len = strlen(seg);
         cbuf = buf = Hmisc_AllocStringBuffer((n_reps * seg_len + 1) * sizeof(char), &cbuff);
   
         for(j=0; j < n_reps; ++j)
         {
            strcpy(buf, seg);
            buf += seg_len;
         }
         *buf = '\0';
   
         SET_STRING_ELT(ans, i, mkChar(cbuf));
      }

      n_counter = (++n_counter < n_length) ? n_counter : 0;
      s_counter = (++s_counter < s_length) ? s_counter : 0;
   }

   Hmisc_FreeStringBuffer(&cbuff);
   
   UNPROTECT(1);
   return ans;
}
