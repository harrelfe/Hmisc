#include <R.h>
#include <Rinternals.h>

SEXP string_box(SEXP string) 
{
   int i,j;
   int num_string = LENGTH(string);
   SEXP ans;
   SEXP names;
   SEXP height;
   SEXP width;
   
   PROTECT(ans = allocVector(VECSXP, 2));
   SET_VECTOR_ELT(ans, 0, height = allocVector(INTSXP, num_string));
   SET_VECTOR_ELT(ans, 1, width = allocVector(INTSXP, num_string));
   setAttrib(ans, R_NamesSymbol, names = allocVector(STRSXP, 2));
   
   SET_STRING_ELT(names, 0, mkChar("rows"));
   SET_STRING_ELT(names, 1, mkChar("columns"));
   
   
   for(i=0; i < num_string; i++) 
   {
      int str_width = 0;
      int str_subwidth = 0;
      int str_height= 0;

      const char *substring = CHAR(STRING_ELT(string, i));
      
      j = 0;
      
      while(substring[j] != '\0')
      {
         if(substring[j] == '\n')
         {
            if(str_subwidth > str_width)
               str_width = str_subwidth;
            
            str_subwidth = 0;
            str_height++;
         }
         else
            str_subwidth++;
         j++;
      }
      
      if(j > 0)
         str_height++;
      
      if(str_subwidth > str_width)
         INTEGER(width)[i] = str_subwidth;
      else
         INTEGER(width)[i] = str_width;
      
      INTEGER(height)[i] = str_height;
   }
   
      
   UNPROTECT(1);
   return(ans);
}
