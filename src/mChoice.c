/* #define USE_RINTERNALS 1 */

#include "Hmisc.h"

static Hmisc_StringBuffer cbuff = {NULL, 0, MAXELTSIZE};


int get_next_mchoice(char **s)
{
     long opt;
     int errsv;
     char *begin, *end, *err_chk;
     S_EVALUATOR
     
     begin = *s;
     
     if(begin == NULL)
          return 0;

     if(*begin == ';')
          end = begin;
     else if(*begin == '\0')
          /* begin points to end of string set end to NULL */
          end = NULL;
     else
          /* set end to the location of the the next ';' */
          end = strchr(begin + 1, ';');
     
     if(end) {
          /* set end value to null and increment */
          *end++ = '\0';
          /* set s to the begining of the next substring */
          *s = end;
     }
     else
          /* end points to the end of the string. Set *s to NULL to 
             indecate all of string consumed. */
          *s = NULL;

     /* if begin is zero length substring return 0 */
     if(strlen(begin) == 0)
          return 0;

     /* convert substring begin into its integer value */
     /* set errno to zero */
     errno = 0;
     opt = strtol(begin, &err_chk, 10);
         
     /* Check to see if an error occured in strtol */
     if(errno != 0) {
          errsv = errno;
          Rf_error("string to integer conversion error: %s", strerror(errsv));
     }
            
     if(err_chk == begin || *err_chk != '\0')
          Rf_error("string %s is not a valid integer number", begin);
     
     /* return the integer mChoice option */
     return (int)opt;
}

SEXP do_mchoice_match(SEXP x, SEXP table, SEXP nomatch) 
{
     SEXP elm_index;            /* Storage for value of first row of 
                                   first match of each element in x *\/ */
     R_len_t len;               /* Number of elements in x */
     R_len_t nfound = 0;        /* count of number of elements of
                                   x matched in table */
     char *str_ptr;             /* current location pointer */
     const char *str;
		 int i, j, comp;
     size_t slen;               /* length of string */

     S_EVALUATOR
     /* get number of elements in x */
     len = LENGTH(x);
     
     /* allocate an index vector of the same length as x */
     PROTECT(elm_index = NEW_INTEGER(len));
     
     /* set all values in elm_index to 0 */
     memset((int *)INTEGER_POINTER(elm_index), 0, len * sizeof(int));

     /* count number of x values that are zero and set nfound to that */
     for(i=0; i < len; i++) {
          if(INTEGER_POINTER(x)[i] == 0) {
               INTEGER_POINTER(elm_index)[i] = INTEGER_POINTER(nomatch)[0];
               nfound++;
          }
     }
     

     /* iterate through each element of table looking for matches to values in x.
        it is done this way because parsing the mChoice string is expensive and looping is not. */
     for(i=0; i < LENGTH(table) && nfound < len; i++) {
          if(STRING_ELT(table, i) == NA_STRING)
               continue;
          
          str = translateCharUTF8(STRING_ELT(table, i));
          slen = strlen(str) + 1;
          
          str_ptr = Hmisc_AllocStringBuffer((slen) * sizeof(char), &cbuff);
          strncpy(str_ptr, str, slen);
          str_ptr[slen] = '\0';
          
          while(str_ptr != NULL && nfound < len) {
               /* get the next component of the mChoice string */
               comp = get_next_mchoice(&str_ptr);
               
               /* if comp is zero the next component was blank continue */
               if(comp == 0)
                    continue;
                    
               /* Compare the component to all elements of x */
               for(j = 0; j < len && nfound < len; j++) {
                    /* If the element index is not zero that value has been prevously
                       matched continue to next value */
                    if(INTEGER_POINTER(elm_index)[j] || INTEGER_POINTER(x)[j] == 0)
                         continue;
                    
                    if(INTEGER_POINTER(x)[j] == comp) {
                         nfound++;
                         INTEGER_POINTER(elm_index)[j] = i+1;
                    }
               }
          }
     }
     
     Hmisc_FreeStringBuffer(&cbuff);
     
     if(nfound < len) {
          /* if not all elements of x are matched to those in table
             set the elements of elmt_index that are zero to the value 
             of nomatch */
          for(i=0; i < len; i++) {
               if(INTEGER_POINTER(elm_index)[i] == 0) {
                    INTEGER_POINTER(elm_index)[i] = INTEGER_POINTER(nomatch)[0];
               }
          }
     }

     UNPROTECT(1);
     return(elm_index);
}


SEXP do_mchoice_equals(SEXP x, SEXP y) 
{
     int x_len = LENGTH(x);     /* length of x vector */
     int y_len = LENGTH(y);     /* length of y vector */
     SEXP ans;                  /* Logical return vector */
     int nfound = 0;                /* number of matches found */
     int i,j, comp;             /* iterators */
     size_t slen;
     char *str_ptr;             /* copy of the x string element */
     const char *str;

     S_EVALUATOR

     if(!IS_INTEGER(y) || y_len == 0)
          Rf_error("y must be an integer vector of at least length one.");
   
     PROTECT(ans = NEW_LOGICAL(x_len));
     
     for(i=0; i < x_len; ++i) {
        nfound = 0;
        str = translateCharUTF8(STRING_ELT(x, i));

        slen = strlen(str) + 1;
        
        /* if length of x element is zero or NA no posible match */
        if(STRING_ELT(x, i) == NA_STRING) {
             SET_NA_LGL(LOGICAL_POINTER(ans)[i]);
             continue;
        }
        if(slen == 0) {
             LOGICAL_POINTER(ans)[i] = 0;
             continue;
        }
        
        str_ptr = Hmisc_AllocStringBuffer((slen) * sizeof(char), &cbuff);
        strncpy(str_ptr, str, slen);
        str_ptr[slen] = '\0';

        while(str_ptr != NULL && nfound < y_len) {
             comp = get_next_mchoice(&str_ptr);

             for(j=0; j < y_len; j++) {
                  if(comp == INTEGER_POINTER(y)[j]) {
                       nfound++;
                       break;
                  }
             }
        }
        
        if(nfound < y_len)
             LOGICAL_POINTER(ans)[i] = 0;
        else
             LOGICAL_POINTER(ans)[i] = 1;
     }
     
     Hmisc_FreeStringBuffer(&cbuff);
     UNPROTECT(1);
     return(ans);
}

