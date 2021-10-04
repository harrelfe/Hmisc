#include "Hmisc.h"

char *Hmisc_AllocStringBuffer(size_t blen, Hmisc_StringBuffer *buf)
{
    size_t blen1, bsize = buf->defaultSize;
    S_EVALUATOR

    if(blen * sizeof(char) < buf->bufsize) return buf->data;
    blen1 = blen = (blen + 1) * sizeof(char);
    blen = (blen / bsize) * bsize;
    if(blen < blen1) blen += bsize;

    if(buf->data == NULL) {
        buf->data = (char *) malloc(blen);
        buf->data[0] = '\0';
    } else
        buf->data = (char *) realloc(buf->data, blen);
    buf->bufsize = blen;
    if(!buf->data) {
        buf->bufsize = 0;
        /* don't translate internal error message */
        Rf_error("could not allocate memory (%u Mb) in C function 'Hmisc_AllocStringBuffer'",
              (unsigned int) blen/1024/1024);
    }
    return buf->data;
}


void Hmisc_FreeStringBuffer(Hmisc_StringBuffer *buf)
{
    if (buf->data != NULL) {
        free(buf->data);
        buf->bufsize = 0;
        buf->data = NULL;
    }
}
