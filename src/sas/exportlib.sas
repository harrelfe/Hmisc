/* Macro exportlib

    Exports all SAS datasets in a data library to csv files.  One of
    the datasets is assumed to be the result of PROC FORMAT CNTLOUT=
    if any user formats are referenced.  Numeric variables are
    formatted in BEST16 format so that date/time variables will be
    exported with their internal numeric values.  A special file
    _contents_.csv is created to hold, for all datasets combined, the
    dataset name, dataset label, variable names, labels, formats,
    types, and lengths.

    Usage:

    %INCLUDE "foo\exportlib.sas";    * Define macro;
    LIBNAME lib ...;                 * E.g. LIBNAME d SASV5XPT "foo.xpt";
    %exportlib(lib, outdir, tempdir);
    
    Arguments:
        lib     - SAS libname for input datasets
        outdir  - directory in which to write .csv files (default ".")
        tempdir - temporary directory to hold generated SAS code
                  (default C:/WINDOWS/TEMP)
                                                                             */
%macro exportlib(lib,  outdir, tempdir);
%IF %QUOTE(&outdir)=   %THEN %LET outdir=.;
%IF %QUOTE(&tempdir)=  %THEN %LET tempdir=C:/WINDOWS/TEMP;
OPTIONS NOFMTERR;
PROC COPY IN=&lib OUT=work;RUN;
PROC CONTENTS DATA=work._ALL_ NOPRINT
    OUT=_contents_(KEEP=memname memlabel name type label format length
                        nobs);RUN;
PROC EXPORT DATA=_contents_ OUTFILE="&outdir/_contents_.csv" REPLACE;RUN;
DATA _NULL_; SET _contents_; BY MEMNAME;
    FILE "&tempdir/_export_.sas"; RETAIN bk -1;
    if FIRST.MEMNAME & (NOBS > 0) THEN DO;
        PUT "DATA " MEMNAME "; SET " MEMNAME ";FORMAT _NUMERIC_ BEST14.;RUN;";
        PUT "PROC EXPORT DATA=" MEMNAME " OUTFILE=" '"' "&outdir/" 
            MEMNAME +bk ".csv" '" ' "REPLACE;RUN;";
        END;
    RUN;
%INCLUDE "&tempdir/_export_.sas";RUN;
%MEND exportlib;
