Hmisc
=====

Harrell Miscellaneous

Current Goals
=============
* Continue to refine the summaryX class of functions that replace tables with graphics
   * See also bpplotM and tabulr
* See https://hbiostat.org/R/Hmisc/summaryFuns.pdf

Web Sites
=============
* Overall: https://hbiostat.org/R/Hmisc
* CRAN: http://cran.r-project.org/web/packages/Hmisc
* Changelog: https://github.com/harrelfe/Hmisc/commits/master

To Do
=====
* Consider using the haven package for importing SAS, Stata, and SPSS files; haven stores labels as the label attribute of each variable as does Hmisc; it converts date and time variables automatically and allows one to specify a format catalog along with the primary dataset
* See if the readstata13 package has advantages over the foreign package for Stata file import
* Consider creating xl.get using the readxl package to read .xls and .xlsx Excel files
* In impute.transcan, sense if a variable in data is not a factor whereas it was treated as a factor during aregImpute; it should be converted to factor before the line v[sub] <- ... levels(as.integer...)) is run

