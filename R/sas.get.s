## $Id$
sas.get <- 
  function(libraryName,
           member,
           variables = character(0), 
           ifs = character(0), 
           format.library = libraryName,
           id, 
           dates. = c("sas","yymmdd","yearfrac","yearfrac2"), 
           keep.log = TRUE,
           log.file = "_temp_.log", 
           macro = sas.get.macro,
           data.frame.out = existsFunction("data.frame"), 
           clean.up = FALSE,
           quiet = FALSE,
           temp = tempfile("SaS"), 
           formats=TRUE,
           recode=formats, 
           special.miss=FALSE,
           sasprog="sas",
           as.is=.5,
           check.unique.id=TRUE,
           force.single=FALSE,
           pos,
           uncompress=FALSE,
           defaultencoding="latin1",
           var.case="lower")
{
  if(force.single) stop('force.single does not work under R')
  dates. <- match.arg(dates.)

  fexists <- function(name) {
    w <- file.exists(name)
    attr(w, 'which') <- name[w]
    w
  }

  file.is.dir <- function(name) {
    isdir <- file.info(name)$isdir
    isdir && !is.na(isdir)
  }

  file.is.readable <- function(name) file.access(name,4)==0

  fileShow <- function(x) file.show(x)

  if(recode) formats <- TRUE

  if(missing(formats) || formats) {
    ## *****  Next line begins mod from Mike Kattan edits 11 Sep 97
    ## Redone FEH 22Oct00
    no.format <- all(!fexists(file.path(format.library,
                                        c('formats.sc2','formats.sct','formats.sct01','formats.sas7bcat'))))
    if(no.format) {
      if((!missing(formats) && formats) || (!missing(recode) && recode))
        warning(paste(paste(format.library, 
                            "/formats.sc? or formats.sas7bcat",sep = ""), 
                      " not found. Formatting ignored. \n"))
      formats <- recode <- FALSE
    }
    ## ***** End Mike Kattan edits 11 Sep 97
  }
  
  ## 5 Changes here from Claudie Berger <claudie@osteo1.ri.mgh.mcgill.ca> 19feb00
  ## Allows work on sas v7.
  sasin   <- paste(temp, ".3.sas", sep = "")
  sasout1 <- paste(temp, ".1.sas", sep = "")
  sasout2 <- paste(temp, ".2.sas", sep = "")
  sasout3 <- paste(temp, ".4.sas", sep = "")
  sasout4 <- paste(temp, ".5.sas", sep = "")
  nvariables <- length(variables)
  if(nvariables>0) {
    if(any(jdup <- duplicated(variables)))
      stop(paste("duplicate variables requested: ", variables[jdup]))
  }
  
  varstring <- paste(variables, collapse = "\n ")
  ifs <- paste("'",paste(ifs, collapse = ";\n "),"'",sep="")
  if(length(sasin) != 1)
    stop("Illegal temporary file name")
  
  temp.files <- c(sasin, sasout1, sasout2, sasout3, sasout4)
  if(!keep.log)
    temp.files <- c(temp.files, log.file)
  
  if(clean.up)
    on.exit(unlink(temp.files))
  ##on.exit(sys(paste("rm -f", paste(temp.files, collapse = " "))))
  ##  4oct03
  
  if(missing(member))
    stop("SAS member name is required")
  
  if(missing(libraryName))
    stop("SAS library name is required")

  ## Encoding added by Reinhold Koch 24Jan14 <reinhold.koch@roche.com>
  cat("%LET DEFAULTE=", defaultencoding, ";\n", sep="", file=sasin)
  cat(macro, sep="\n", file=sasin, append=TRUE)

  sasds.suffix <- c('sd2','sd7','ssd01','ssd02','ssd03','ssd04','sas7bdat') 
  ## 22Oct00

  if(libraryName == "") libraryName <- "."
  if(!file.is.dir(libraryName))
    stop(paste(sep = "", "library, \"", libraryName, 
               "\", is not a directory"))

  unix.file <- file.path(libraryName, paste(member, sasds.suffix, sep="."))

  if(uncompress) {
    if(any(fe <- fexists(paste(unix.file,".gz",sep=""))))
      system(paste("gunzip ", attr(fe,'which'),'.gz',sep=''))
    else if(any(fe <- fexists(paste(unix.file,".Z",sep=""))))
      system(paste("uncompress ",attr(fe,'which'),'.Z',sep=''))
  }

  if(!any(fe <- fexists(unix.file))) {
    stop(paste(sep = "", "Unix file, \"",
               paste(unix.file,collapse=' '), 
               "\", does not exist"))
  } else {
    file.name <- attr(fe,'which')
    if(!file.is.readable(file.name)) {
      stop(paste(sep = "", 
                 "You do not have read permission for Unix file, \"",
                 file.name, "\""))   # 22Oct00
    }
  }
    
  cat("libname temp '", libraryName, "';\n", file = sasin, append = TRUE,
      sep = "")
  
  ## format.library should contain formats.sct containing user defined
  ## formats used by this dataset.  It must be present.
  cat("libname library '", format.library, "';\n", file = sasin,
      append = TRUE, sep = "")
  cat("%sas_get(temp.", member, ",\n",
      "  ", sasout1, ",\n",
      "  ", sasout2, ",\n",
      "  ", sasout3, ",\n",
      "  ", sasout4, ",\n",
      "  dates=", dates., ",\n",
      "  vars=",  varstring, ",\n",
      "  ifs=",   ifs, ",\n",
      "  formats=", as.integer(formats), "\n,",
      "  specmiss=", as.integer(special.miss), ");\n",
      file = sasin, append = TRUE, sep = "")

  
  status <- system(paste(shQuote(sasprog), shQuote(sasin), "-log",
                         shQuote(log.file)), intern=FALSE)
  ## 24nov03 added output=F
  if(status != 0) {
    if(!quiet && fexists(log.file)) fileShow(log.file)  ## 4oct03
    stop(paste("SAS job failed with status", status))
  }
  ##
  ## Read in the variable information
  ##
  if(!(fexists(sasout1) && fexists(sasout2))) {
    if(!quiet)
      fileShow(log.file)  ## 4oct03
    
    stop("SAS output files not found")
  }
  
  vars <-
    scan(sasout1, list(name = "", type = 0, length = 0,
                       format = "", label = "", n = 0),
         multi.line = FALSE, sep = "\022",
         flush=TRUE, comment.char='', quote='')
  ## Thanks Don MacQueen for scan fix for R
  
  nvar <- length(vars$name)
  if(nvar == 0) {
    if(!quiet)
      fileShow(log.file)  ## 4oct03
    
    stop("First SAS output is empty")
  }
  
  nrow <- vars$n[1]	#n is the same for each variable
  
  ## Read the data in
  ##  We try to be clever about the variable type.  If SAS is character
  ##  use char of course.  If is numeric and length >4, use double.  If
  ##  numeric and length <4, use single.  We could also use the format to
  ##  choose further, if it consists of a number followed by a "."
  ##  can we safely assume integer.
  ##
  type <- ifelse(vars$type == 2, "character(nrow)", 
                 ifelse(force.single,  ##28Mar01
                        "single(nrow)", "double(nrow)"))
  ##BILL: I corrected the macro so the following isn't needed:
  ## get rid of trailing blank on names
  ##	vars$name <- unix("sed 's/ $//'", vars$name)
  inlist <- paste("\"", vars$name, "\"=", type,
                  sep = "", collapse = ", ")
  
  inlist <- parse(text = paste("list(", inlist, ")"))
  ## Inlist would now be the size of the final data structure, if I had
  ## evaluated it.

  ## Read the data
  ds <-
    scan(sasout2, eval(inlist), sep = "\022", multi.line = FALSE,
         flush=TRUE, comment.char='', quote='')
  
  if(length(ds) < nvariables) {
    m <- variables[is.na(match(variables, names(ds)))]
    if(length(m) > 0) {
      warning(paste(length(m), 
                    "requested variables did not exist:", 
                    paste("\"", m, "\"", sep = "", collapse = " "), 
                    "\n\t(use sas.contents())"))
    }
  }
  
  format <- vars$format
  format[format=='$'] <- ' '    # 1Mar00
  label <- vars$label
  name <- vars$name
  esasout3 <- formats && fexists(sasout3)   #added formats && 1/20/93
  if(recode && !esasout3) recode <- FALSE
  FORMATS <- NULL

  if(formats && esasout3) {
    FORMATS <- dget(sasout3)
    if(length(FORMATS)==0) {
      FORMATS <- NULL;
      recode <- FALSE
    }	
  }
  
  smiss <- NULL
  if(special.miss && fexists(sasout4))
    smiss <-
      scan(sasout4, list(name="", code="", obs=integer(1)),
           multi.line=FALSE, flush=TRUE, sep="\022",
           comment.char='', quote='')
  
  sasdateform <- c("date","mmddyy","yymmdd","ddmmyy","yyq","monyy",
                   "julian","qtr","weekdate","weekdatx","weekday","month")
  dateform <- 	
    list(as.name("ddmmmyy"),"m/d/y","y/m/d","d/m/y",as.name("ddmmmyy"),
         "mon year",as.name("ddmmmyy"),"mon",as.name("ddmmmyy"),
         as.name("ddmmmyy"), as.name("ddmmmyy"),"m")
  
  sastimeform <- c("hhmm","hour","mmss","time")
  timeform <- c("h:m","h","m:s","h:m:s")
  sasdatetimeform <- c("datetime","tod")
  datetimeform <- list(list(as.name("ddmmmyy"),"h:m:s"), c("m/d/y"," "))
  z <- "%02d%b%Y"
  dateform4 <-
    c(z,"%02m/%02d/%Y","%Y/%02m/%02d","%02d/%02m/%Y", z,"%02m %Y",
      z,"%02m", z, z, z,"%02m")
  
  timeform4 <- c("%02H:%02M","%02H","%02M:%02S","%02H:%02M:%02S")
  datetimeform4 <- c("%02d%b%Y %02h:%02m:%02s","%02m/%02d/%Y")
  
  ## Don MacQueen
  days.to.adj <- as.numeric(difftime(ISOdate(1970,1,1,0,0,0) , 
                                     ISOdate(1960,1,1,0,0,0), 'days'))
  secs.to.adj <- days.to.adj*24*60*60
  
  for(i in 1:nvar) {
    atr <- list()
    dsi <- ds[[i]]
    fname <- format[i]
    rec <- FALSE
    if(fname!=" ") {
      ff <- fname
      if(dates.=="sas" & (m <- match(fname,sasdateform,0)) >0) {
        ##look for partial dates
        dd <- dsi-floor(dsi)
        ddn <- !is.na(dd)
        if(any(ddn) && any(dd[ddn]!=0)) {
          ll <- 1:length(dd)
          atr$partial.date <- 
            list(month=ll[dd==.5],day=ll[dd==.25],both=ll[dd==.75])
          atr$imputed <- ll[dd!=0]
          dsi <- floor(dsi)
        }
        dsi <- importConvertDateTime(dsi, 'date', 'sas',
                                     form=dateform[m])
        
        if(length(atr$imputed)) 
          attr(dsi,'class') <- c("impute",attr(dsi,'class'))
        
        ff <- NULL
      } else {
        if((m <- match(fname,sastimeform,0)) >0) {
          dsi <- importConvertDateTime(dsi, 'time', 'sas', 
                                       form=timeform[m])
          ff <- NULL			
        } else if((m <- match(fname,sasdatetimeform,0))>0) {
          dsi <- importConvertDateTime(dsi, 'datetime', 'sas',
                                       form=datetimeform[m])
          
          ff <- NULL					
        }
      }

      atr$format <- ff
      if(recode & length(g <- FORMATS[[fname]])) {
        labs <- g$labels
        if(!is.logical(recode)) {
          labs <- if(recode==1) paste(g$values,":",labs,sep="")
                  else paste(labs,"(",g$values,")",sep="")
        }

	dsi <- factor(dsi, g$values, labs)
        atr$sas.codes <- g$values
        rec <- TRUE
      }   
    }

    if(data.frame.out && !rec && vars$type[i]==2 &&
       ((is.logical(as.is) && !as.is) || 
        (is.numeric(as.is) && length(unique(dsi)) < as.is*length(dsi))))
      dsi <- factor(dsi, exclude="") #exclude added 5Mar93
    
    ## For data frames, char. var usually factors
    if(label[i]!=" ")
      label(dsi) <- label[i]  #atr$label <- label[i]
    
    if(length(smiss$name)) {
      j <- smiss$name==name[i]
      if(any(j)) {
        atr$special.miss <- 
          list(codes=smiss$code[j],obs=smiss$obs[j])
        attr(dsi,'class') <- c("special.miss",attr(dsi,'class'))
      }
    }

    if(!is.null(atr))
      attributes(dsi) <- c(attributes(dsi),atr)

    if(missing(pos))
      ds[[i]] <- dsi
    else
      assign(name[i], dsi, pos=pos)				
  }

  if(!missing(pos))
    return(structure(pos, class="pos"))

  atr <- list()
  
  if(missing(id)) {
    if(data.frame.out)
      atr$row.names <- as.character(1:nrow)
  } else {
    idname <- id 
    jj <- match(idname, names(ds), 0)
    if(any(jj==0))
      stop(paste("id variable(s) not in dataset:",
                 paste(idname[jj==0],collapse=" ")))
    
    if(length(idname)==1) {
      id <- ds[[idname]] #Need since not use data.frame
    } else {		 
      id <- as.character(ds[[idname[1]]])
      for(jj in 2:length(idname))
        id <- paste(id, as.character(ds[[idname[jj]]]))
    }

    if(check.unique.id) {
      dup <- duplicated(id)
      if(any(dup))
        warning(paste("duplicate IDs:",
                      paste(id[dup], collapse=" ")))
    }

    if(data.frame.out)
      atr$row.names <- as.character(id)
    else atr$id <- id	
  }

  if(var.case=="lower"){
    names(ds)=tolower(names(ds))
  }
  if(var.case=="upper"){
    names(ds)=toupper(names(ds))
  }

  if(!is.null(FORMATS))
    atr$formats <- FORMATS

  if(data.frame.out)
    atr$class <- "data.frame"

  attributes(ds) <- c(attributes(ds),atr)
  ds
}

importConvertDateTime <- 
  function(x, type=c('date','time','datetime'),
           input=c('sas','spss','dataload'), form) {
  type <- match.arg(type)
  input <- match.arg(input)

  if(input != 'sas' && type != 'date')
    stop('only date variables are support for spss, dataload')
		
  adjdays <- c(sas=3653, spss=141428, dataload=135080)[input]
  ## 1970-1-1 minus 1960-1-1, 1582-10-14, or 1600-3-1
  origin <- c(sas='1960-01-01', spss='1582-10-14', dataload='1600-03-01')[input]
  if(input=='spss') x <- x/86400
  
  switch(type,
         date = structure(x - adjdays, class='Date'),
         time = {
           ## Don MacQueen 3Apr02
           z <- structure(x, class=c('POSIXt','POSIXct'))
           f <- format(z, tz='GMT')
           z <- as.POSIXct(format(z, tz='GMT'), tz='')
           structure(z, class=c('timePOSIXt','POSIXt','POSIXct'))},
         datetime = as.POSIXct(x, origin=origin, tz='GMT'))
#           chron((x - adjdays*86400)/86400,
#                 out.format=c(dates='day mon year', times='h:m:s'))})
}


## Don MacQueen 3Apr02
## slightly modified copy of format.POSIXct() from R base
format.timePOSIXt <- function (x, format = "%H:%M:%S", tz = "",
                               usetz = FALSE, ...) {
  if (!inherits(x, c("timePOSIXt","POSIXct"))) stop("wrong class")
  class(x) <- class(x)[-1]
  structure(format.POSIXlt(as.POSIXlt(x, tz), format, usetz, ...),
            names = names(x))
}

print.timePOSIXt <- function(x, ...) print(format(x, ...))


##if(!.R.) {
## Output format routine needed by chron for usual SAS date format
ddmmmyy <- function(x)
{
  if (!requireNamespace("chron", quietly = TRUE))
    stop("This function requires the 'chron' package.")
  y <- chron::month.day.year(trunc(unclass(x)), attr(x,"origin"))
  yr <- y$year
  m <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct",
         "Nov","Dec")[y$month]
  ifelse(yr<1900 | yr>=2000, paste(y$day,m,yr,sep=""),
         paste(y$day,m,yr-1900,sep=""))
}


## Functions to handle special.miss class
is.special.miss <- function(x, code)
{
  sm <- attr(x, "special.miss")
  if(!length(sm))
    return(rep(FALSE, length(x)))
  
  if(missing(code)) {
    z <- rep(FALSE, length(x))
    z[sm$obs] <- TRUE
  } else {
    z <- rep(FALSE, length(x))
    z[sm$obs[sm$codes==code]] <- TRUE
  }
  
  z
}


"[.special.miss" <- function(x, ..., drop=FALSE)
{
  ats <- attributes(x)
  ats$dimnames <- NULL
  ats$dim <- NULL
  ats$names <- NULL
  attr(x,'class') <- NULL
  y <- x[..., drop = drop]
  if(length(y) == 0)
    return(y)
  
  k <- seq(along=x)
  names(k) <- names(x)
  k <- k[...]
  attributes(y) <- c(attributes(y), ats)
  smiss <- attr(y, "special.miss")
  codes <- rep("ZZ",length(x))
  codes[smiss$obs] <- smiss$codes
  codes <- codes[...]
  which <- codes!="ZZ"
  if(sum(which)) attr(y,"special.miss") <- 
    list(obs=seq(along=k)[codes!="ZZ"],codes=codes[codes!="ZZ"])
  else {
    attr(y,"special.miss") <- NULL
    attr(y,'class') <- attr(y,'class')[attr(y,'class') != "special.miss"]
    if(length(attr(y,'class'))==0)
      attr(y,'class') <- NULL
  }
  
  y
}


format.special.miss <- function(x, ...)
{
  w <-
    if(is.factor(x))
      as.character(x)
    else {
      cl <- attr(x,'class');
      cl <- cl[cl!="special.miss"]
      if(length(cl)) {
        attr(x,'class') <- cl;
        format(x, ...)
      } else format.default(x, ...)
    }
  
  sm <- attr(x, "special.miss")
  names(w) <- names(x)
  if(!length(sm))
    return(w)
  
  w[sm$obs] <- sm$codes
  attr(w,"label") <- attr(w,"special.miss") <- attr(w,"class") <- NULL
  w
}


print.special.miss <- function(x, ...)
{
  sm <- attr(x, "special.miss")
  if(!length(sm)) {
    print.default(x)
    return(invisible())
  }
  
  w <- format.special.miss(x)
  print.default(w, quote=FALSE)
  invisible()
}


sas.codes <- function(object) attr(object, "sas.codes")


code.levels <- function(object) {
  if(length(cod <- attr(object,"sas.codes"))) 
    levels(object) <- paste(cod,":",levels(object),sep="")
  
  object
}


as.data.frame.special.miss <- function(x, row.names = NULL, optional = FALSE, ...)
{
  nrows <- length(x)
  if(is.null(row.names)) {
    ## the next line is not needed for the 1993 version of data.class and is
    ## included for compatibility with 1992 version
    if(length(row.names <- names(x)) == nrows &&
       !anyDuplicated(row.names)) {
    }
    else if(optional)
      row.names <- character(nrows)
    else row.names <- as.character(1:nrows)
  }

  value <- list(x)
  if(!optional)
    names(value) <- deparse(substitute(x))[[1]]

  structure(value, row.names=row.names, class='data.frame')
}


## val{nval}=compress(value)||"" was =value  23mar04
sas.get.macro <-
  c("/* Macro sas_get (modified by F. Harrell 30Jan90, Bill Dunlap Dec90, FH Mar92,",
    "\t\t\tFH Apr95 (extend LENGTH smiss))", 
    "    Sets up for conversion of SAS dataset to S dataset.", 
    "    Arguments:", "\tdataset - name of SAS dataset", 
    "\ttemp1\t- Name of temporary dataset to contain data dictionar (unquoted)",
    "\t\t  default=/tmp/file.1", 
    "\ttemp2\t- Name of temporary dataset to contain ASCII version of SAS", 
    "\t\t  dataset (unquoted)", "\t\t  default=/tmp/file.2", 
    "\ttemp3   - Name of temporary dataset to contain ASCII file with S", 
    "\t\t  program to store format values and labels", 
    "\ttemp4   - Name of temporary dataset to contain ASCII file with", 
    "\t\t  locations of special missing values", 
    "\tdates\t- SAS to store date variables in SAS format ( # days from 1/1/60)",
    "\t\t  (default)", 
    "\t\t- YEARFRAC to store as days from 1/1/1900, divided by 365.25", 
    "\t\t- YEARFRAC2 to store as year + fraction of current year", 
    "\t\t- YYMMDD to store as numeric YYMMDD", 
    "\tvars    - list of variable in dataset that you want returned to S",
    "                  (unquoted, separate variable names with spaces)  If empty,",
    "                  then return all variables.", 
    "        ifs     - sequence of SAS subsetting if statements, (unquoted,",
    "                  separated by semicolons).", 
    "\tformats - 0 (default) - do not create file on temp3 containing S", 
    "\t\t  statements to store format values and labels, 1 do create", 
    "\tspecmiss- 0 (default).  Set to 1 to write a data file on temp4 with",
    "\t\t  the fields: variable name, special missing value code,", 
    "\t\t  observation number", 
    "\tdefencod - default encoding of dataset if it does not specify",
    "                                                                              */",
    "%macro sas_get(dataset,  temp1, temp2, temp3, temp4, dates=SAS, vars=, ifs=, ",
    "\tformats=0, specmiss=0, defencod=&DEFAULTE);", 
    "OPTIONS NOFMTERR;",
    "%LET DSID=%SYSFUNC(open(&dataset,i));",
    "%LET ENCODE=%SCAN(%SYSFUNC(ATTRC(&DSID,ENCODING)),1);",
    "%IF &ENCODE=Default %THEN %LET dataset=&dataset(encoding=&defencod);",
    "%IF %QUOTE(&temp1)=  %THEN %LET temp1=/tmp/file.1;", 
    "%IF %QUOTE(&temp2)=  %THEN %LET temp2=/tmp/file.2;", 
    "%IF %QUOTE(&temp3)=  %THEN %LET temp3=/tmp/file.3;", 
    "%IF %QUOTE(&temp4)=  %THEN %LET temp4=/tmp/file.4;",
    ## Next line had %QUOTE(&ifs),1,\"'\"  31oct02
    "%LET dates=%UPCASE(&dates);", "%LET ifs=%SCAN(%QUOTE(&ifs),1,'');", 
    "%LET _s_=_sav_;", 
    "/* BILL: Can these 2 subsets be combined into one pass of the data? -Frank*/",
    "/* Subset by observation first */", "%IF %QUOTE(&ifs)^= %THEN %DO;", 
    " data _osub_ ;", "  set &dataset ;", "  &ifs ;", 
    " %LET dataset=_osub_ ;", " %END;", "/* Then subset by variable */", 
    "%IF &vars^= %THEN %DO;", " data _vsub_ ;", "  set &dataset ;", 
    "  keep &vars ;", " %LET dataset=_vsub_ ;", " %END;", 
    "proc contents data=&dataset out=&_s_(KEEP=name type length label format nobs ",
    " varnum) noprint; ", "%IF &formats=1 %THEN %DO;", 
    "   PROC FORMAT LIBRARY=LIBRARY CNTLOUT=f(KEEP=fmtname type start end label);",
    "   DATA f; SET f; RETAIN n 0; n+1; IF type=\"C\" THEN fmtname=\"$\"||fmtname;",
    "   PROC SORT DATA=f OUT=f(DROP=n); BY fmtname n; ", 
    "  *Sort by n instead of start for numerics so 13 sorts after 2;", 
    "  *Dont consider formats containing ANY range of values;", 
    "  *Dont consider formats that dont have at least one non-missing (if", 
    "   numeric) starting value.  This gets rid of formats that are used", 
    "   only to label special missing values;", 
    "   DATA f2; SET f; BY fmtname; RETAIN anyrange 0 anynmiss 0;", 
    "      IF FIRST.fmtname THEN DO;anyrange=0;anynmiss=0;END;", 
    "      IF start^=end THEN anyrange=1;", 
    "      IF TYPE=\"C\" THEN anynmiss=1; ", 
    "      ELSE IF (start+0)>. THEN anynmiss=1;", 
    "      IF LAST.fmtname & anynmiss & ^anyrange THEN OUTPUT; KEEP fmtname;",
    "   DATA f; MERGE f f2(IN=in2); BY fmtname; IF in2;", 
    "      IF TYPE=\"N\" THEN DO; IF (start+0)>.;  *S cannot handle special missings;",
    "         END;", "      RENAME fmtname=format start=value; DROP end;", 
    "   PROC SORT DATA=&_s_(KEEP=format) OUT=sform; BY format;", 
    "   DATA sform; SET sform; BY format; IF LAST.format;", 
    "   DATA f; MERGE sform(IN=in1) f(IN=in2); BY format; ", 
    "      IF in1 & in2;", 
    "   *This keeps formats ever used by any variable;", 
    "   DATA _NULL_; SET f END=_eof_; BY format;", 
    "      ARRAY val{*} $ 16 val1-val500; ARRAY lab{*} $ 40 lab1-lab500; ", 
    "      RETAIN done 0 nform 0 nval 0 val1-val500 \" \" lab1-lab500 \" \" bk -1; ",
    "      FILE \"&temp3\" LRECL=4096;", "      IF FIRST.format THEN DO;", 
    "         IF ^done THEN PUT 'list(' @@;  done=1;", 
    "         nform=nform+1; nval=0;", 
    "         format=TRANSLATE(format,\".abcdefghijklmnopqrstuvwxyz\",", 
    "                                 \"_ABCDEFGHIJKLMNOPQRSTUVWXYZ\");", 
    "          IF nform=1 THEN PUT '\"' format +bk '\"=list(' @@;", 
    "         ELSE PUT ', \"' format +bk '\"=list(' @@;", "         END;", 
    "      nval=nval+1; ", 
    "      IF nval>500 THEN DO; ERROR \">500 format values not allowed\";ABORT ABEND;",
    "         END;", '      val{nval}=compress(value)||""; lab{nval}=label; ', 
    "      IF LAST.format THEN DO;", "         PUT \"values=c(\" @@; ", 
    "         DO i=1 TO nval; IF i>1 THEN PUT \",\" @@;", 
    "            IF type=\"N\" THEN PUT val{i} +bk @@;", 
    "            ELSE PUT '\"' val{i} +bk '\"'  @@;", "            END;", 
    "         PUT \"),labels=c(\" @@;", 
    "         DO i=1 TO nval; IF i>1 THEN PUT \",\" @@;", 
    "            PUT '\"' lab{i} +bk '\"' @@;", "            END;", 
    "         PUT \"))\";", "         END;", 
    "      IF _eof_ THEN PUT \")\";", "   %END;", 
    "PROC SORT DATA=&_s_;BY varnum;", "data _null_;", " set &_s_ end=eof;", 
    " FILE \"&temp1\";  RETAIN _bk_ -1;", " if _n_ = 1 then do;", 
    "%IF &specmiss=0 %THEN %LET ofile=_NULL_; ", 
    "%ELSE %LET ofile=smiss(KEEP=vname val obs);", 
    "  put \"data &ofile; set &dataset end=eof;\";", 
    "  put '  file \"&temp2\" RECFM=D LRECL=4096;';", 
    "  put \"  retain __delim 18 _bk_ -1 obs 0; LENGTH _xx_ $ 20 obs 5;obs+1; \";",
    "%IF &specmiss=1 %THEN %DO;", 
    "  put \"LENGTH vname $ 8 val $ 1;\"; %END;", "  end;", 
    " IF type=2 THEN DO;", "  PUT 'FORMAT ' name ';' @;", 
    "  PUT 'IF ' name '=\" \" THEN PUT __delim IB1. @;';", 
    "/* $char added F.H. 24Mar92, dropped  +_bk_ before __delim */", 
    "/* $CHAR. removed FEH 2Aug92, added null FORMAT above, added back +_bk_ */",
    "  PUT 'ELSE PUT ' name '+_bk_ __delim IB1. @;';", "  END;", 
    " ELSE DO; ", "  PUT 'IF ' name '<=.Z THEN _xx_=\"NA\";' @;", 
    "  PUT 'ELSE _xx_=LEFT(PUT(' @;", "  format=UPCASE(format);", 
    "  IF format=\"DATE\"|format=\"MMDDYY\"|format=\"YYMMDD\"|",
    "format=\"DDMMYY\"|format=\"YYQ\"|format=\"MONYY\"|format=\"JULIAN\" THEN DO;",
    "   %IF &dates=SAS %THEN", "    PUT name \",BEST18.)\";", 
    "   %ELSE %IF &dates=YYMMDD %THEN", "    PUT name \",YYMMDD6.)\";", 
    "   %ELSE %IF &dates=YEARFRAC %THEN", 
    "    PUT \"(\" name \"-MDY(1,1,1900))/365.25,7.3)\";", 
    "   %ELSE %IF &dates=YEARFRAC2 %THEN %DO;", 
    "    PUT \"YEAR(\" name \")-1900+(\" name \"-MDY(1,1,YEAR(\" name \")))/\" @;",
    "    PUT \"(MDY(12,31,YEAR(\" name \"))-MDY(1,1,YEAR(\" name \"))+1),7.3)\";",
    "    %END;", "   ;", "   END;\t", 
    "  ELSE DO;PUT name \",BEST18.)\" @;END;", 
    "  PUT ');  PUT _xx_ +_bk_ __delim IB1. @;';  *Added +_bk_ 2Aug92;", 
    "%IF &specmiss=1 %THEN %DO;", 
    "  put 'IF .A<=' name '<=.Z THEN DO;",
    "   vname=\"' name +_bk_ '\"; val=put(' name ',1.); OUTPUT; END;';",
    "  %END;", "  END;", "if eof then PUT 'PUT; RUN;';", "run;", 
    "%include \"&temp1\";", "data _null_; set &_s_;", 
    " retain __delim 18 _bk_ -1; ", " file \"&temp1\" LRECL=4096;", 
    " format=TRANSLATE(format,\".abcdefghijklmnopqrstuvwxyz\",", 
    "                         \"_ABCDEFGHIJKLMNOPQRSTUVWXYZ\");", 
    " put name +_bk_ __delim IB1. type +_bk_ __delim IB1. length +_bk_ __delim IB1.",
    "  format +_bk_ __delim IB1. label +_bk_ __delim IB1. nobs +_bk_ __delim IB1.;",
    "run;", "%IF &specmiss=1 %THEN %DO;", 
    " PROC SORT DATA=smiss OUT=smiss;BY vname val obs;", 
    " DATA _NULL_; SET smiss;FILE \"&temp4\" RECFM=D LRECL=30;", 
    " RETAIN _bk_ -1 __delim 18;", 
    " vname=TRANSLATE(vname,\".abcdefghijklmnopqrstuvwxyz\",", 
    "\t\t       \"_ABCDEFGHIJKLMNOPQRSTUVWXYZ\");", 
    " PUT vname +_bk_ __delim IB1. val +_bk_ __delim IB1. obs +_bk_ __delim IB1.;",
    " RUN;", " %END;", "%mend sas_get;")


sasxport.get <- function(file, lowernames=TRUE,force.single=TRUE,
                         method=c('read.xport','dataload','csv'),
                         formats=NULL, allow=NULL, out=NULL,
                         keep=NULL, drop=NULL, as.is=0.5, FUN=NULL) {
  method <- match.arg(method)
  if(length(out) && method!='csv')
    stop('out only applies to method="csv"')
  
  rootsoftware <- if(method=='dataload')'dataload'
  else 'sas'

  sasdateform <-
    toupper(c("date","mmddyy","yymmdd","ddmmyy","yyq","monyy",
              "julian","qtr","weekdate","weekdatx","weekday","month"))
  sastimeform     <- toupper(c("hhmm","hour","mmss","time"))
  sasdatetimeform <- toupper(c("datetime","tod"))

  ## Note: unlike read.spss, read.dta, SAS xport reading functions do not
  ## support URLs.  And thanks to Kurt Hornik for https
  if(grepl("^https?://", tolower(file))) {
    tf <- tempfile()
    download.file(file, tf, mode='wb', quiet=TRUE)
    file <- tf
  }

  dsinfo <-
    if(method == 'csv') lookupSASContents(file)
    else lookup.xport(file)

  whichds <-
    if(length(keep))
      keep
    else
      setdiff(names(dsinfo), c(drop,'_CONTENTS_','_contents_'))
  
  ds <- switch(method,
               read.xport= read.xport(file),
               dataload  = read.xportDataload(file, whichds),
               csv       = if(!length(out))
               readSAScsv(file, dsinfo, whichds))

  if(method=='read.xport' && (length(keep) | length(drop)))
    ds <- ds[whichds]

  ## PROC FORMAT CNTLOUT= dataset present?
  fds <- NULL
  if(!length(formats)) {
    fds <- sapply(dsinfo, function(x)
                  all(c('FMTNAME','START','END','MIN','MAX','FUZZ')
                      %in% x$name))
    fds <- names(fds)[fds]
    if(length(fds) > 1) {
      warning('transport file contains more than one PROC FORMAT CNTLOUT= dataset; using only the first')
      fds <- fds[1]
    }
  }
  
  finfo <- NULL
  if(length(formats) || length(fds)) {
    finfo <-
      if(length(formats))
        formats
      else if(length(out))
        readSAScsv(file, dsinfo, fds)
      else ds[[fds]]

    ## Remove leading $ from char format names
    ##  fmtname <- sub('^\\$','',as.character(finfo$FMTNAME))
    fmtname <- as.character(finfo$FMTNAME)
    finfo <- split(finfo[c('START','END','LABEL')], fmtname)
    finfo <- lapply(finfo,
                    function(f)
                    {
                      rb <- function(a)
                        {  # remove leading + trailing blanks
                          a <- sub('[[:space:]]+$', '', as.character(a))
                          sub('^[[:space:]]+', '', a)
                        }

                      st <- rb(f$START)
                      en <- rb(f$END)
                      lab <- rb(f$LABEL)
                      ##j <- is.na(st) | is.na(en)
                      ##  st %in% c('','.','NA') | en %in% c('','.','NA')
                      j <- is.na(st) | is.na(en) | st == '' | en == ''
                      if(any(j)) {
                        warning('NA in code in FORMAT definition; removed')
                        st <- st[!j]; en <- en[!j]; lab <- lab[!j]
                      }

                      if(!all(st==en))
                        return(NULL)

                      list(value = all.is.numeric(st, 'vector'),
                           label = lab)
                    })
  }

  ## Number of non-format datasets
  nods <- length(whichds)
  nds  <- nods - (length(formats) == 0 && length(finfo) > 0)
  which.regular <- setdiff(whichds, fds)
  dsn <- tolower(which.regular)
  
  if((nds > 1) && !length(out)) {
    res <- vector('list', nds)
    names(res) <- gsub('_','.',dsn)
  }

  if(length(FUN)) {
    funout <- vector('list', length(dsn))
    names(funout) <- gsub('_','.',dsn)
  }
  possiblyConvertChar <- if(method=='read.xport')
    (is.logical(as.is) && as.is)  ||
      (is.numeric(as.is) && as.is < 1) else
  (is.logical(as.is) && !as.is) ||
    (is.numeric(as.is) && as.is > 0)
  ## reverse logic because read.xport always converts characters to factors
  j <- 0
  for(k in which.regular) {
    j   <- j + 1
    cat('Processing SAS dataset', k, '\t ')
    w   <-
      if(length(out))
        readSAScsv(file, dsinfo, k)
      else if(nods==1)
        ds
      else ds[[k]]

    cat('.')
    if(!length(w)) {
      cat('Empty dataset', k, 'ignored\n')
      next
    }

    chcase <- if(lowernames) tolower else function(x) x
    nam      <- chcase(makeNames(names(w), allow=allow))
    names(w) <- nam
    dinfo    <- dsinfo[[k]]
    fmt      <- sub('^\\$','',dinfo$format)
    lab      <- dinfo$label
    ndinfo   <- chcase(makeNames(dinfo$name, allow=allow))
    names(lab) <- names(fmt) <- ndinfo
    for(i in 1:length(w)) {
      changed <- FALSE
      x  <- w[[i]]
      fi <- fmt[nam[i]]; names(fi) <- NULL
      if(fi != '' && length(finfo) && (fi %in% names(finfo))) {
        f <- finfo[[fi]]
        if(length(f)) {  ## may be NULL because had a range in format
          x <- factor(x, f$value, f$label)
          attr(x, 'format') <- fi
          changed <- TRUE
        }
      }
      if(is.numeric(x)) {
        if(fi %in% sasdateform) {
          x <- importConvertDateTime(x, 'date', rootsoftware)
          changed <- TRUE
        } else if(fi %in% sastimeform) {
          x <- importConvertDateTime(x, 'time', rootsoftware)
          changed <- TRUE
        } else if(fi %in% sasdatetimeform) {
          x <- importConvertDateTime(x, 'datetime', rootsoftware)
          changed <- TRUE
        } else if(force.single) {
          if(all(is.na(x))) {
            storage.mode(x) <- 'integer'
            changed <- TRUE
          } else if(max(abs(x),na.rm=TRUE) <= (2^31-1) &&
                    all(floor(x) == x, na.rm=TRUE)) {
            storage.mode(x) <- 'integer'
            changed <- TRUE
          }
        }
      } else if(method=='read.xport' && possiblyConvertChar && is.factor(x)) {
        if((is.logical(as.is) && as.is) ||
           (is.numeric(as.is) && length(unique(x)) >= as.is*length(x))) {
          x <- as.character(x)
          changed <- TRUE
        }
      } else if(possiblyConvertChar && is.character(x)) {
        if((is.logical(as.is) && !as.is) || 
           (is.numeric(as.is) && length(unique(x)) < as.is*length(x))) {
          x <- factor(x, exclude='')
          changed <- TRUE
        }
      }

      lz <- lab[nam[i]]
      if(lz != '') {
        names(lz) <- NULL
        label(x)  <- lz
        changed   <- TRUE
      }
      
      if(changed)
        w[[i]] <- x
    }

    cat('.\n')
    if(length(out)) {
      nam <- gsub('_','.',dsn[j])
      assign(nam, w)
      ## ugly, but a way to get actual data frame name into first
      ## argument of save( )
      eval(parse(text=paste('save(',nam,', file="',
                   paste(out, '/', nam,'.rda',sep=''),
                   '", compress=TRUE)',sep='')))
      if(length(FUN) && length(w))
        funout[[nam]] <- FUN(w)

      remove(nam)
    } else if(nds > 1)
      res[[j]] <- w
  }

  if(length(out)) {
    names(dsinfo) <- gsub('_','.',tolower(names(dsinfo)))
    if(length(FUN))
      attr(dsinfo, 'FUN') <- funout

    invisible(dsinfo)
  } else if(nds > 1)
    res
  else w
}

## Use dataload program to create a structure like read.xport does
read.xportDataload <- function(file, dsnames) {
  outf <- substring(tempfile(tmpdir=''),2)
  file.copy(file, paste(tempdir(),outf,sep='/'))
  curwd <- getwd()
  on.exit(setwd(curwd))
  setwd(tempdir())
  n <- length(dsnames)
  w <- vector('list', n); names(w) <- dsnames
  for(a in dsnames) {
    status <- system(paste('dataload', outf, 'zzzz.rda', a),
                     intern=FALSE)
    if(status==0) {
      load('zzzz.rda')
      names(zzzz) <- makeNames(names(zzzz))
      w[[a]] <- zzzz
    }
  }

  w
}

utils::globalVariables(c("NOBS", "memname", "memlabel"))
## Read _contents_.csv and store it like lookup.xport output
lookupSASContents <- function(sasdir) {
  w <- read.csv(paste(sasdir,'_contents_.csv',sep='/'), as.is=TRUE)
  z <- tapply(w$NOBS, w$MEMNAME, function(x)x[1])
  if(any(z == 0)) {
    cat('\nDatasets with 0 observations ignored:\n')
    print(names(z)[z == 0], quote=FALSE)
    w <- subset(w, NOBS > 0)
  }

  w$TYPE <- ifelse(w$TYPE==1, 'numeric', 'character')
  names(w) <- tolower(names(w))
  unclass(split(subset(w,select=-c(memname,memlabel)), w$memname))
}

## Read all SAS csv export files and store in a list
readSAScsv <- function(sasdir, dsinfo, dsnames=names(dsinfo)) {
  sasnobs <- sapply(dsinfo, function(x)x$nobs[1])
  multi <- length(dsnames) > 1
  if(multi) {
    w <- vector('list', length(dsnames))
    names(w) <- dsnames
  }

  for(a in dsnames) {
    z <- read.csv(paste(sasdir,'/',a,'.csv', sep=''),
                  as.is=TRUE, blank.lines.skip=FALSE,
                  comment.char="")

    importedLength <- length(z[[1]])
    if(importedLength != sasnobs[a])
      cat('\nError: NOBS reported by SAS (',sasnobs[a],') for dataset ',
          a,' is not the same as imported length (', importedLength,
          ')\n', sep='')

    if(multi)
      w[[a]] <- z
  }

  if(multi)
    w
  else z
}

sasdsLabels <- function(file)
{
  w <- scan(file, sep='\n', what='', quiet=TRUE)
  i <- grep('Data Set Name:', w)
  if(!length(i))
    return(NULL)
  
  n <- tolower(sub('.*\\.([A-Z0-9\\_]*)[[:space:]]+.*','\\1',w[i]))
  w <- gsub('\t','',w)
  labs <- ifelse(nchar(w[i-1])==0,w[i-2],w[i-1])
  names(labs) <- n
  labs
}
