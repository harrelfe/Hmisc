list.tree <- function(struct,depth=-1, numbers=FALSE, maxlen=22,
                      maxcomp=12, attr.print=TRUE, front="",
                      fill=". ", name.of, size=TRUE)
{ 
  if(depth==0)
    return()
  
  opts <- options('digits')
  options(digits=5)
  on.exit(options(opts))
  if (missing(name.of))
    name.of <- deparse(substitute(struct))
  
  len <- length(struct)
  cat(front,name.of,"=",storage.mode(struct),len)
  if(size)
    cat(" (",object.size(struct)," bytes)",sep="")
  
  if(is.array(struct))
    cat("=",
        if(length(dimnames(struct)))
          "named", 
        "array",paste(dim(struct),collapse=" X "))
  
  if(is.ts(struct)) cat("= time series",tsp(struct)) 
  if(is.factor(struct)) 
    cat("= factor (",length(levels(struct))," levels)",sep="")
  
  if(length(attr(struct,'class'))>0)
    cat("(",attr(struct,'class'),")")
  
  if(is.atomic(struct) && !is.character(struct)&& len>0 && maxlen>0) {
    field <- "="
    for(i in 1:length(struct)) {
      field <- paste(field,format(as.vector(struct[i])))
      if(nchar(field)>maxlen-6) {
        field <- paste(field,"...");
        break
      }
    }
    
    cat(field,"\n",sep="")
  } else if(is.character(struct) && len>0 && maxlen>0) 
    cat("=",substring(struct[1:(last <- max(1,(1:len)
                                            [cumsum(nchar(struct)+1)<maxlen]))],1,maxlen),
        if(last<len)
          " ...","\n")
        else cat("\n")
  
  if (mode(struct)=="list" && len>0) {
    structnames <- names(struct)
    if(!length(structnames))
      structnames <- rep("",len)
    
    noname <- structnames==""
    structnames[noname] <- 
      paste("[[",(1:length(structnames))[noname],"]]",sep="")
    for (i in 1:min(length(structnames),maxcomp)) 
      if (mode(struct[[i]])=="argument" | mode(struct[[i]])=="unknown") 
        cat(front,fill," ",structnames[i]," = ",
            as.character(struct[[i]])[1],"\n",sep="")
      else 
        list.tree(struct[[i]],depth=depth-1,numbers,maxlen,maxcomp,
                  attr.print,
                  if(numbers)
                    paste(front,i,sep=".")
                  else paste(front,fill,sep=""),
                  
                  fill,structnames[i],size=FALSE)

    if(length(structnames)>maxcomp) 
      cat(front,fill," ...   and ",length(structnames)-maxcomp,
          " more\n",sep="")
  }
  
  attribs <- attributes(struct)
  attribnames <- names(attribs)
  if(length(attribnames)>0 && attr.print)
    for (i in (1:length(attribnames))
         [attribnames!="dim" & attribnames!="dimnames" & 
          attribnames!="levels" & attribnames!="class" &
          attribnames!="tsp" & 
          (attribnames!="names" | mode(struct)!="list")])
      list.tree(attribs[[i]],depth-1,numbers,maxlen,maxcomp,attr.print,
		if(numbers)
                  paste(front,i,sep="A")
                else paste(front,"A ",sep=""),
                
		fill,attribnames[i],size=FALSE)
  
  invisible()
}


##############################################################################
expr.tree <- function(struct,front="",fill=". ",name.of,numbers=FALSE,depth=-1,
                      show.comment=FALSE)
{ 
  if (missing(name.of))
    name.of <- deparse(substitute(struct))
  else if(is.atomic(struct) | is.name(struct))
    name.of <- paste(name.of,deparse(struct))
  
  cat(front,"",name.of,"=",mode(struct),length(struct),"\n")
  if(depth!=0 && is.recursive(struct) ) {
    structlength <- length(struct)
    structnames <- names(struct)
    if(length(structnames)==0)
      structnames <- rep("",structlength)
    if(structlength>0)
      for (i in 1:length(structnames)) {
        if((mode(struct[[i]])!="missing" || is.function(struct)) &&
           (mode(struct[[i]])!="comment" || show.comment))
          expr.tree(struct[[i]],
                    if(numbers)
                    paste(front,i,sep=".")
                    else paste(front,fill,sep=""),
                    
                    fill,structnames[i],numbers,"depth"=depth-1)
      }
  }
  
  invisible(character(0))
}
