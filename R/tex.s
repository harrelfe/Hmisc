tex  <-  function(string, lref='c', psref='c', scale=1, srt=0) 
  paste('\\tex[',lref,'][',psref,'][',
        format(scale),'][',format(srt),']{',string,'}',sep='')
