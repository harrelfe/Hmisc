## For every object in a data frame that has a 'label' attribute, make it
## class 'labelled'

data.frame.labelled <- function(object)
{
  for(n in names(object))
    if(length(attr(object[[n]],'label')))
      attr(object[[n]],'class') <- c('labelled',attr(object[[n]],'class'))

  object
}
