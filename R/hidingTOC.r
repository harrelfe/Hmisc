#' Moving and Hiding Table of Contents
#' 
#' Moving and hiding table of contents for Rmd HTML documents
#' 
#' \code{hidingTOC} creates a table of contents in a Rmd document that
#' can be hidden at the press of a button. It also generate buttons that allow
#' the hiding or unhiding of the diffrent level depths of the table of contents.
#'
#' @param buttonLabel The text on the button that hides and unhides the
#'   table of contents
#' @param levels The max depth of the table of contents that it is desired to
#'   have control over the display of.  (defaults to 3)
#' @param side which side of the page should the table of contents be placed
#'   on. Can be either \code{'right'} or \code{'left'}. Defaults to \code{'right'}
#' @param buttonSide which side of the page should the button be placed on.
#'   Can be either \code{'right'} or \code{'left'}. Defaults to \code{'right'}
#' @param hidden Logical should the table of contents be hidden at page load
#'   Defaults to \code{FALSE}
#'
#' @return a HTML formated text string to be inserted into an markdown document
#' @author Thomas Dupont
#' @examples
#' \dontrun{
#' hidingTOC()
#' }
#' @export

hidingTOC <- function(buttonLabel="Table of Contents", levels=3,
                      side=c('right','left'), buttonSide=c('right','left'),
                      hidden=FALSE) {

    ## Make javascript functions that controll the hiding and unhiding of
    ## different levels of the TOC
    ## This is done by switching on elements with id equal to TOC or class equal
    ## tocify-subheader and with attribute data-tag values less then or equal to
    ## the requested level and by switching off elements with attribute data-tag
    ## values greater then the requested level.
    makeLevelExpandFun <- function(level, maxLevels) {
        ## Sanity check to make sure that level is never greater then maxLevels
        if (level > maxLevels)
            stop("level value ", level, " is greater then maxLevels value ", maxLevels)

        ## There are 2 special cases.
        return(if (level == 1L) {
                   ## Where the reqested level equals 1. Unhide the element with id
                   ## equal to TOC and hide the elements with class equal to
                   ## tocify-subheader.
                   'function expandLevel1(){$("#TOC").toggle(true);$(".tocify-subheader").toggle(false)}'
               } else if (level == maxLevels) {
                   ## Where the requested level is equal to maxLevels then just unhide
                   ## all elements with id equal to TOC or class equal to
                   ## tocify-subheader.
                   paste0('function expandLevel', level, '(){$("TOC,.tocify-subheader").toggle(true)}')
               } else {
                   ## General case level greater then 1 and less then maxLevels. Unhide
                   ## the elements with id equal to TOC or class equal to
                   ## tocify-subheader with attribute data-tag values less then or
                   ## equal to the requested level. Then hide elements with class
                   ## tocify-subheader with attribute data-tag values greater then the
                   ## requested level but less then or equal to maxLevels.
                   paste0("function expandLevel", level, '(){$("#TOC,',
                          paste0('.tocify-subheader[data-tag=',seq.int(2L, level),']', collapse=','),
                          '").toggle(true);$("',
                          paste0('.tocify-subheader[data-tag=',seq.int(level+1L, maxLevels),']', collapse=','),
                          '").toggle(false)}')
               })
    }

    ## basic HTML skeleton to inwhich to place various values
    skeleton <- '<style type="text/css">
  #TOC{
    position:fixed;
    top:0;
    %s:0;
    margin: 54px 20px 20px 20px;
    z-index: 9;%s
  }
  #toc-toggle{
    position:fixed;
    top:0;
    %s:0;
    margin: 5px 20px 5px 20px;
  }
  .col-md-3{
    width: 0%%;
  }
  .col-md-9{
    width: 100%%;
  }
  div.container-fluid.main-container{
    max-width:none;
    margin-left:0px;
    margin-right:none;
  }
</style><script>function toggleTOC(){$("#TOC").toggle()}%s</script><div id="toc-toggle" class="pull-right"><button type="button" class="btn btn-default btn-xs toc-folding-btn pull-right" onclick="toggleTOC()">%s</button><br/>%s</div>
'
    levelSequence <- seq_len(levels)

    ## Generate the javascript text needed for the TOC level display buttons'
    ## functions.
    scriptText <- paste0(vapply(levelSequence, makeLevelExpandFun, "", maxLevels=levels),
                         collapse="")

    ## Generate the button HTML text.
    buttonText <- paste0("<center>", paste0('<button id="toc-expand-level', levelSequence,
                                            '" type="button" class="btn btn-default btn-xs toc-folding-btn" onclick="expandLevel',
                                            levelSequence, '()">', levelSequence, '</button>',
                                            collapse=""),
                         "</center>")

    buttonSide <- match.arg(buttonSide)
    side <- match.arg(side)
    return(sprintf(skeleton, side, if(hidden) "\ndisplay:none" else "", buttonSide, scriptText, buttonLabel, buttonText))
}
