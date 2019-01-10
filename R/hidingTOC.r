#' Moving and Hiding Table of Contents
#' 
#' Moving and hiding table of contents for Rmd HTML documents
#' 
#' \code{hidingTOC} creates a table of contents in a Rmd document that
#' can be hidden at the press of a button. It also generate buttons that allow
#' the hiding or unhiding of the diffrent level depths of the table of contents.
#'
#' @param buttonLabel the text on the button that hides and unhides the
#'   table of contents. Defaults to \code{Contents}.
#' @param levels the max depth of the table of contents that it is desired to
#'   have control over the display of.  (defaults to 3)
#' @param posCollapse if \code{'margin'} then display the depth select buttons
#'   vertically along the side of the page choosen by \code{buttonSide}. If
#'   \code{'top'} then display the depth select buttons horizontally under the
#'   button that hides the TOC. Defaults to \code{'margin'}. \code{'bottom'} is
#'   currently unimplemented.
#' @param tocSide which side of the page should the table of contents be placed
#'   on. Can be either \code{'right'} or \code{'left'}. Defaults to
#'   \code{'right'}
#' @param buttonSide which side of the page should the button that hides the TOC
#'   be placed on. Can be either \code{'right'} or \code{'left'}. Defaults to
#'   \code{'right'}
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

hidingTOC <- function(buttonLabel="Contents", levels=3,
                      tocSide=c('right','left'), buttonSide=c('right','left'),
                      posCollapse=c('margin','top','bottom'), hidden=FALSE) {

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
                   paste0('function expandLevel', level, '(){$("#TOC,.tocify-subheader").toggle(true)}')
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
    skeleton <- '<style type="text/css">%s</style><script>function toggleTOC(){$("#TOC").toggle();$(".toc-level-select-group").toggle()}%s</script><div id="toc-controls">%s<br/>%s</div>
'
    buttonSide <- match.arg(buttonSide)
    tocSide <- match.arg(tocSide)
    posCollapse <- match.arg(posCollapse)
    if(posCollapse == 'bottom') {
        stop("arguement posCollapse = bottom is not supported yet")
    }

    if(tocSide != buttonSide)
        stop("non-symmetric values for tocSide and buttonSide are not supported")
    
    if(!missing(hidden) && (length(hidden) == 0L || (!is.logical(hidden) && !is.numeric(hidden))))
        stop("hidden must be of logical type")

    levelSequence <- seq_len(levels)

    ## CSS text
    cssText <- paste0(".toc-level-select-group{clear:both}#TOC{position:fixed;top:0;",
                      tocSide, ':0;margin:',
                      switch(posCollapse,
                             margin = '23px ',
                             top = '44px '),
                      switch(posCollapse,
                             margin = '20px ',
                             top = '0px '),
                      '20px ',
                      switch(posCollapse,
                             margin = '20px',
                             top = '0px'),
                      ';z-index:9',
                      if(hidden) ";display:none",
                      '}#toc-controls{position:fixed;top:0;',
                      buttonSide, ':0;margin:0px}.col-md-3{width: 0%}.col-md-9{width: 100%}',
                      'div.container-fluid.main-container{max-width:none;margin-left:0px;margin-right:none}')



    ## Generate the javascript text needed for the TOC level display buttons'
    ## functions.
    scriptText <- paste0(vapply(levelSequence, makeLevelExpandFun, "", maxLevels=levels),
                         collapse="")

    ## Which side the buttons should be pulled to.
    pullClass <- if(buttonSide == "right") {
                     "pull-right"
                 } else {
                     "pull-left"
                 }

    ## Generate the hiding button HTML text.
    buttonText <- paste0('<button type="button" id="toc-toggle" class="btn btn-default btn-xs toc-folding-btn ',
                         pullClass, '" onclick="toggleTOC()">', buttonLabel, '</button>')
    
    ## Generate the level buttons' HTML text.
    levelButtonText <- paste0('<div class="toc-level-select-group',
                              switch(posCollapse,
                                     margin = paste0(" ", pullClass),
                                     ""),
                              '">',
                              paste0('<button id="toc-expand-level', levelSequence,
                                     '" type="button" class="btn btn-default btn-xs toc-collapsing-btn',
                                     switch(posCollapse,
                                            margin = paste0(" ", pullClass),
                                            ""),
                                     '" onclick="expandLevel',
                                     levelSequence, '()">', levelSequence, '</button>',
                                     collapse=switch(posCollapse,
                                                     margin = "<br/>",
                                                     top="",
                                                     stop("Unknown value for posCollapse ", posCollapse))),
                              "</div>")

    return(sprintf(skeleton, cssText, scriptText, buttonText, levelButtonText))
}
