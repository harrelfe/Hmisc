#' Hiding Table of Contents for Rmd Files
#' 
#' \code{hidingTOC} creates a table of contents in a Rmd Document that
#' can be hidden at the press of a button.
#'
#' @param buttonLabel The text on the button that hides and unhides the
#'   table of contents
#' @param side which side of the page should the table of contents be placed
#'   on. Can be either \code{'right'} or \code{'left'}. Defaults to \code{'right'}
#' @param buttonSide which side of the page should the button be placed on.
#'   Can be either \code{'right'} or \code{'left'}. Defaults to \code{'right'}
#' @param hidden Logical should the table of contents be hidden at page load
#'   Defaults to \code{FALSE}
#'
#' @return a HTML formated text string to be inserted into an markdown document
#' 
#' @examples
#' createHidingTOC()

hidingTOC <- function(buttonLabel="Table of Contents",
                      side=c('right','left'), buttonSide=c('right','left'),
                      hidden=FALSE) {
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
</style>
<script>
function toggleTOC(){
  $("#TOC").toggle();
}

function collapseLevel1() {
$("#TOC").toggle(true);
$(".tocify-subheader").toggle(false)
}

function collapseLevel2() {
$("#TOC,.tocify-subheader[data-tag=2]").toggle(true);
$(".tocify-subheader[data-tag=3]").toggle(false)
}

function collapseLevel3() {
$("#TOC,.tocify-subheader").toggle(true);
}
</script>
<div id="toc-toggle" class="pull-right"><button id="toc-toggle" type="button" class="btn btn-default btn-xs code-folding-btn collapsed" onclick="toggleTOC()">%s</button><br/><center><button id="toc-toggle-level1" type="button" class="btn btn-default btn-xs code-folding-btn collapsed" onclick="collapseLevel1()">1</button><button id="toc-toggle-level2" type="button" class="btn btn-default btn-xs code-folding-btn collapsed" onclick="collapseLevel2()">2</button><button id="toc-toggle-level3" type="button" class="btn btn-default btn-xs code-folding-btn collapsed" onclick="collapseLevel3()">3</button></center></div>
'
  buttonSide <- match.arg(buttonSide)
  side <- match.arg(side)
  return(sprintf(skeleton, side, if(hidden) "\ndisplay:none" else "", buttonSide, buttonLabel))
}
