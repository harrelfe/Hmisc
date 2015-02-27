# From Sandy Muspratt: http://stackoverflow.com/questions/28652284/how-to-change-color-of-facet-borders-when-using-facet-grid/

colorFacet <- function(g, col=adjustcolor('blue', alpha.f=0.3)) {

## Get the plot grob
gt <- ggplotGrob(g)

## Check the layout
##gtable_show_layout(gt)   # Vertical gaps are in columns 5 and 7
                         # and span rows 3 to 6
                         # Horizontal gap is in row 5
                         # and spans columns 4 to 9


## To automate the selection of the relevant rows and columns:
## Find out which items in the layout correspond to the panels.
## "r" and "b" (below) refer to the right and bottom indices for the panels in the layout
## The gaps' indices are one to the right of the panels' r index (except the right most panel)
## and one below the panels' b index (except the bottom most panel)
## Rmin and Rmax give the span of the horizontal gap;
## Bmin and Bmax give the span of the vertical gap
panelsR <- unique(gt$layout$r[grepl("panel", gt$layout$name)])
Rmin = 4
Rmax = panelsR[length(panelsR)] + 1
panelsR = panelsR[-length(panelsR)] +1

panelsB <- unique(gt$layout$b[grepl("panel", gt$layout$name)])
Bmin = 3
Bmax = panelsB[length(panelsB)]
panelsB = panelsB[-length(panelsB)] + 1

## Add colored rectangles into the vertical and horizontal gaps
for(i in panelsR) 
   gt <- gtable::gtable_add_grob(gt, 
            list(grid::rectGrob(gp = grid::gpar(col = NA, fill = col))), 
                                 Bmin, i, Bmax, i)

for(j in panelsB) 
   gt <- gtable::gtable_add_grob(gt, 
            list(grid::rectGrob(gp = grid::gpar(col = NA, fill = col))), 
                                 j, Rmin, j, Rmax)
grid::grid.draw(gt)
invisible()
}
