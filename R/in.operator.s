"%in%" <- function(a,b)				{

if(is.factor(a) & is.numeric(b))	{
   warning("a is factor, b is numeric.  Assuming b is coded factor values")
   a <- oldUnclass(a)			}

else if(is.numeric(a) && is.factor(b))	{
   warning("a is numeric, b is factor.  Assuming a is coded factor values")
   b <- oldUnclass(b)			}

match(a, b, nomatch=0) > 0
						}


"%nin%" <- function(a, b) ! (a %in% b)
