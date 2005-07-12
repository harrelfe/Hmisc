if(!.R.) mem <- function()
{
  cat("Memory used:  Current=",memory.size(),
      " Maximum=",memory.size(TRUE),"\n")
  invisible()
}
