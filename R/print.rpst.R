print.rpst=function(x, ...)
{
  tempnode = x$node
  tempinfo = x$nodeinfo
  cat("node), split,  n,  c(n1, n2),  c(avertime0,avertime1)",  "\n", sep=" ")
  cat("+ denotes a nominal covariate\n", sep=" ")
  cat("1 ) root", tempinfo[1,c(1:2)],  "\n", sep=" ")    
  tempinfo <- tempinfo[tempinfo[, 3]!="-1",]
  tempnode <- tempnode[tempnode[,2]!=0, ]
  
  if (is.matrix(tempnode))	{
    for ( i in 2:nrow(tempnode))
    {
      if (tempinfo[i, 5]!="0")  {
        cat(i, ") +", tempinfo[i, c(1:2)], "\n", sep=" ")
      } else {       
        cat(i, ") ", tempinfo[i, c(1:2)], "\n", sep=" ")
      }
    }
  }				
}

