plot.rpst <- function(x, xlength = 1, ylength = 3, xshift = 0.5 , ysegment = 6, ...)
  
  {
  node = x$node
  nodeinfo = x$nodeinfo
  tempnode = x$existingnode
  
  if (!is.matrix(tempnode)) tempnode <- as.matrix(t(tempnode)) #tree with root node only
  maxnodenumber <- tempnode[nrow(tempnode), 1]                            
  maxlayer <- tempnode[nrow(tempnode), 3]                                        
  nodeplot <- matrix(rep(0, nrow(tempnode)*9), nrow=nrow(tempnode))
  colnames(nodeplot) <- c("Node number", "Node layer", "Upper node number in plot", "Left daughter node number in plot", "Right daughter node number in plot", "X axis", "Y axis", "Shape", "X axis relocated")
  for (i in 1:nrow(tempnode)) {
    nodeplot[i, c(1, 2)] <- tempnode[i, c(1, 3)]
    for (k in 1:nrow(tempnode)) {
      if (tempnode[k, 1] == node[tempnode[i, 1], 4]) { nodeplot[i, 3] <- k}
      if (tempnode[k, 1] == node[tempnode[i, 1], 6]) { nodeplot[i, 4] <- k}
      if (tempnode[k, 1] == node[tempnode[i, 1], 7]) { nodeplot[i, 5] <- k}
    }
    temp <- tempnode[i, 1] - 2^(tempnode[i, 3] - 1)
    nodeplot[i, 6] <- 2^(maxlayer - tempnode[i, 3]) + temp*2^(maxlayer - tempnode[i, 3] + 1) + 3*xlength
    nodeplot[i, 7] <- (maxlayer - tempnode[i, 3])*ysegment
    if (nodeplot[i, 4]==0 && nodeplot[i, 5]==0) {nodeplot[i, 8] <- 1}  # 0: ellipse; 1: rectangle
  }
  for (i in 1:nrow(nodeplot)) {
    nodeplot[order(nodeplot[,6])[i], 9] <- i + 3*xlength
  }
  plot(c(0, nrow(nodeplot) + 6*xlength), c(0, maxlayer*ysegment), type = "n", xlab="", ylab="", axes = FALSE)
  for (i in 1:nrow(nodeplot)) {
    if (nodeplot[i, 2] > 1) {
      segments(nodeplot[nodeplot[i, 3], 9], nodeplot[nodeplot[i, 3], 7], nodeplot[i, 9], nodeplot[i, 7])
      if (nodeplot[i, 1]%%2 == 0) {
        text( ((nodeplot[nodeplot[i, 3], 9] + nodeplot[i, 9])/2 - xshift), (nodeplot[i, 7] + ysegment/6*5), nodeinfo[nodeplot[i, 1], 3], cex=0.5)
        text( ((nodeplot[nodeplot[i, 3], 9] + nodeplot[i, 9])/2 - xshift), (nodeplot[i, 7] + ysegment/6*4), nodeinfo[nodeplot[i, 1], 4], cex=0.5)
      }
    }
    if (nodeplot[i, 8]==1) {
      rect(nodeplot[i, 9] - xlength, nodeplot[i, 7], nodeplot[i, 9] + xlength, nodeplot[i, 7] + ylength, col="white")
    } else {
      draw.ellipse(nodeplot[i, 9], nodeplot[i, 7] + ylength/2, xlength, ylength/2, col="white")
    }
    text(nodeplot[i, 9], (nodeplot[i, 7] + ysegment/6*2), nodeinfo[nodeplot[i, 1], 1], cex=0.5)
    text(nodeplot[i, 9], (nodeplot[i, 7] + ysegment/6*1), nodeinfo[nodeplot[i, 1], 2], cex=0.5)
  }
}

