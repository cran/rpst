get.region = function(node,nodeinfo,tempnode){
  if (!is.matrix(tempnode)) tempnode <- as.matrix(t(tempnode)) #tree with root node only
  maxnodenumber <- tempnode[nrow(tempnode), 1]                            
  maxlayer <- tempnode[nrow(tempnode), 3]                                        
  nodeplot <- matrix(rep(0, nrow(tempnode)*5), nrow=nrow(tempnode))
  colnames(nodeplot) <- c("Node number", "Node layer", "Upper node number", "Left daughter node number", "Right daughter node number")
  for (i in 1:nrow(tempnode)) {
    nodeplot[i, c(1, 2)] <- tempnode[i, c(1, 3)]
    for (k in 1:nrow(tempnode)) {
      if (tempnode[k, 1] == node[tempnode[i, 1], 4]) { nodeplot[i, 3] <- k}
      if (tempnode[k, 1] == node[tempnode[i, 1], 6]) { nodeplot[i, 4] <- k}
      if (tempnode[k, 1] == node[tempnode[i, 1], 7]) { nodeplot[i, 5] <- k}
    }
  }
  if(maxlayer == 1)region = "ALL" else 
    end = which(nodeplot[,4]==0 &  nodeplot[,5]==0)
  region = sapply(end,function(i){
    j = i
    expr = character()
    while(nodeplot[j,3]!=0){
      temp = paste0("data$'",unlist(strsplit(nodeinfo[nodeplot[j,1],3],"X")),"'",nodeinfo[nodeplot[j,1],4])
      expr= paste(c(temp,expr),collapse = " & ")
      j = nodeplot[j,3]
    }
    return(expr)
  })
  return(region)
}

get.class = function(data,region){
  class = sapply(1:length(region),function(k){
    expr = paste0("which(",region[k],")")
    id = eval(parse(text = expr))
    
    subdata  = data[id,]
    fit = summary(survfit(Surv(y, status) ~ tr,data=subdata))$table
    avertime0 = fit[1,"*rmean"] 
    avertime1 = fit[2,"*rmean"]
    # text = paste0(round(avertime0,2)," : ",round(avertime1,2))
    # nodeinfo[nodeplot[end[k],1],2] = text
    c = ifelse(avertime0 >= avertime1, 0,1)
    return(c)
  })
  return(class)
}


predict.rpst=function(object, newdata,...)
{
  region = get.region(object$node, object$nodeinfo, object$existingnode)
  class = get.class(newdata[-1,],region)
  data = newdata[-1,]
  data$class = 0
  for(k in 1:length(region)){
    expr = paste0("which(",region[k],")")
    id = eval(parse(text = expr))
    data[id,"class"] = class[k]
  }
  return(data)
}

