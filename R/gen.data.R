gen.data = function(n=100, p=10, c = 4, beta = NULL)
{
  x <- matrix(rnorm(n*p),n, p)
  if(is.null(beta)) beta = c(runif(1,1.5,2.5),rep(0,p-1))
  if(length(beta)!= p) stop("length of beta is wrong!")
  lamda = exp(x%*%beta)
  s = runif(n)
  t = (-log(s)/lamda)^(1/p)
  q = quantile(t)
  z = ifelse(t>=q[4] | (t>=q[2] & t< q[3]),1,0)
  trem = ifelse(t>=q[3],1,0)
  
  c = runif(n,0,c)
  y = sapply(1:n,function(i){min(t[i],c[i])})
  status = ifelse(t>y,0,1)
  
  data = as.data.frame(cbind(as.numeric(trem),as.numeric(y),as.numeric(status),x))
  colnames(data) = c("tr","y","status",paste0("V",1:p))
  surdata = data
  surdata[1,] = c(1,2,2,rep(3,p))
  surdata[2:(n+1),] = data
  
  return(list(data = surdata,beta = beta))
  
}
