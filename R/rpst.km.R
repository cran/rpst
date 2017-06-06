rpst.km <- function(object, newdata, ...){
  
  data = predict.rpst(object, newdata)
  
  subdata = data[data$tr == 1,]
  # table(subdata$class)
  # require(survival)
  plot(survfit(Surv(y, status) ~ class,data=subdata),mark.time=F,lty = 1:2)
  # legend(0.5, 0.3, c("non-sensitive", "sensitive"), lty = 1:2) 
  title("Kaplan-Meier Curves for two subgroups with treatment")
  lines((survfit(Surv(y, status) ~ 1,data=subdata)),conf.int=F,lty = 4,col = 2)
  
  subdata = data[data$tr == 0,]
  # table(subdata$class)
  # require(survival)
  plot(survfit(Surv(y, status) ~ class,data=subdata),mark.time=F,lty = 1:2)
  # legend(13, 0.5, c("non-sensitive", "sensitive"), lty = 1:2) 
  title("Kaplan-Meier Curves for two subgroups without treatment")
  lines((survfit(Surv(y, status) ~ 1,data=subdata)),conf.int=F,lty = 4,col = 2)
}