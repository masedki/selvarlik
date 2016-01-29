bic_j <- function(j,x,S, both=TRUE)
{
  min.model = lm(x[,j] ~ 1., data= as.data.frame(x[,S]))
  biggest <- formula(lm(x[,j]~.,as.data.frame(x[,S])))
  if(both)
    reg.model = step(min.model, direction='both', k = log(nrow(x)), scope=biggest, trace=FALSE)
  else
    reg.model = step(min.model, direction='forward', k = log(nrow(x)), scope=biggest, trace=FALSE)  
  return(BIC(reg.model))
}
