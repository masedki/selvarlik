bic_j <- function(j,x,S, both=TRUE)
{
  if(length(S)==1)
  {
    if(BIC(lm(x[,j]~1.)) < BIC(lm(x[,j]~as.vector(x[,S]))))
    {return(BIC(lm(x[,j]~1.)))}
    else
    {return(BIC(lm(x[,j]~as.vector(x[,S]))))}
  }
  else
  {
    min.model = lm(x[,j] ~ 1., data=x[,S])
    biggest <- formula(lm(x[,j]~. ,data=x[,S]))
    if(both)
      reg.model = step(min.model, direction='both', k = log(nrow(x)), scope=biggest, trace=FALSE)
    else
      reg.model = step(min.model, direction='forward', k = log(nrow(x)), scope=biggest, trace=FALSE)
    return(BIC(reg.model))
  }
}
